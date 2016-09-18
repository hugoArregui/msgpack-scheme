;;;;; msgpack-imple.scm - MessagePack scheme implementation
;;
;;  Copyright (c) 2013, Hugo Arregui
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. The name of the authors may not be used to endorse or promote products
;;     derived from this software without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(use srfi-69 byte-blob numbers srfi-1 matchable)
(include "flonum-utils.scm")

;; limits
(define fixed_uint_limit 127)
(define uint8_limit (- (expt 2 8) 1))
(define uint16_limit (- (expt 2 16) 1))
(define uint32_limit (- (expt 2 32) 1))
(define uint64_limit (- (expt 2 64) 1))
(define fixed_int_limit -32)
(define int8_limit (- (- (expt 2 7) 1)))
(define int16_limit (- (- (expt 2 15) 1)))
(define int32_limit (- (- (expt 2 31) 1)))
(define int64_limit (- (- (expt 2 63) 1)))
(define fixed_raw_limit 31) ;in bytes
(define raw8_limit (- (expt 2 8) 1))
(define raw16_limit (- (expt 2 16) 1))
(define raw32_limit (- (expt 2 32) 1))
(define fixed_array_limit 15) ;in bytes
(define array16_limit (- (expt 2 16) 1))
(define array32_limit (- (expt 2 32) 1))
(define fixed_map_limit 15) ;in bytes
(define map16_limit (- (expt 2 16) 1))
(define map32_limit (- (expt 2 32) 1))

;; constants
(define constants '((()      . #xc0)
                    (#t      . #xc3)
                    (#f      . #xc2)
                    (uint8   . #xcc)
                    (uint16  . #xcd)
                    (uint32  . #xce)
                    (uint64  . #xcf)
                    (int8    . #xd0)
                    (int16   . #xd1)
                    (int32   . #xd2)
                    (int64   . #xd3)
                    (str8    . #xd9)
                    (str16   . #xda)
                    (str32   . #xdb)
                    (bin8    . #xc4)
                    (bin16   . #xc5)
                    (bin32   . #xc6)
                    (float   . #xca)
                    (double  . #xcb)
                    (array16 . #xdc)
                    (array32 . #xdd)
                    (map16   . #xde)
                    (map32   . #xdf)))

(define constant-repr-map (alist->hash-table constants))

(define repr-constant-map
  (alist->hash-table (map (lambda (entry)
                            (cons (cdr entry) (car entry))) constants)))

(define (read-byte/eof-error port)
  (let ((b (read-byte port)))
    (if (eof-object? b)
      (error "premature eof")
      b)))

;; byte manipulation primitives
(define (byte-complement2 n)
  (add1 (- 255 n)))

(define (get-smallest-byte n)
  (bitwise-and #xff n))

(define (write-bytes port value size)
  (if (> size 0)
    (let* ((shift_size (- size 1))
           (byte (get-smallest-byte (arithmetic-shift value (- (* 8 shift_size))))))
      (write-byte byte port)
      (write-bytes port value shift_size))))

(define (read-bytes port size f)
  (apply bitwise-ior
         (let loop ((shift_size (- (* size 8) 8)))
           (if (>= shift_size 0)
             (cons (arithmetic-shift
                     (f (read-byte/eof-error port)) shift_size)
                   (loop (- shift_size 8)))
             '()))))

(define write-uint write-bytes)

(define (read-uint port size #!optional (mapper identity))
  (mapper (read-bytes port size identity)))

(define (write-raw port blob size)
  (assert (byte-blob? blob))
  (let loop ((index 0))
    (if (< index size)
      (let ((byte (byte-blob-uref blob index)))
        (write-byte byte port)
        (loop (+ index 1))))))

(define (read-raw port size #!optional (mapper identity))
  (let loop ((size size)
             (data (byte-blob-empty)))
    (if (> size 0)
      (let ((byte (read-byte/eof-error port)))
        (loop (- size 1) (byte-blob-cons byte data)))
      (mapper (byte-blob-reverse data)))))

(define (write-array port value size)
  (assert (= (vector-length value) size))
  (let loop ((index 0))
    (when (< index size)
      (pack port (vector-ref value index))
      (loop (+ index 1)))))

(define (write-map port value size)
  (assert (hash-table? value))
  (hash-table-walk value (lambda (k v)
                           (pack port k)
                           (pack port v))))

(define (write-header port type)
  (let ((header (hash-table-ref constant-repr-map type)))
    (write-byte header port)))

(define (fixed-uint? value)
  (= (bitwise-and #x80 value) 0))

(define (fixed-sint? value)
  (= (bitwise-and #xe0 value) #xe0))

(define (fixed-str? value)
  (= (bitwise-and #xe0 value) #xa0))

(define (fixed-array? value)
  (= (bitwise-and #xf0 value) #x90))

(define (fixed-map? value)
  (= (bitwise-and #xf0 value) #x80))
;;

(define (out-of-limit-error type value)
  (error type "Out of limit" value))

(define Uint
  (let* ((lowrite
           (lambda (port value header size)
             (write-header port header)
             (write-uint port value size)))
         (read-uint
           read-uint)
         (pack
           (lambda (port value)
             (cond ((<= value fixed_uint_limit)
                    (let ((header (bitwise-and value #x7f)))
                      (write-byte header port)))
                   ((<= value uint8_limit)  (lowrite port value 'uint8  1))
                   ((<= value uint16_limit) (lowrite port value 'uint16 2))
                   ((<= value uint32_limit) (lowrite port value 'uint32 4))
                   ((<= value uint64_limit) (lowrite port value 'uint64 8))
                   (#t                      (out-of-limit-error 'uint value))))))
    (match-lambda
      ('read read-uint)
      ('pack pack))))

(define Sint
  (let* ((write-sint
           write-bytes)
         (lowrite
           (lambda (port value header size)
             (write-header port header)
             (write-sint port value size)))
         (read-sint
           (lambda (port size #!optional (mapper identity))
             (mapper (- (add1 (read-bytes port size (cut - 255 <>)))))))
         (pack
           (lambda (port value)
             (cond ((>= value fixed_int_limit)
                    (let ((header (bitwise-ior #xe0 (get-smallest-byte value))))
                      (write-byte header port)))
                   ((>= value int8_limit)  (lowrite port value 'int8  1))
                   ((>= value int16_limit) (lowrite port value 'int16 2))
                   ((>= value int32_limit) (lowrite port value 'int32 4))
                   ((>= value int64_limit) (lowrite port value 'int64 8))
                   (#t                     (out-of-limit-error 'sint value))))))
    (match-lambda
      ('read read-sint)
      ('pack pack))))

(define Float
  (let* ((write-float
           (lambda (port value)
             (write-uint port (float->uint32 value) 4)))
         (read-float
           (lambda (port #!optional (mapper identity))
             (mapper (byte-blob->float (byte-blob-reverse (read-raw port 4))))))
         (pack
           (lambda (port value)
             (write-header port 'float)
             (write-float port value))))
    (match-lambda
      ('read read-float)
      ('pack pack))))

(define Double
  (let* ((write-double
           (lambda (port value)
             (write-raw port (double->byte-blob value) 8)))
         (read-double
           (lambda (port #!optional (mapper identity))
             (mapper (byte-blob->double (byte-blob-reverse (read-raw port 8))))))
         (pack
           (lambda (port value)
             (write-header port 'double)
             (write-double port value))))
    (match-lambda
      ('read read-double)
      ('pack pack))))

(define Array
  (let* ((lowrite
           (lambda (port value header header-size size)
             (write-header port header)
             (write-uint port size header-size)
             (write-array port value size)))
         (read-array
           (lambda (port size #!optional (mapper identity))
             (define array (make-vector size))
             (let loop ((index 0))
               (when (< index size)
                 (vector-set! array index (unpack port mapper))
                 (loop (+ index 1))))
             (mapper array)))
         (pack
           (lambda (port value)
             (let ((size (vector-length value)))
               (cond ((<= size fixed_array_limit)
                      (let ((header (bitwise-ior #x90 size)))
                        (write-byte header port)
                        (write-array port value size)))
                     ((<= size array16_limit) (lowrite port value 'array16 2 size))
                     ((<= size array32_limit) (lowrite port value 'array32 4 size))
                     (#t                      (out-of-limit-error 'array value)))))))
    (match-lambda
      ('read read-array)
      ('pack pack))))

(define Map
  (let* ((lowrite
           (lambda (port value header header-size size)
             (write-header port header)
             (write-uint port size header-size)
             (write-map port value size)))
         (read-map
           (lambda (port size #!optional (mapper identity))
             (define table (make-hash-table #:size size))
             (let loop ((size size))
               (if (> size 0)
                 (let* ((k (unpack port mapper))
                        (v (unpack port mapper)))
                   (hash-table-set! table k v)
                   (loop (- size 1)))))
             (mapper table)))
         (pack
           (lambda (port value)
             (let ((size (hash-table-size value)))
               (cond ((<= size fixed_map_limit)
                      (let ((header (bitwise-ior #x80 size)))
                        (write-byte header port)
                        (write-map port value size)))
                     ((<= size map16_limit) (lowrite port value 'map16 2 size))
                     ((<= size map32_limit) (lowrite port value 'map32 4 size))
                     (#t                    (out-of-limit-error 'map value)))))))
    (match-lambda
      ('read read-map)
      ('pack pack))))

(define Str
  (let* ((lowrite
           (lambda (port value header header-size size)
             (write-header port header)
             (write-uint port size header-size)
             (write-raw port value size)))
         (read-str
          (lambda (port size #!optional (mapper identity))
            (mapper (read-raw port size byte-blob->string))))
        (pack
          (lambda (port value)
            (assert (string? value))
            (let* ((blob (string->byte-blob value))
                   (size (byte-blob-length blob)))
              (cond ((<= size fixed_raw_limit)
                     (let ((header (bitwise-ior #xa0 size)))
                       (write-byte header port)
                       (write-raw port blob size)))
                    ((<= size raw8_limit)  (lowrite port blob 'str8  1 size))
                    ((<= size raw16_limit) (lowrite port blob 'str16 2 size))
                    ((<= size raw32_limit) (lowrite port blob 'str32 4 size))
                    (#t                    (out-of-limit-error 'str value)))))))
    (match-lambda
      ('read read-str)
      ('pack pack))))

(define Bin
  (let* ((lowrite
           (lambda (port value header header-size size)
             (write-header port header)
             (write-uint port size header-size)
             (write-raw port value size)))
         (read-bin
           read-raw)
         (pack
           (lambda (port value)
             (assert (byte-blob? value))
             (let ((size (byte-blob-length value)))
               (cond
                 ((<= size raw8_limit)  (lowrite port value 'bin8  1 size))
                 ((<= size raw16_limit) (lowrite port value 'bin16 2 size))
                 ((<= size raw32_limit) (lowrite port value 'bin32 4 size))
                 (#t                    (out-of-limit-error 'bin value)))))))
    (match-lambda
      ('read read-bin)
      ('pack pack))))

(define pack-uint   (Uint   'pack))
(define pack-sint   (Sint   'pack))
(define pack-float  (Float  'pack))
(define pack-double (Double 'pack))
(define pack-array  (Array  'pack))
(define pack-map    (Map    'pack))
(define pack-bin    (Bin    'pack))
(define pack-str    (Str    'pack))

(define (pack port value)
  (let ((repr/or-false (hash-table-ref/default constant-repr-map value #f)))
    (cond (repr/or-false
            (write-byte repr/or-false port))
          ((char? value)
           (pack-uint port (char->integer value)))
          ((integer? value)
           ((if (>= value 0) pack-uint pack-sint) port value))
          ((flonum? value)
           (pack-double port value))
          ((blob? value)
           (pack-bin port (blob->byte-blob value)))
          ((byte-blob? value)
           (pack-bin port value))
          ((string? value)
           (pack-str port value))
          ((list? value)
           (pack-array port (list->vector value)))
          ((vector? value)
           (pack-array port value))
          ((hash-table? value)
           (pack-map port value))
          (#t
           (error (format "I don't know how to handle: ~A" value))))))

(define (unpack port #!optional (mapper identity))
  (let ((value (read-byte port)))
    (if (eof-object? value)
      value
      (cond ((hash-table-exists? repr-constant-map value)
             (let ((constant (hash-table-ref repr-constant-map value)))
               (match constant
                      ('uint8   ((Uint   'read) port 1 mapper))
                      ('uint16  ((Uint   'read) port 2 mapper))
                      ('uint32  ((Uint   'read) port 4 mapper))
                      ('uint64  ((Uint   'read) port 8 mapper))
                      ('int8    ((Sint   'read) port 1 mapper))
                      ('int16   ((Sint   'read) port 2 mapper))
                      ('int32   ((Sint   'read) port 4 mapper))
                      ('int64   ((Sint   'read) port 8 mapper))
                      ('str8    ((Str    'read) port (read-uint port 1) mapper))
                      ('str16   ((Str    'read) port (read-uint port 2) mapper))
                      ('str32   ((Str    'read) port (read-uint port 4) mapper))
                      ('bin8    ((Bin    'read) port (read-uint port 1) mapper))
                      ('bin16   ((Bin    'read) port (read-uint port 2) mapper))
                      ('bin32   ((Bin    'read) port (read-uint port 4) mapper))
                      ('float   ((Float  'read) port mapper))
                      ('double  ((Double 'read) port mapper))
                      ('array16 ((Array  'read) port (read-uint port 2) mapper))
                      ('array32 ((Array  'read) port (read-uint port 4) mapper))
                      ('map16   ((Map    'read) port (read-uint port 2) mapper))
                      ('map32   ((Map    'read) port (read-uint port 4) mapper))
                      (else     (mapper constant)))))
            ((fixed-uint? value)
             (mapper value))
            ((fixed-sint? value)
             (mapper (- (byte-complement2 value))))
            ((fixed-str? value)
             (let ((size (bitwise-and #x1f value)))
               ((Str 'read) port size mapper)))
            ((fixed-array? value)
             (let ((size (bitwise-and #x0f value)))
               ((Array 'read) port size mapper)))
            ((fixed-map? value)
             (let ((size (bitwise-and #x0f value)))
               ((Map 'read) port size mapper)))
            (#t
             (error 'unpack "cannot unpack" value))))))
