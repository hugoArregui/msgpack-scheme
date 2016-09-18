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
  (+ (- 255 n) 1))

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
;;

(define (out-of-limit-error type value)
  (error type "Out of limit" value))

(define (write-uint port value size)
  (write-bytes port value size))

(define (read-uint port size #!optional (mapper identity))
  (mapper (read-bytes port size (lambda (b) b))))

(define (write-sint port value size)
  (write-bytes port value size))

(define (read-sint port size #!optional (mapper identity))
  (mapper
    (- (+ (read-bytes port size (lambda (b) (- 255 b)))
          1))))

(define (write-float port value)
  (write-uint port (float->uint32 value) 4))

(define (read-float port #!optional (mapper identity))
  (mapper (byte-blob->float (byte-blob-reverse (read-raw port 4)))))

(define (write-double port value)
  (write-raw port (double->byte-blob value) 8))

(define (read-double port #!optional (mapper identity))
  (mapper (byte-blob->double (byte-blob-reverse (read-raw port 8)))))

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

(define (read-str port size #!optional (mapper identity))
  (mapper (read-raw port size byte-blob->string)))

(define read-bin read-raw)

(define (write-array port array size)
  (assert (= (vector-length array) size))
  (let loop ((index 0))
    (when (< index size)
      (pack port (vector-ref array index))
      (loop (+ index 1)))))

(define (read-array port size #!optional (mapper identity))
  (define array (make-vector size))
  (let loop ((index 0))
    (when (< index size)
      (vector-set! array index (unpack port mapper))
      (loop (+ index 1))))
  (mapper array))

(define (write-map port value size)
  (assert (hash-table? value))
  (hash-table-walk value (lambda (k v)
                           (pack port k)
                           (pack port v))))

(define (read-map port size #!optional (mapper identity))
  (define table (make-hash-table #:size size))
  (let loop ((size size))
    (if (> size 0)
      (let* ((k (unpack port mapper))
             (v (unpack port mapper)))
        (hash-table-set! table k v)
        (loop (- size 1)))))
  (mapper table))

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

(define (write-header port type)
  (let ((header (hash-table-ref constant-repr-map type)))
    (write-byte header port)))

(define (pack-uint port value)
  (cond ((<= value fixed_uint_limit)
         (let ((header (bitwise-and value #x7f)))
           (write-byte header port)))
        ((<= value uint8_limit)
         (write-header port 'uint8)
         (write-uint port value 1))
        ((<= value uint16_limit)
         (write-header port 'uint16)
         (write-uint port value 2))
        ((<= value uint32_limit)
         (write-header port 'uint32)
         (write-uint port value 4))
        ((<= value uint64_limit)
         (write-header port 'uint64)
         (write-uint port value 8))
        (#t
         (out-of-limit-error 'uint value))))

(define (pack-sint port value)
  (cond ((>= value fixed_int_limit)
         (let ((header (bitwise-ior #xe0 (get-smallest-byte value))))
           (write-byte header port)))
        ((>= value int8_limit)
         (write-header port 'int8)
         (write-sint port value 1))
        ((>= value int16_limit)
         (write-header port 'int16)
         (write-sint port value 2))
        ((>= value int32_limit)
         (write-header port 'int32)
         (write-sint port value 4))
        ((>= value int64_limit)
         (write-header port 'int64)
         (write-sint port value 8))
        (#t
         (out-of-limit-error 'sint value))))

(define (pack-float port value)
  (write-header port 'float)
  (write-float port value))

(define (pack-double port value)
  (write-header port 'double)
  (write-double port value))

(define (pack-str port value)
  (assert (string? value))
  (let* ((blob (string->byte-blob value))
         (size (byte-blob-length blob)))
    (cond ((<= size fixed_raw_limit)
           (let ((header (bitwise-ior #xa0 size)))
             (write-byte header port)
             (write-raw port blob size)))
          ((<= size raw8_limit)
           (write-header port 'str8)
           (write-uint port size 1)
           (write-raw port blob size))
          ((<= size raw16_limit)
           (write-header port 'str16)
           (write-uint port size 2)
           (write-raw port blob size))
          ((<= size raw32_limit)
           (write-header port 'str32)
           (write-uint port size 4)
           (write-raw port blob size))
          (#t
           (out-of-limit-error 'str value)))))

(define (pack-bin port value)
  (assert (byte-blob? value))
  (let ((size (byte-blob-length value)))
    (cond ((<= size raw8_limit)
           (write-header port 'bin8)
           (write-uint port size 1)
           (write-raw port value size))
          ((<= size raw16_limit)
           (write-header port 'bin16)
           (write-uint port size 2)
           (write-raw port value size))
          ((<= size raw32_limit)
           (write-header port 'bin32)
           (write-uint port size 4)
           (write-raw port value size))
          (#t
           (out-of-limit-error 'bin value)))))

(define (pack-array port value)
  (let ((size (vector-length value)))
    (cond ((<= size fixed_array_limit)
           (let ((header (bitwise-ior #x90 size)))
             (write-byte header port)
             (write-array port value size)))
          ((<= size array16_limit)
           (write-header port 'array16)
           (write-uint port size 2)
           (write-array port value size))
          ((<= size array32_limit)
           (write-header port 'array32)
           (write-uint port size 4)
           (write-array port value size))
          (#t
           (out-of-limit-error 'array value)))))

(define (pack-map port value)
  (let ((size (hash-table-size value)))
    (cond ((<= size fixed_map_limit)
           (let ((header (bitwise-ior #x80 size)))
             (write-byte header port)
             (write-map port value size)))
          ((<= size map16_limit)
           (write-header port 'map16)
           (write-uint port size 2)
           (write-map port value size))
          ((<= size map32_limit)
           (write-header port 'map32)
           (write-uint port size 4)
           (write-map port value size))
          (#t
           (out-of-limit-error 'map value)))))

(define (pack port value)
  (let ((repr/or-false (hash-table-ref/default constant-repr-map value #f)))
    (cond (repr/or-false
            (write-byte repr/or-false port))
          ((char? value)
           (pack-uint port (char->integer value)))
          ((integer? value)
           (if (>= value 0)
             (pack-uint port value)
             (pack-sint port value)))
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
                      ('uint8   (read-uint port 1 mapper))
                      ('uint16  (read-uint port 2 mapper))
                      ('uint32  (read-uint port 4 mapper))
                      ('uint64  (read-uint port 8 mapper))
                      ('int8    (read-sint port 1 mapper))
                      ('int16   (read-sint port 2 mapper))
                      ('int32   (read-sint port 4 mapper))
                      ('int64   (read-sint port 8 mapper))
                      ('str8    (read-str port (read-uint port 1) mapper))
                      ('str16   (read-str port (read-uint port 2) mapper))
                      ('str32   (read-str port (read-uint port 4) mapper))
                      ('bin8    (read-bin port (read-uint port 1) mapper))
                      ('bin16   (read-bin port (read-uint port 2) mapper))
                      ('bin32   (read-bin port (read-uint port 4) mapper))
                      ('float   (read-float port mapper))
                      ('double  (read-double port mapper))
                      ('array16 (read-array port (read-uint port 2) mapper))
                      ('array32 (read-array port (read-uint port 4) mapper))
                      ('map16   (read-map port (read-uint port 2) mapper))
                      ('map32   (read-map port (read-uint port 4) mapper))
                      (else     (mapper constant)))))
            ((fixed-uint? value)
             (mapper value))
            ((fixed-sint? value)
             (mapper (- (byte-complement2 value))))
            ((fixed-str? value)
             (let ((size (bitwise-and #x1f value)))
               (read-str port size mapper)))
            ((fixed-array? value)
             (let ((size (bitwise-and #x0f value)))
               (read-array port size mapper)))
            ((fixed-map? value)
             (let ((size (bitwise-and #x0f value)))
               (read-map port size mapper)))
            (#t
             (error 'unpack "cannot unpack" value))))))
