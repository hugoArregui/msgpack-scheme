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
(define fixed_uint_limit  127)
(define uint8_limit       (sub1 (expt 2 8)))
(define uint16_limit      (sub1 (expt 2 16)))
(define uint32_limit      (sub1 (expt 2 32)))
(define uint64_limit      (sub1 (expt 2 64)))
(define fixed_int_limit   -32)
(define int8_limit        (sub1 (- (expt 2 7))))
(define int16_limit       (sub1 (- (expt 2 15))))
(define int32_limit       (sub1 (- (expt 2 31))))
(define int64_limit       (sub1 (- (expt 2 63))))
(define int64_maxlimit    (sub1 (expt 2 63)))
(define fixed_raw_limit   31) ;in bytes
(define raw8_limit        (sub1 (expt 2 8)))
(define raw16_limit       (sub1 (expt 2 16)))
(define raw32_limit       (sub1 (expt 2 32)))
(define fixed_array_limit 15) ;in bytes
(define array16_limit     (sub1 (expt 2 16)))
(define array32_limit     (sub1 (expt 2 32)))
(define fixed_map_limit   15) ;in bytes
(define map16_limit       (sub1 (expt 2 16)))
(define map32_limit       (sub1 (expt 2 32)))

;; constants
(define constants '((()       . #xc0)
                    (#t       . #xc3)
                    (#f       . #xc2)
                    (uint8    . #xcc)
                    (uint16   . #xcd)
                    (uint32   . #xce)
                    (uint64   . #xcf)
                    (int8     . #xd0)
                    (int16    . #xd1)
                    (int32    . #xd2)
                    (int64    . #xd3)
                    (str8     . #xd9)
                    (str16    . #xda)
                    (str32    . #xdb)
                    (bin8     . #xc4)
                    (bin16    . #xc5)
                    (bin32    . #xc6)
                    (float    . #xca)
                    (double   . #xcb)
                    (array16  . #xdc)
                    (array32  . #xdd)
                    (map16    . #xde)
                    (map32    . #xdf)
                    (ext8     . #xc7)
                    (ext16    . #xc8)
                    (ext32    . #xc9)
                    (fixext1  . #xd4)
                    (fixext2  . #xd5)
                    (fixext4  . #xd6)
                    (fixext8  . #xd7)
                    (fixext16 . #xd8)))

(define-record extension type data)

(define make-extension
  (let ((old-make-extension make-extension))
    (lambda (type data)
      (if (or (not (integer? type))
              (< type 0)
              (> type 127))
        (error (format "invalid type ~A, it should be a number between 0 and 127" type)))
      (if (not (byte-blob? data))
        (error (format "invalid data ~A, it should be a byte-blob" data)))
      (old-make-extension type data))))

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

(define (read-bytes* port size)
  (map
    (lambda (e)
      (read-byte/eof-error port))
    (iota size)))

(define (bytes->number bytes f)
  (let* ((size (length bytes)))
    (apply bitwise-ior
           (map
             (lambda (byte shift_size)
               (arithmetic-shift (f byte) shift_size))
             bytes
             (iota size (* (sub1 size) 8) -8)))))

(define write-uint write-bytes)

(define (read-uint port size #!optional (mapper identity))
  (mapper (bytes->number (read-bytes* port size) identity)))

(define write-sint write-bytes)

(define (read-sint port size #!optional (mapper identity))
  (let ((bytes (read-bytes* port size)))
    (mapper
      (if (bit-set? (car bytes) 7) ; its negative
        (- (add1 (bytes->number bytes (cut - 255 <>))))
        (bytes->number bytes identity)))))

(define (write-raw port blob size)
  (assert (byte-blob? blob) "write-raw: expected byte-blob" blob)
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
  (assert (= (vector-length value) size) "write-array: invalid size" size)
  (let loop ((index 0))
    (when (< index size)
      (pack port (vector-ref value index))
      (loop (+ index 1)))))

(define (write-map port value size)
  (assert (hash-table? value) "write-map: expected hash-table" value)
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
             (if (or (not (integer? value))
                     (< value 0))
               (error 'badInput "cannot pack value as uint" value))
             (cond ((<= value fixed_uint_limit)
                    (let ((header (bitwise-and value #x7f)))
                      (write-byte header port)))
                   ((<= value uint8_limit)  (lowrite port value 'uint8  1))
                   ((<= value uint16_limit) (lowrite port value 'uint16 2))
                   ((<= value uint32_limit) (lowrite port value 'uint32 4))
                   ((<= value uint64_limit) (lowrite port value 'uint64 8))
                   (#t                      (out-of-limit-error 'uint value))))))
    (match-lambda*
      (('unpack 'uint8)  (cut read-uint <> 1 <>)) ; port mapper
      (('unpack 'uint16) (cut read-uint <> 2 <>))
      (('unpack 'uint32) (cut read-uint <> 4 <>))
      (('unpack 'uint64) (cut read-uint <> 8 <>))
      (('unpack 'fixed)  (lambda (port value mapper)
                           (mapper value)))
      (('pack)           pack))))

(define Sint
  (let* ((lowrite
           (lambda (port value header size)
             (write-header port header)
             (write-sint port value size)))
         (read-sint
           read-sint)
         (pack
           (lambda (port value)
             (if (not (integer? value))
               (error 'badInput "cannot pack value as sint" value))
             (cond ((> value int64_maxlimit)
                    (out-of-limit-error 'sint value))
                   ((> value 0)
                    ((Uint 'pack) port value))
                   ((>= value fixed_int_limit)
                    (let ((header (bitwise-ior #xe0 (get-smallest-byte value))))
                      (write-byte header port)))
                   ((>= value int8_limit)  (lowrite port value 'int8  1))
                   ((>= value int16_limit) (lowrite port value 'int16 2))
                   ((>= value int32_limit) (lowrite port value 'int32 4))
                   ((>= value int64_limit) (lowrite port value 'int64 8))
                   (#t                     (out-of-limit-error 'sint value))))))
    (match-lambda*
      (('unpack 'int8)  (cut read-sint <> 1 <>)) ; port mapper
      (('unpack 'int16) (cut read-sint <> 2 <>))
      (('unpack 'int32) (cut read-sint <> 4 <>))
      (('unpack 'int64) (cut read-sint <> 8 <>))
      (('unpack 'fixed)  (lambda (port value mapper)
                           (mapper (- (byte-complement2 value)))))
      (('pack)           pack))))

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
      ('unpack read-float)
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
      ('unpack read-double)
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
             (if (not (vector? value))
               (error 'badInput "cannot pack value as array" value))
             (let ((size (vector-length value)))
               (cond ((<= size fixed_array_limit)
                      (let ((header (bitwise-ior #x90 size)))
                        (write-byte header port)
                        (write-array port value size)))
                     ((<= size array16_limit) (lowrite port value 'array16 2 size))
                     ((<= size array32_limit) (lowrite port value 'array32 4 size))
                     (#t                      (out-of-limit-error 'array value)))))))
    (match-lambda*
      (('unpack 'array16) (lambda (port mapper) (read-array port (read-uint port 2) mapper)))
      (('unpack 'array32) (lambda (port mapper) (read-array port (read-uint port 4) mapper)))
      (('unpack 'fixed)   (lambda (port value mapper)
                            (let ((size (bitwise-and #x0f value)))
                              (read-array port size mapper))))
      (('pack)            pack))))

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
             (if (not (hash-table? value))
               (error 'badInput "cannot pack value as map" value))
             (let ((size (hash-table-size value)))
               (cond ((<= size fixed_map_limit)
                      (let ((header (bitwise-ior #x80 size)))
                        (write-byte header port)
                        (write-map port value size)))
                     ((<= size map16_limit) (lowrite port value 'map16 2 size))
                     ((<= size map32_limit) (lowrite port value 'map32 4 size))
                     (#t                    (out-of-limit-error 'map value)))))))
    (match-lambda*
      (('unpack 'map16) (lambda (port mapper) (read-map port (read-uint port 2) mapper)))
      (('unpack 'map32) (lambda (port mapper) (read-map port (read-uint port 4) mapper)))
      (('unpack 'fixed) (lambda (port value mapper)
                          (let ((size (bitwise-and #x0f value)))
                            (read-map port size mapper))))
      (('pack)           pack))))

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
             (if (not (string? value))
               (error 'badInput "cannot pack value as str" value))
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
    (match-lambda*
      (('unpack 'str8)  (lambda (port mapper) (read-str port (read-uint port 1) mapper)))
      (('unpack 'str16) (lambda (port mapper) (read-str port (read-uint port 2) mapper)))
      (('unpack 'str32) (lambda (port mapper) (read-str port (read-uint port 4) mapper)))
      (('unpack 'fixed) (lambda (port value mapper)
                          (let ((size (bitwise-and #x1f value)))
                            (read-str port size mapper))))
      (('pack)          pack))))

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
             (if (not (byte-blob? value))
               (error 'badInput "cannot pack value as bin" value))
             (let ((size (byte-blob-length value)))
               (cond
                 ((<= size raw8_limit)  (lowrite port value 'bin8  1 size))
                 ((<= size raw16_limit) (lowrite port value 'bin16 2 size))
                 ((<= size raw32_limit) (lowrite port value 'bin32 4 size))
                 (#t                    (out-of-limit-error 'bin value)))))))
    (match-lambda*
      (('unpack 'bin8)  (lambda (port mapper) (read-bin port (read-uint port 1) mapper)))
      (('unpack 'bin16) (lambda (port mapper) (read-bin port (read-uint port 2) mapper)))
      (('unpack 'bin32) (lambda (port mapper) (read-bin port (read-uint port 4) mapper)))
      (('pack)          pack))))

(define Ext
  (let* ((lowrite
           (lambda (port header type data header-size size)
             (write-header port header)
             (if header-size
               (write-uint port size header-size))
             (write-sint port type 1)
             (write-raw port data size)))
         (read-ext
           (lambda (port size mapper)
             (let* ((type (read-sint port 1))
                    (data (read-raw port size)))
               (mapper (make-extension type data)))))
         (pack (lambda (port value)
                 (if (not (extension? value))
                   (error 'badInput "cannot pack value as extension " value))
                 (let* ((type (extension-type value))
                        (data (extension-data value))
                        (size (byte-blob-length data)))
                   (cond
                     ((<= size 1)           (lowrite port 'fixext1  type data #f 1))
                     ((<= size 2)           (lowrite port 'fixext2  type data #f 2))
                     ((<= size 4)           (lowrite port 'fixext4  type data #f 4))
                     ((<= size 8)           (lowrite port 'fixext8  type data #f 8))
                     ((<= size 16)          (lowrite port 'fixext16 type data #f 16))
                     ((<= size raw8_limit)  (lowrite port 'ext8     type data 1  size))
                     ((<= size raw16_limit) (lowrite port 'ext16    type data 2  size))
                     ((<= size raw32_limit) (lowrite port 'ext32    type data 4  size))
                     (#t                    (out-of-limit-error 'ext value)))))))
    (match-lambda*
      (('unpack 'fixext1)  (lambda (port mapper) (read-ext port 1 mapper)))
      (('unpack 'fixext2)  (lambda (port mapper) (read-ext port 2 mapper)))
      (('unpack 'fixext4)  (lambda (port mapper) (read-ext port 4 mapper)))
      (('unpack 'fixext8)  (lambda (port mapper) (read-ext port 8 mapper)))
      (('unpack 'fixext16) (lambda (port mapper) (read-ext port 16 mapper)))
      (('unpack 'ext8)     (lambda (port mapper) (read-ext port (read-uint port 1) mapper)))
      (('unpack 'ext16)    (lambda (port mapper) (read-ext port (read-uint port 2) mapper)))
      (('unpack 'ext32)    (lambda (port mapper) (read-ext port (read-uint port 4) mapper)))
      (('pack)             pack))))

(define pack-uint   (Uint   'pack))
(define pack-sint   (Sint   'pack))
(define pack-float  (Float  'pack))
(define pack-double (Double 'pack))
(define pack-array  (Array  'pack))
(define pack-map    (Map    'pack))
(define pack-bin    (Bin    'pack))
(define pack-str    (Str    'pack))
(define pack-ext    (Ext    'pack))

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
          ((extension? value)
           (pack-ext port value))
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
                      ((or 'uint8 'uint16 'uint32 'uint64) ((Uint   'unpack constant) port mapper))
                      ((or 'int8 'int16 'int32 'int64)     ((Sint   'unpack constant) port mapper))
                      ((or 'str8 'str16 'str32)            ((Str    'unpack constant) port mapper))
                      ((or 'bin8 'bin16 'bin32)            ((Bin    'unpack constant) port mapper))
                      ((or 'array16 'array32)              ((Array  'unpack constant) port mapper))
                      ((or 'map16 'map32)                  ((Map    'unpack constant) port mapper))
                      ((or 'ext8 'ext16 'ext32
                           'fixext1 'fixext2 'fixext4
                           'fixext8 'fixext16)             ((Ext    'unpack constant) port mapper))
                      ('float                              ((Float  'unpack)          port mapper))
                      ('double                             ((Double 'unpack)          port mapper))
                      (else                                (mapper constant)))))
            ((fixed-uint? value)  ((Uint  'unpack 'fixed) port value mapper))
            ((fixed-sint? value)  ((Sint  'unpack 'fixed) port value mapper))
            ((fixed-str? value)   ((Str   'unpack 'fixed) port value mapper))
            ((fixed-array? value) ((Array 'unpack 'fixed) port value mapper))
            ((fixed-map? value)   ((Map   'unpack 'fixed) port value mapper))
            (#t                   (error 'unpack "cannot unpack" value))))))
