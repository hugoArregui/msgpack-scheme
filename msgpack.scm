;;;;; msgpack.scm - MessagePack scheme implementation
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

(use srfi-69 endian-blob byte-blob numbers)

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
                    (float   . #xca)
                    (double  . #xcb)
                    (raw16   . #xda)
                    (raw32   . #xdb)
                    (array16 . #xdc)
                    (array32 . #xdd)
                    (map16   . #xde)
                    (map32   . #xdf)))

(define (identity x)
  x)

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
  (let ((uint8_vector (endian-blob->u8vector (ieee_float32->endian-blob value))))
    (write-raw port (blob->byte-blob 
                 (u8vector->blob uint8_vector)) 4)))

(define (read-float port #!optional (mapper identity))
  (mapper (endian-blob->ieee_float32
            (byte-blob->endian-blob (read-raw port 4)))))

(define (write-double port value)
  (let ((uint8_vector (endian-blob->u8vector (ieee_float64->endian-blob value))))
    (write-raw port (blob->byte-blob 
                 (u8vector->blob uint8_vector)) 8)))

(define (read-double port #!optional (mapper identity)) 
  (mapper (endian-blob->ieee_float64 
            (byte-blob->endian-blob (read-raw port 8)))))

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
    (if (> size 0 )
      (let ((byte (read-byte/eof-error port)))
        (loop (- size 1) (byte-blob-cons byte data)))
      (mapper (byte-blob-reverse data)))))

(define (write-array port array size)
  (assert (= (vector-length array) size))
  (let loop ((index 0))
    (if (< index size)
      (let ()
        (pack port (vector-ref array index))
        (loop (+ index 1))))))

(define (read-array port size #!optional (mapper identity)) 
  (define array (make-vector size))
  (let loop ((index 0))
    (if (< index size)
      (let ()
        (vector-set! array index (unpack port mapper))
        (loop (+ index 1)))))
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

(define (fixed-raw? value)
  (= (bitwise-and #xe0 value) #xa0))

(define (fixed-array? value)
  (= (bitwise-and #xf0 value) #x90))

(define (fixed-map? value)
  (= (bitwise-and #xf0 value) #x80))

(define (pack-uint port value)
  (cond ((<= value fixed_uint_limit)
         (let ((header (bitwise-and value #x7f)))
           (write-byte header port)))
        ((<= value uint8_limit)
         (let ((header (hash-table-ref constant-repr-map 'uint8)))
           (write-byte header port)
           (write-uint port value 1)))
        ((<= value uint16_limit)
         (let ((header (hash-table-ref constant-repr-map 'uint16)))
           (write-byte header port)
           (write-uint port value 2)))
        ((<= value uint32_limit)
         (let ((header (hash-table-ref constant-repr-map 'uint32)))
           (write-byte header port)
           (write-uint port value 4)))
        ((<= value uint64_limit)
         (let ((header (hash-table-ref constant-repr-map 'uint64)))
           (write-byte header port)
           (write-uint port value 8)))
        (#t
         (out-of-limit-error 'uint value))))
        
(define (pack-sint port value)
  (cond ((>= value fixed_int_limit)
         (let ((header (bitwise-ior #xe0 (get-smallest-byte value))))
           (write-byte header port)))
        ((>= value int8_limit)
         (let ((header (hash-table-ref constant-repr-map 'int8)))
           (write-byte header port)
           (write-sint port value 1)))
        ((>= value int16_limit)
         (let ((header (hash-table-ref constant-repr-map 'int16)))
           (write-byte header port)
           (write-sint port value 2)))
        ((>= value int32_limit)
         (let ((header (hash-table-ref constant-repr-map 'int32)))
           (write-byte header port)
           (write-sint port value 4)))
        ((>= value int64_limit)
         (let ((header (hash-table-ref constant-repr-map 'int64)))
           (write-byte header port)
           (write-sint port value 8)))
        (#t
         (out-of-limit-error 'sint value))))

(define (pack-float port value)
  (let* ((header (hash-table-ref constant-repr-map 'float)))
    (write-byte header port)
    (write-float port value)))

(define (pack-double port value)
  (let* ((header (hash-table-ref constant-repr-map 'double)))
    (write-byte header port)
    (write-double port value)))

(define (pack-raw port value)
  (assert (byte-blob? value))
  (let ((size (byte-blob-length value)))
    (cond ((= size 0)
           #f) ;do nothing
          ((<= size fixed_raw_limit)
           (let ((header (bitwise-ior #xa0 size)))
             (write-byte header port)
             (write-raw port value size)))
          ((<= size raw16_limit)
           (let ((header (hash-table-ref constant-repr-map 'raw16)))
             (write-byte header port)
             (write-uint port size 2)
             (write-raw port value size)))
          ((<= size raw32_limit)
           (let ((header (hash-table-ref constant-repr-map 'raw32)))
             (write-byte header port)
             (write-uint port size 4)
             (write-raw port value size)))
          (#t
             (out-of-limit-error 'raw value)))))

(define (pack-array port value)
  (let ((size (vector-length value)))
    (cond ((<= size fixed_array_limit)
           (let ((header (bitwise-ior #x90 size)))
             (write-byte header port)
             (write-array port value size)))
          ((<= size array16_limit)
           (let ((header (hash-table-ref constant-repr-map 'array16)))
             (write-byte header port)
             (write-uint port size 2)
             (write-array port value size)))
          ((<= size array32_limit)
           (let ((header (hash-table-ref constant-repr-map 'array32)))
             (write-byte header port)
             (write-uint port size 4)
             (write-array port value size)))
          (#t
             (out-of-limit-error 'array value)))))

(define (pack-map port value)
  (let ((size (hash-table-size value)))
    (cond ((<= size fixed_map_limit)
           (let ((header (bitwise-ior #x80 size)))
             (write-byte header port)
             (write-map port value size)))
          ((<= size map16_limit)
           (let ((header (hash-table-ref constant-repr-map 'map16)))
             (write-byte header port)
             (write-uint port size 2)
             (write-map port value size)))
          ((<= size map32_limit)
           (let ((header (hash-table-ref constant-repr-map 'map32)))
             (write-byte header port)
             (write-uint port size 4)
             (write-map port value size)))
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
           (pack-raw port (blob->byte-blob value)))
          ((byte-blob? value)
           (pack-raw port value))
          ((string? value)
           (pack-raw port (string->byte-blob value)))
          ((list? value)
           (pack-array port (list->vector value)))
          ((vector? value)
           (pack-array port value))
          ((hash-table? value)
           (pack-map port value))
          (#t
           (error (format "I don't know how to handle: ~A" value))))))

(define (raw->string/mapper e)
  (if (byte-blob? e)
    (byte-blob->string e)
     e))

(define (unpack port #!optional (mapper identity))
  (let ((value (read-byte port)))
    (if (eof-object? value)
      value
      (cond ((hash-table-exists? repr-constant-map value)
             (let ((constant (hash-table-ref repr-constant-map value)))
               (cond ((eq? constant 'uint8)
                      (read-uint port 1 mapper))
                     ((eq? constant 'uint16)
                      (read-uint port 2 mapper))
                     ((eq? constant 'uint32)
                      (read-uint port 4 mapper))
                     ((eq? constant 'uint64)
                      (read-uint port 8 mapper))
                     ((eq? constant 'int8)
                      (read-sint port 1 mapper))
                     ((eq? constant 'int16)
                      (read-sint port 2 mapper))
                     ((eq? constant 'int32)
                      (read-sint port 4 mapper))
                     ((eq? constant 'int64)
                      (read-sint port 8 mapper))
                     ((eq? constant 'float)
                      (read-float port mapper))
                     ((eq? constant 'double)
                      (read-double port mapper))
                     ((eq? constant 'raw16)
                      (let ((size (read-uint port 2)))
                        (read-raw port size mapper)))
                     ((eq? constant 'raw32)
                      (let ((size (read-uint port 4)))
                        (read-raw port size mapper)))
                     ((eq? constant 'array16)
                      (let ((size (read-uint port 2)))
                        (read-array port size mapper)))
                     ((eq? constant 'array32)
                      (let ((size (read-uint port 4)))
                        (read-array port size mapper)))
                     ((eq? constant 'map16)
                      (let ((size (read-uint port 2)))
                        (read-map port size mapper)))
                     ((eq? constant 'map32)
                      (let ((size (read-uint port 4)))
                        (read-map port size mapper)))
                     (#t
                      (mapper constant)))))
            ((fixed-uint? value)
             (mapper value))
            ((fixed-sint? value)
             (mapper (- (byte-complement2 value))))
            ((fixed-raw? value)
             (let ((size (bitwise-and #x1f value)))
               (read-raw port size mapper)))
            ((fixed-array? value)
             (let ((size (bitwise-and #x0f value)))
               (read-array port size mapper)))
            ((fixed-map? value)
             (let ((size (bitwise-and #x0f value)))
               (read-map port size mapper)))
            (#t
             (error 'unpack "cannot unpack" value))))))
