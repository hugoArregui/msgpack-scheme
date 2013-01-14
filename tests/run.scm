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


(load "msgpack.scm")
(load "tests/utils.scm")

(define fast/full 'fast) ; some tests are slow

(test-group "read-byte/eof-error"
            (test "ok" 97 (call-with-input-string "a" read-byte/eof-error))
            (test-error "eof" (call-with-input-string "" read-byte/eof-error)))

(test-group "pack/unpack"
            (define (pack/unpack value #!optional (mapper identity))
              (let* ((packed-buffer (call-with-output-string (cut pack <> value))))
                (call-with-input-string packed-buffer (cut unpack <> mapper))))

            (define (pack/unpack-test name value #!optional (mapper identity) (packer pack))
              (let* ((packed-buffer (call-with-output-string (cut packer <> value))))
                (call-with-input-string packed-buffer 
                                        (lambda (port)
                                          (let ((v (unpack port mapper)))
                                            (test name value v) ; unpacked value is the same that was packed
                                            (test-assert "" (eof-object? (unpack port mapper))) ; no more left
                                            v)))))

            (define (mapper-test name value mapper #!optional (packer pack))
              (let* ((packed-buffer (call-with-output-string (cut packer <> value))))
                (call-with-input-string packed-buffer 
                                        (lambda (port)
                                          (let ((v (unpack port mapper)))
                                            (test name v (mapper value))
                                            v)))))
            (test-group "constants"
                        (let ((mapper (lambda (x) (not x))))
                          (mapper-test "constant mapper" #f mapper))
                        (pack/unpack-test "true" #t)
                        (pack/unpack-test "false" #f)
                        (pack/unpack-test "null" '()))

            (test-group "uint"
                        (let ((c #\a))
                          (test "char" (pack/unpack c) (char->integer c)))
                        (let ((mapper (lambda (x) (+ x 1)))
                              (v 30))
                          (mapper-test "uint mapper" v mapper))
                        (pack/unpack-test "fixed uint" 1)
                        (pack/unpack-test "uint8" 200)
                        (pack/unpack-test "uint16" 60000)
                        (pack/unpack-test "uint32" 70000)
                        (pack/unpack-test "uint64" (expt 2 33)))

            (test-group "flonum"
                        (let ((mapper (lambda (x) (+ x 1)))
                              (v 3.7))
                          (mapper-test "float32 mapper" v mapper pack-float)
                          (mapper-test "double64 mapper" v mapper))
                        (pack/unpack-test "float32" 1.3 identity pack-float)
                        (pack/unpack-test "double64" 1.3))

            (test-group "sint"
                        (let ((mapper (lambda (x) (+ x 1)))
                              (v -1))
                          (mapper-test "sint mapper" v mapper))
                        (pack/unpack-test "fixed sint" -1)
                        (pack/unpack-test "sint8" -40)
                        (pack/unpack-test "sint16" -30000)
                        (pack/unpack-test "sint32" -50000)
                        (pack/unpack-test "sint64" (- (expt 2 33))))

            (test-group "raw"
                        (let ((v (string->byte-blob "hola")))
                          (mapper-test "raw mapper" v raw->string/mapper))
                        (pack/unpack-test "fixed raw" (make-byte-blob 1))
                        (pack/unpack-test "raw16" (make-byte-blob 40))
                        (if (eq? fast/full 'full)
                          (pack/unpack-test "raw32" (make-byte-blob (expt 2 17)))))

            (test-group "array"
                        (pack/unpack-test "array mapper" '#("hola" 1 2) raw->string/mapper)
                        (let ((list '(1 2 3 4)))
                          (test "list" (pack/unpack list) (list->vector list)))
                        (pack/unpack-test "fixed array" (make-vector 1 1))
                        (pack/unpack-test "fixed array: uint and raw" `#(1 2 ,(string->byte-blob "hola")))
                        (pack/unpack-test "array16" (make-vector 40 1))
                        (pack/unpack-test "nested array" '#(1 2 #(1 4)))
                        (if (eq? fast/full 'full)
                          (pack/unpack-test "array32" (make-vector (expt 2 17) 1))))

            (test-group "map"
                        (pack/unpack-test "map mapper" 
                                          (alist->hash-table '(("hola" . 1)
                                                               (12 . "chau"))) raw->string/mapper)
                        (pack/unpack-test "fixed map" (make-rnd-hash-table 2))
                        (pack/unpack-test "map16" (make-rnd-hash-table 40))
                        (let ((table (make-rnd-hash-table 10)))
                          (hash-table-set! table 1 (make-rnd-hash-table 2))
                          (pack/unpack-test "nested map" table))
                        (if (eq? fast/full 'full)
                          (pack/unpack-test "map32" (make-rnd-hash-table (expt 2 17)))))
            ); end pack/unpack-test

;(test-group "mapper"

;(read-uint port size #!optional (mapper identity))
;(read-sint port size #!optional (mapper identity))
;(read-float port #!optional (mapper identity))
;(read-map port size #!optional (mapper identity)) 
;(read-array port size #!optional (mapper identity)) 
;(read-double port #!optional (mapper identity)) 
;(read-raw port size #!optional (mapper identity))
  

(test-group "limits"
            (define (fake-pack value)
              (string->byte-blob (call-with-output-string (cut pack <> value))))

            (define (packed-header value)
              (byte-blob-uref (fake-pack value) 0))

            (define (test-header name value header)
              (test name (packed-header value) (hash-table-ref constant-repr-map header)))

            (define (test-limit type min max)
              (test-header (string-append (symbol->string type) " min") min type)
              (test-header (string-append (symbol->string type) " max") max type))

            (define (test-struct-limit type size-mocker restorer value min max)
              (size-mocker min)
              (test-header (string-append (symbol->string type) " min") value type)
              (size-mocker max)
              (test-header (string-append (symbol->string type) " max") value type)
              (restorer))

            (define (test-struct-out-of-limit size-mocker restorer value limit)
              (size-mocker (+ limit 1))
              (test-error "out of limit" (fake-pack value))
              (restorer))

            (define (struct-packed-header mocker restorer value size)
              (mocker size)
              (let ((header (packed-header value)))
                (restorer)
                header))

            (test-group "uint"
                        (test-assert "fixed uint min" (fixed-uint? (packed-header 0)))
                        (test-assert "fixed uint max" (fixed-uint? (packed-header fixed_uint_limit)))
                        (test-limit 'uint8 (+ fixed_uint_limit 1) uint8_limit)
                        (test-limit 'uint16 (+ uint8_limit 1) uint16_limit)
                        (test-limit 'uint32 (+ uint16_limit 1) uint32_limit)
                        (test-limit 'uint64 (+ uint32_limit 1) uint64_limit)
                        (test-error "out of limit" (fake-pack (+ uint64_limit 1))))

            (test-group "sint"
                        (test-assert "fixed sint min" (fixed-sint? (packed-header -1)))
                        (test-assert "fixed sint max" (fixed-sint? (packed-header fixed_int_limit)))
                        (test-limit 'int8 (- fixed_int_limit 1)  int8_limit)
                        (test-limit 'int16 (- int8_limit 1) int16_limit)
                        (test-limit 'int32 (- int16_limit 1) int32_limit)
                        (test-limit 'int64 (- int32_limit 1) int64_limit)
                        (test-error "out of limit" (fake-pack (- int64_limit 1))))

            (test-group "raw"
                        (let* ((raw (byte-blob-empty))
                               (restorer (let ((len_original byte-blob-length)
                                               (write_original write-raw))
                                           (lambda ()
                                             (set! write-raw write_original)
                                             (set! byte-blob-length len_original))))
                               (mocker (lambda (size)
                                         (set! write-raw (lambda (port value size) #t))
                                         (set! byte-blob-length (lambda (e) size))))
                               (raw-packed-header (lambda (size)
                                                    (struct-packed-header mocker restorer raw size)))
                               (test-raw-limit (lambda (type min max)
                                                 (test-struct-limit type mocker restorer raw min max))))
                          (test-assert "empty" (empty-string? (call-with-output-string (cut pack <> raw))))
                          (test-assert "fixed raw min" (fixed-raw? (raw-packed-header 1)))
                          (test-assert "fixed raw max" (fixed-raw? (raw-packed-header fixed_raw_limit)))
                          (test-raw-limit 'raw16 (+ 1 fixed_raw_limit) raw16_limit)
                          (test-raw-limit 'raw32 (+ 1 raw16_limit) raw32_limit)
                          (test-struct-out-of-limit mocker restorer raw raw32_limit)))

            (test-group "array"
                        (let* ((array '#())
                               (restorer (let ((len_original vector-length)
                                               (write_original write-array))
                                           (lambda ()
                                             (set! write-array write_original)
                                             (set! vector-length len_original))))
                               (mocker (lambda (size)
                                         (set! write-array (lambda (port value size) #t))
                                         (set! vector-length (lambda (e) size))))
                               (array-packed-header (lambda (size)
                                                    (struct-packed-header mocker restorer array size)))
                               (test-array-limit (lambda (type min max)
                                                 (test-struct-limit type mocker restorer array min max))))
                          (test-assert "fixed array min" (fixed-array? (array-packed-header 0)))
                          (test-assert "fixed array max" (fixed-array? (array-packed-header fixed_array_limit)))
                          (test-array-limit 'array16 (+ 1 fixed_array_limit) array16_limit)
                          (test-array-limit 'array32 (+ 1 array16_limit) array32_limit)
                          (test-struct-out-of-limit mocker restorer array array32_limit)))

            (test-group "map"
                        (let* ((table (make-hash-table))
                               (restorer (let ((original hash-table-size))
                                           (lambda ()
                                             (set! hash-table-size original))))
                               (mocker (lambda (size)
                                         (set! hash-table-size (lambda (e) size))))
                               (map-packed-header (lambda (size)
                                                    (struct-packed-header mocker restorer table size)))
                               (test-map-limit (lambda (type min max)
                                                 (test-struct-limit type mocker restorer table min max))))
                          (test-assert "fixed map min" (fixed-map? (map-packed-header 0)))
                          (test-assert "fixed map max" (fixed-map? (map-packed-header fixed_map_limit)))
                          (test-map-limit 'map16 (+ 1 fixed_map_limit) map16_limit)
                          (test-map-limit 'map32 (+ 1 map16_limit) map32_limit)
                          (test-struct-out-of-limit mocker restorer table map32_limit)))
            ); end limits test

(test-exit)

