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

(declare (not standard-bindings vector-length))
(include "utils.scm")
(include "msgpack-imple.scm")
(use test byte-blob numbers)

(define fast/full 'fast) ; some tests are slow

(define-syntax with-mocks
  (syntax-rules ()
    ((with-mocks ((name value) . rest) body ...)
     (let ((original name))
       (set! name value)
       (let ((r (with-mocks rest body ...)))
	 (set! name original)
	 r)))
    ((with-mocks () body ...)
     (let ()
       body ...))))

(test-group "read-byte/eof-error"
  (test "ok" 97 (call-with-input-string "a" read-byte/eof-error))
  (test-error "eof" (call-with-input-string "" read-byte/eof-error)))

(test-group "pack/unpack"
  (define (string-pack value)
    (call-with-output-string (cut pack <> value)))

  (define (pack/unpack value #!optional (mapper identity))
    (let* ((packed-buffer (string-pack value)))
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

  (test-group "str"
    (pack/unpack-test "fixed str" (make-string 1))
    (pack/unpack-test "str8" (make-string 40))
    (pack/unpack-test "str16" (make-string 40))
    (if (eq? fast/full 'full)
      (pack/unpack-test "str32" (make-string (expt 2 17)))))

  (test-group "bin"
    (let ((v (string->byte-blob "hola")))
      (mapper-test "bin mapper" v byte-blob->string))
    (pack/unpack-test "bin8" (make-byte-blob 40))
    (pack/unpack-test "bin16" (make-byte-blob 40))
    (if (eq? fast/full 'full)
      (pack/unpack-test "bin32" (make-byte-blob (expt 2 17)))))

  (test-group "array"
    (let ((list '(1 2 3 4)))
      (test "list" (pack/unpack list) (list->vector list)))
    (pack/unpack-test "fixed array" (make-vector 1 1))
    (pack/unpack-test "fixed array: uint and raw" `#(1 2 ,(string->byte-blob "hola")))
    (pack/unpack-test "array16" (make-vector 40 1))
    (pack/unpack-test "nested array" '#(1 2 #(1 4)))
    (if (eq? fast/full 'full)
      (pack/unpack-test "array32" (make-vector (expt 2 17) 1))))

  (test-group "map"
    (pack/unpack-test "fixed map" (make-rnd-hash-table 2))
    (pack/unpack-test "map16" (make-rnd-hash-table 40))
    (let ((table (make-rnd-hash-table 10)))
      (hash-table-set! table 1 (make-rnd-hash-table 2))
      (pack/unpack-test "nested map" table))
    (if (eq? fast/full 'full)
      (pack/unpack-test "map32" (make-rnd-hash-table (expt 2 17)))))
  ); end pack/unpack-test

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

  (define-syntax test-struct-limit
    (syntax-rules ()
      ((test-struct-limit size_proc_name type value min max)
       (let ()
	 (with-mocks ((size_proc_name (lambda (e) min)))
		     (test-header (string-append (symbol->string type) " min") value type))
	 (with-mocks ((size_proc_name (lambda (e) max)))
		     (test-header (string-append (symbol->string type) " max") value type))))))

  (define-syntax test-struct-out-of-limit
    (syntax-rules ()
      ((test-struct-out-of-limit size_proc_name limit value)
       (with-mocks ((size_proc_name (lambda (x) (+ limit 1))))
		   (test-error "out of limit" (fake-pack value))))))

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

  (test-group "bin"
    (define (test-bin-limit type min max)
      (test-struct-limit byte-blob-length type (byte-blob-empty) min max))

    (define (bin-packed-header size)
      (with-mocks ((byte-blob-length (lambda (e) size)))
		  (packed-header (byte-blob-empty))))

    (with-mocks ((write-raw (lambda (port value size) #t)))
		(test-bin-limit 'bin8  (+ 1 fixed_raw_limit) raw8_limit)
		(test-bin-limit 'bin16 (+ 1 raw8_limit)      raw16_limit)
		(test-bin-limit 'bin32 (+ 1 raw16_limit)     raw32_limit)
		(test-struct-out-of-limit byte-blob-length raw32_limit (byte-blob-empty))))

  (test-group "str"
    (define (test-str-limit type min max)
      (test-struct-limit byte-blob-length type "" min max))

    (define (str-packed-header size)
      (with-mocks ((byte-blob-length (lambda (e) size)))
		  (packed-header "")))

    (with-mocks ((write-raw (lambda (port value size) #t)))
		(test-assert "fixed str min" (fixed-str? (str-packed-header 1)))
		(test-assert "fixed str max" (fixed-str? (str-packed-header fixed_raw_limit)))
		(test-str-limit 'str8  (+ 1 fixed_raw_limit) raw8_limit)
		(test-str-limit 'str16 (+ 1 raw8_limit)      raw16_limit)
		(test-str-limit 'str32 (+ 1 raw16_limit)     raw32_limit)
		(test-struct-out-of-limit byte-blob-length raw32_limit (byte-blob-empty))))

  (test-group "array"
    (define (test-array-limit type min max)
      (test-struct-limit vector-length type '#() min max))

    (define (array-packed-header size)
      (with-mocks ((vector-length (lambda (e) size)))
		  (packed-header '#())))

    (with-mocks ((write-array (lambda (port value size) #t)))
		(test-assert "fixed array min" (fixed-array? (array-packed-header 0)))
		(test-assert "fixed array max" (fixed-array? (array-packed-header fixed_array_limit)))
		(test-array-limit 'array16 (+ 1 fixed_array_limit) array16_limit)
		(test-array-limit 'array32 (+ 1 array16_limit) array32_limit)
		(test-struct-out-of-limit vector-length array32_limit '#())))

  (test-group "map"
    (define (test-map-limit type min max)
      (test-struct-limit hash-table-size type (make-hash-table) min max))

    (define (map-packed-header size)
      (with-mocks ((hash-table-size (lambda (e) size)))
		  (packed-header (make-hash-table))))

    (test-assert "fixed map min" (fixed-map? (map-packed-header 0)))
    (test-assert "fixed map max" (fixed-map? (map-packed-header fixed_map_limit)))
    (test-map-limit 'map16 (+ 1 fixed_map_limit) map16_limit)
    (test-map-limit 'map32 (+ 1 map16_limit) map32_limit)
    (test-struct-out-of-limit hash-table-size map32_limit (make-hash-table)))
  ) ;end limits test

(test-exit)
