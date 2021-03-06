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
  (define (packs value #!optional (packer pack))
    (call-with-output-string (cut packer <> value)))

  (define (pack/unpack value #!optional (mapper identity))
    (let* ((packed-buffer (packs value)))
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
    (test-error "invalid: negative number" (packs -1 pack-uint))
    (test-error "invalid: no integer" (packs '() pack-uint))
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
    (pack/unpack-test "positive" 10)
    (pack/unpack-test "positive" 3000)
    (pack/unpack-test "fixed sint" -1)
    (pack/unpack-test "sint8" -40)
    (pack/unpack-test "sint16" -30000)
    (pack/unpack-test "sint32" -50000)
    (pack/unpack-test "sint64" (- (expt 2 33))))

  (test-group "str"
    (test-error "invalid: number" (packs -1 pack-str))
    (pack/unpack-test "fixed str" (make-string 1))
    (pack/unpack-test "str8" (make-string 40))
    (pack/unpack-test "str16" (make-string 40))
    (if (eq? fast/full 'full)
      (pack/unpack-test "str32" (make-string (expt 2 17)))))

  (test-group "bin"
    (test-error "invalid: number" (packs -1 pack-bin))
    (let ((v (string->byte-blob "hola")))
      (mapper-test "bin mapper" v byte-blob->string))
    (pack/unpack-test "bin8" (make-byte-blob 40))
    (pack/unpack-test "bin16" (make-byte-blob 40))
    (if (eq? fast/full 'full)
      (pack/unpack-test "bin32" (make-byte-blob (expt 2 17)))))

  (test-group "ext"
    (test-error "invalid ext type" (packs (make-extension 200 (make-byte-blob 1)) pack-ext))
    (pack/unpack-test "fixext1"  (make-extension 1 (make-byte-blob 1)))
    (pack/unpack-test "fixext2"  (make-extension 1 (make-byte-blob 2)))
    (pack/unpack-test "fixext4"  (make-extension 1 (make-byte-blob 4)))
    (pack/unpack-test "fixext8"  (make-extension 1 (make-byte-blob 8)))
    (pack/unpack-test "fixext16" (make-extension 1 (make-byte-blob 16)))
    (pack/unpack-test "ext8"     (make-extension 1 (make-byte-blob 17)))
    (pack/unpack-test "ext16"    (make-extension 1 (make-byte-blob raw16_limit)))
    (if (eq? fast/full 'full)
      (pack/unpack-test "ext32"  (make-extension 1 (make-byte-blob (add1 raw16_limit))))))

  (test-group "array"
    (test-error "invalid: number" (packs -1 pack-array))
    (let ((list '(1 2 3 4)))
      (test "list" (pack/unpack list) (list->vector list)))
    (pack/unpack-test "fixed array" (make-vector 1 1))
    (pack/unpack-test "fixed array: uint and raw" `#(1 2 ,(string->byte-blob "hola")))
    (pack/unpack-test "array16" (make-vector 40 1))
    (pack/unpack-test "nested array" '#(1 2 #(1 4)))
    (if (eq? fast/full 'full)
      (pack/unpack-test "array32" (make-vector (expt 2 17) 1))))

  (test-group "map"
    (test-error "invalid: number" (packs -1 pack-map))
    (pack/unpack-test "fixed map" (make-rnd-hash-table 2))
    (pack/unpack-test "map16" (make-rnd-hash-table 40))
    (let ((table (make-rnd-hash-table 10)))
      (hash-table-set! table 1 (make-rnd-hash-table 2))
      (pack/unpack-test "nested map" table))
    (if (eq? fast/full 'full)
      (pack/unpack-test "map32" (make-rnd-hash-table (expt 2 17)))))
  ); end pack/unpack-test

(test-group "limits"
  (define (pack/as-blob value)
    (string->byte-blob (call-with-output-string (cut pack <> value))))

  (define (packed-header value)
    (byte-blob-uref (pack/as-blob value) 0))

  (define (test-header name value header)
    (test name (packed-header value) (hash-table-ref constant-repr-map header)))

  (define (test-limit type min max)
    (test-header (string-append (symbol->string type) " min") min type)
    (test-header (string-append (symbol->string type) " max") max type))

  (define-syntax test-container-limit
    (syntax-rules ()
      ((test-container-limit size_proc_name type value min max)
       (let ()
	 (with-mocks ((size_proc_name (lambda (e) min)))
		     (test-header (string-append (symbol->string type) " min") value type))
	 (with-mocks ((size_proc_name (lambda (e) max)))
		     (test-header (string-append (symbol->string type) " max") value type))))))

  (define-syntax test-container-out-of-limit
    (syntax-rules ()
      ((test-container-out-of-limit size_proc_name limit value)
       (with-mocks ((size_proc_name (lambda (x) (+ limit 1))))
		   (test-error "out of limit" (pack/as-blob value))))))

  (test-group "uint"
    (test-assert "fixed uint min" (fixed-uint? (packed-header 0)))
    (test-assert "fixed uint max" (fixed-uint? (packed-header fixed_uint_limit)))
    (test-limit 'uint8 (+ fixed_uint_limit 1) uint8_limit)
    (test-limit 'uint16 (+ uint8_limit 1) uint16_limit)
    (test-limit 'uint32 (+ uint16_limit 1) uint32_limit)
    (test-limit 'uint64 (+ uint32_limit 1) uint64_limit)
    (test-error "out of limit" (pack/as-blob (+ uint64_limit 1))))

  (test-group "sint"
    (test-assert "fixed sint min" (fixed-sint? (packed-header -1)))
    (test-assert "fixed sint max" (fixed-sint? (packed-header fixed_int_limit)))
    (test-limit 'int8 (- fixed_int_limit 1)  int8_limit)
    (test-limit 'int16 (- int8_limit 1) int16_limit)
    (test-limit 'int32 (- int16_limit 1) int32_limit)
    (test-limit 'int64 (- int32_limit 1) int64_limit)
    (test-error "out of limit" (pack/as-blob (- int64_limit 1))))

  (test-group "bin"
    (define (test-bin-limit type min max)
      (test-container-limit byte-blob-length type (byte-blob-empty) min max))

    (with-mocks ((write-raw (lambda (port value size) #t)))
		(test-bin-limit 'bin8  (+ 1 fixed_raw_limit) raw8_limit)
		(test-bin-limit 'bin16 (+ 1 raw8_limit)      raw16_limit)
		(test-bin-limit 'bin32 (+ 1 raw16_limit)     raw32_limit)
		(test-container-out-of-limit byte-blob-length raw32_limit (byte-blob-empty))))

  (test-group "str"
    (define (test-str-limit type min max)
      (test-container-limit byte-blob-length type "" min max))

    (with-mocks ((write-raw (lambda (port value size) #t)))
		(test-assert "fixed str min" (fixed-str? (packed-header "")))
		(test-assert "fixed str max" (fixed-str? (packed-header (make-string fixed_raw_limit))))
		(test-str-limit 'str8  (+ 1 fixed_raw_limit) raw8_limit)
		(test-str-limit 'str16 (+ 1 raw8_limit)      raw16_limit)
		(test-str-limit 'str32 (+ 1 raw16_limit)     raw32_limit)
		(test-container-out-of-limit byte-blob-length raw32_limit (byte-blob-empty))))

  (test-group "array"
    (define (test-array-limit type min max)
      (test-container-limit vector-length type '#() min max))

    (with-mocks ((write-array (lambda (port value size) #t)))
		(test-assert "fixed array min" (fixed-array? (packed-header '#())))
		(test-assert "fixed array max" (fixed-array? (packed-header (make-vector fixed_array_limit 1))))
		(test-array-limit 'array16 (+ 1 fixed_array_limit) array16_limit)
		(test-array-limit 'array32 (+ 1 array16_limit) array32_limit)
		(test-container-out-of-limit vector-length array32_limit '#())))

  (test-group "map"
    (define (test-map-limit type min max)
      (test-container-limit hash-table-size type (make-hash-table) min max))

    (with-mocks ((write-map (lambda (port value size) #t)))
		(test-assert "fixed map min" (fixed-map? (packed-header (make-rnd-hash-table 0))))
		(test-assert "fixed map max" (fixed-map? (packed-header (make-rnd-hash-table fixed_map_limit))))
		(test-map-limit 'map16 (+ 1 fixed_map_limit) map16_limit)
		(test-map-limit 'map32 (+ 1 map16_limit) map32_limit)
		(test-container-out-of-limit hash-table-size map32_limit (make-hash-table))))
  ) ;end limits test

(test-exit)
