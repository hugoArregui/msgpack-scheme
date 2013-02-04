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

(declare (unit utils))

(use test)
(use random-bsd)
(use byte-blob)

(define (empty-string? s)
  (string=? s ""))

(define (hash-table=? t1 t2 eq?)
  (and (= (hash-table-size t1) (hash-table-size t2))
       (every (lambda (k) 
                (and (hash-table-exists? t2 k)
                     (eq? (hash-table-ref t1 k) (hash-table-ref t2 k))))
              (hash-table-keys t1))))

(define (build-test-equals? default-equals?)
  (define (test-equals? v1 v2)
    (cond ((default-equals? v1 v2)
           #t)
          ((and (hash-table? v1)
                (hash-table? v2))
           (hash-table=? v1 v2 test-equals?))
          (#t
           #f)))
  test-equals?)

(define (make-byte-blob size)
  (byte-blob-replicate size (random-integer 256)))

(define (random-generator #!key (size 1000))
  (let ((rnd (random-integer size))
        (rnd2 (random-integer 1)))
    (cond ((< rnd (* size .01))
           (vector-ref '#(#t #f ()) (random-integer 3)))
          ((odd? rnd)
           (make-byte-blob (+ (random-integer 12) 1)))
          (#t
           (if (= 0 rnd2)
             rnd
             (- rnd))))))

(define (make-rnd-hash-table size)
  (define table (make-hash-table))
  (let loop ((size size))
    (if (> size 0)
      (let ((k size)
            (v (random-generator)))
        (hash-table-set! table k v)
        (loop (- size 1)))))
  (assert (= size (hash-table-size table )))
  table)

(current-test-comparator (build-test-equals? (current-test-comparator)))
