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

(require-library srfi-4 byte-blob numbers blob-set-int)
(import scheme chicken srfi-4 byte-blob numbers blob-set-int)

(define (byte-blob->float bblob)
  (let ((v (byte-blob->u8vector bblob)))
    (assert (= (u8vector-length v) 4))
    (u8vector->float v)))

(define u8vector->float
  (foreign-lambda* float ((nonnull-u8vector v))
#<<EOS
     union { uint32_t i; float f; } mem;
     memcpy(&mem.i, v, 8);
     C_return(mem.f);
EOS
))

(define float->uint32 
  (foreign-lambda* unsigned-integer32 ((float f))
#<<EOS
     union { float f; uint32_t i; } mem;
     mem.f = f;
     C_return(mem.i);
EOS
))

(define (byte-blob->double bblob)
  (let ((v (byte-blob->u8vector bblob)))
    (assert (= (u8vector-length v) 8))
    (u8vector->double v)))

(define u8vector->double
  (foreign-lambda* double ((nonnull-u8vector v))
#<<EOS
     union { uint64_t i; double d; } mem;
     memcpy(&mem.i, v, 8);
     C_return(mem.d);
EOS
))

(define double->uint16
  (foreign-lambda* void ((double d) 
                         ((ref unsigned-integer32) b1) 
                         ((ref unsigned-integer32) b2)
                         ((ref unsigned-integer32) b3)
                         ((ref unsigned-integer32) b4))
#<<EOS
     union { double d; uint64_t i; } mem;
     mem.d = d;
     b1 = mem.i & 0xffff;
     b2 = (mem.i >> 16) & 0xffff;
     b3 = (mem.i >> 32) & 0xffff;
     b4 = (mem.i >> 48) & 0xffff;
EOS
))

(define (double->byte-blob value)
  (let-location ((b1 unsigned-integer32)
                 (b2 unsigned-integer32)
                 (b3 unsigned-integer32)
                 (b4 unsigned-integer32))
                (double->uint16 value 
                                (location b1) 
                                (location b2)
                                (location b3)
                                (location b4))
                (let ((blob (make-blob 8)))
                  (blob-set-u16-be! blob b1 6)
                  (blob-set-u16-be! blob b2 4)
                  (blob-set-u16-be! blob b3 2)
                  (blob-set-u16-be! blob b4 0)
                  (blob->byte-blob blob))))
