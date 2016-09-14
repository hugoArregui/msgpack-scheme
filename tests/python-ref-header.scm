(use byte-blob test msgpack)

(define (byte-blob . args)
  (byte-blob-reverse 
    (fold
      byte-blob-cons
      (byte-blob-empty)
      args)))

(define (unpack/from-blob blob)
  (let ((s (byte-blob->string blob)))
    (call-with-input-string s (cut unpack <>))))

(define (pack/to-blob value)
  (string->byte-blob
    (call-with-output-string (cut pack <> value))))

