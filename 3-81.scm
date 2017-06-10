(define (random-update guess)
  (remainder (+ (* 13 x) 5) 24))

(define (random-init x)
  (define stream
    (cons-strema x (stream-map random-update stream)))
  stream)
(define (show-first s)
  (stream-ref s 1))
(define (random-stream operate)
  (define (op-parse operation result)
    (cond ((eq? operation `reset) (random-init 0))
	  ((eq? operation `generate) (stream-cdr result))
	  ((error "UNKNOWN operation ---- random-stream" operate))))
  (define result
    (cons-stream (random-init 0)
		 (stream-map op-parse operate result)))
  result)
	
