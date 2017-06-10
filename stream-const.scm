(define (add-streams a b)
  (cons-stream (+ (stream-car a) (stream-car b))
	       (add-streams (stream-cdr a) (stream-cdr b))))
(define ones
  (cons-stream 1 ones))
(define ints
  (cons-stream 1 (add-streams ints ones)))
