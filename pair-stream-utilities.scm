(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))
(define (display-stream s times)
  (if (> times 0)
      (begin
	(display (stream-car s))
	(display-stream (stream-cdr s) (- times 1)))
      `finish))
  
