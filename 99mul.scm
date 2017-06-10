(define ones (cons-stream 1 ones))
(define ints
  (cons-stream 1 (add-stream ints ones)))
(define (add-stream s1 s2)
  (cons-stream (+ (stream-car s1) (stream-car s2))
	       (add-stream (stream-cdr s1) (stream-cdr s2))))
(define (make-scheme n)
  (define (iter k)
    (if (> k n)
	`()
	(cons
	  (stream-filter (lambda (x) (not (> x (* k k))))
			 (scale-stream ints k))
	  (iter (+ k 1)))))
  (iter 1))
(define (scale-stream s fac)
  (stream-map (lambda (x) (* x fac))
	      s))
(define (display-stream s)
  (if (stream-null? s)
      `over
      (begin
	(display (stream-car s))
	(newline)
	(display-stream (stream-cdr s)))))

