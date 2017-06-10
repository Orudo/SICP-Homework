(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (eval-def exp)
 (def-variable
   (define-vari exp)
   (eval (define-val exp) env)
   env)
 `ok)

(define (define-val exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;paramaters
		   (caddr exp))));body

(define (make-lambda para body)
  (list `lambda para body))


					;now eval lambda on env
(eval lambda env)->

(define (make-process para body env)
  (list `process para body env))

					;now i have a vari 'append which is assigned to (`process (x y) body goble-env)


(append `(a b c) `(d e f))

(eval (append `(a b c) `(d e f)) g-env)
->
(apply (eval append env) (list-of-value (operand exp) env))

(apply (`proc (x y) body env) ((a b c) (d e f)))


(eval-seq (body (`proc (x y) body env))
	  (extend-env (para-proc proc)
		      args
		      (procedure-env proc)))
