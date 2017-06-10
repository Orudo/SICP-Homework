(define (let? exp) (tagged-list? exp `let))
(define (let-para-value exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let->combination exp)
  (let ((para (map car (let-para-value exp)))
	(value (map cdr (let-para-value exp))))
    (eval (make-lambda para (let-body exp)) value)))



;nominated let


(define (name-let? exp) (and (let? exp) (symbol? (cadr exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-vari exp) (map car(caddr exp)))
(define (named-let-valu exp) (map cdr (caddr exp)))
(define (named-let-body exp) (cadddr exp))
(define (let->func exp)
  (list 'define (cons (named-let-vari exp) (named-let-valu exp)) (named-let-body exp)))
  
	
