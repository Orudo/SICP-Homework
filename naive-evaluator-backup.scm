
(define apply-in-underlying-scheme apply)
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quote? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((cond? exp) (eval-cond exp env))
	((lambda? exp)
	 ;(display "eval-lambda")
	 ;(newline)
	 ;(display exp)
	 (make-procedure (lambda-parameters exp)
			 (cddr exp)
			 env))
	((Begin? exp)
	 (eval-sequence (begin-action exp) env))
	((let? exp)
	 (eval-let (let-content exp) env))
	((application? exp)
	 (apply-myevaluator
	  (eval (operator exp) env)
	  (list-of-value (operand exp) env)))
	(else
	 (error "Unknown expression type--EVAL" exp))))


(define (showtag exp)
  (car tag))

(define (variable? exp)
  (symbol? exp))
(define (quote? exp)
  (tagged-list exp `quote))
(define (tagged-list exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
(define (assignment? exp)
  (tagged-list exp `set!))
(define (definition? exp)
  (tagged-list exp `define))
(define (if? exp)
  (tagged-list exp `if))
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))


(define (text-of-quotation exp)
  (cdr exp))


					;imp for let
(define (let? exp)
  (tagged-list exp `let))
(define (let-content exp)
  (cdr exp))
(define (eval-let exp env)
  (let ((list-of-var-val (car exp))
	(body (cdr exp)))
    (display "eval let")
    (newline)
    ;(display (let-lambda list-of-var-val body))
    (apply-myevaluator (eval (make-lambda (map car list-of-var-val) body) env) (map cadr list-of-var-val))))
(define (make-let var-val body)
  (cons 'let (cons var-val body)))




;Sentences for assignment
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		      (eval (assignment-value exp) env)
		      env)
  `ok)
					;Sentence for definition
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
		 (eval (definition-value exp) env)
		 env)
  `ok)
					;sentence for if



	



					;implementation above may not runable
(define (no-operand args)
  (null? args))
(define (list-of-value args env)
  (if (no-operand args)
      `()
      (cons (eval (first-operand args) env)
	    (list-of-value (rest-operand args) env))))
					; the implementation is from scip


;implementation for if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (if? exp)
  (tagged-list exp `if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      `false));the false for user
(define (make-if predicate consequent alternative)
  (list `if predicate consequent alternative))

					;implementation for consequent sentence
(define (begin? exp)
  (tagged-list exp `begin))
(define (begin-action exp)
  (cdr exp))
(define (last-exp exp)
  (null? (cdr exp)))

(define (first-exp exp)
  (car exp))
(define (rest-exp exp)
  (cdr exp))
(define (make-begin seq)
  (cons `begin seq))
(define (seq->exp seq);used in cond sentence where no begin tagged
  (cond ((null? seq) seq)
	((last-exp seq) (first-exp seq))
	(else (make-begin seq))))


(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operand exp)
  (cdr exp))

(define (first-operand ops)
  (car ops))
(define (rest-operand ops)
  (cdr ops))



					;implementation for lambda
(define (make-lambda para body)
  (cons `lambda (cons para body)))
(define (lambda? exp)
  (tagged-list exp `lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))


(define (eval-sequence exp env)
  (cond ((last-exp exp) (eval (first-exp exp) env))
	(else (eval (first-exp exp) env)
	      (eval-sequence (rest-exp exp) env))))


(define (primitive-procedure? exp)
  (tagged-list exp `primitive))

(define (apply-myevaluator proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 (eval-sequence (procedure-body proc)
			(extend-environment (procedure-parameters proc)
				    args
				    (procedure-environment proc))))
	(else
	 (error "UNKNOWN proc type--APPLY" proc)))
  )





					;predication check
(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
      


(define (make-procedure para body env)
  (display (list `res para (scan-out-defines body)))
  (list `procedure para (scan-out-defines body) env))
(define (scan-out-defines body)
  (let ((internal-def (filter (lambda (x) (eq? (car x) `define)) body))
	(no-def-body (filter (lambda (x) (not (eq? (car x) `define))) body)))
    (if (null? internal-def)
	body
	(let ((list-of-var-val (map (lambda (x) (list x `*unassigned*)) (map definition-variable internal-def)))
	      (sets (map (lambda (var body) (list `set! var body)) (map definition-variable internal-def)  (map definition-value internal-def))))
	  (list (make-let list-of-var-val (append sets no-def-body)))))))
	    
(define (compound-procedure? exp)
  (tagged-list exp `procedure))
(define (procedure-parameters exp)
  (cadr exp))
(define (procedure-body exp)
  (caddr exp))
(define (procedure-environment exp)
  (cadddr exp))


					;how to find a var on a env

(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))
(define (make-frame vars vals)
  (cons vars vals))
(define (enclosing-environment env)
  (cdr env))
(define (first-frame env)
  (car env))
(define the-empty-environment `())
(define (add-binding var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) env)
      (if (< (length vars) (length vals))
	  (error "too many arguments supplied"vars vals )
	  (error "Too few args supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (loop-env env)
    (define (scan vars vals)
      (cond ((null? vars) (loop-env (enclosing-environment env)))
	    ((eq? (car vars) var) (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "No such var on environment--lookup-var-val" var)
	(scan (frame-variables (first-frame env)) (frame-values (first-frame env)))))
  (loop-env env));mahou no you na koudo

(define (set-variable-value! var val env)
  (define (loop-env env)
    (define (scan vars vals)
      (cond ((null? vars) (loop-env (enclosing-environment env)))
	    ((eq? (car vars) var) (set-car! vals val))
	    (else (scan (cdr vars vals)))))
    (if (eq? env the-empty-environment)
	(error "No such var on given environment--set-var-val!" var)
	(scan (frame-variables (first-frame env)) (frame-values (first-frame env)))))
  (loop-env env))
(define (define-variable! var val env)
  (begin
    (define (scan vars vals)
      (cond ((null? vars) (add-binding var val (first-frame env)))
	    ((eq? (car vars) var) (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables (first-frame env))
	  (frame-values (first-frame env))))
  )

(define (setup-environment)
  (let ((init-environment
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
  (define-variable! `true #t init-environment)
  (define-variable! `false #f init-environment)
  init-environment))


(define (primitive-implementation exp)
  (cadr exp))

(define primitive-procedures
  (list (list `car car)
	(list `+ +)
	(list `= =)
	(list `cdr cdr)
	(list `list list)
	(list `cons cons)
	(list `null? null?)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list `primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (begin
    (apply-in-underlying-scheme
     (primitive-implementation proc)
     args)))



  

(define global-environment (setup-environment))



(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval output:")
(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))
(define (announce-output string )
  (newline)
  (display string)
  (newline))
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (display input)
    (newline)
    (let ((output (eval input global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list `compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     `procedure env))
      (display object)))



				      



					;cond implementation
(define (cond-clauses exp)
  (cdr exp))
(define (cond? exp)
  (tagged-list exp 'cond))
(define (cond-predicate clause)
  (car clause))
(define (cond-action clause)
  (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (not (cond-else-clause? first))
	    (make-if (cond-predicate first)
		     (seq->exp (cond-action first))
		     (expand-clauses rest))
	    (seq->exp (cond-action first))))))
(define (cond->if exp)
  ;(display (expand-clauses (cond-clauses exp)))
  (expand-clauses (cond-clauses exp)))
(define (eval-cond exp env)
  (eval (cond->if exp) env))
  










					;imp for internal simultaneous def
