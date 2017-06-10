(define (apply proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 (eval-sequence
	  (procedure-body proc)
	  (extend-environment
	   (procedure-paramaters proc)
	   args
	   (procedure-environment proc))))
	(else
	 (error "Unknown Procedure type--APPLY" proc))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-paramaters exp)
			 (lambda-body exp)
			 (env)))
	((begin? exp)
	 (eval-sequence (begin-action exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-value (operand exp) env)))
	(else
	 (error "Unknown expression type--EVAL" exp))))
(define (evaln exp env)
  (cond ((self-evaluating? exp) env)
	((variable? exp) (lookup-variable-value exp env))
	((get (car exp)) ((get (car exp)) exp env))
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-value (operand exp) env)))
	(else
	 (error "UNKNOWN experssion type -- EVALN" exp))))
(define operation-table (make-table))
(define get (operation-table `lookup-proc))
(define put (operation-table `insert-proc))

(put `op `quote text-of-quotation)
(put `op `assignment eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if) 




(define (list-of-value exps env)
  (if (no-operands? exps)
      `()
      (cons (eval (first-operand exps) env)
	    (list-of-value (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exp) env))
	(else (eval (first-exp exp) env)
	      (eval-sequence (rest-exps exp) env))))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignmeng-value exp) env)
		       env)
  `ok)
(define (eval-definition exp env)
  (set-variable-value! (definition-variable exp)
		       (eval (difinition-value exp) env)
		       env)
  `ok)

(define (eval-and exp) (expand-and (cdr exp) env))
(define (and-last? exp) (null? (cdr exp)))
(define (and-first exp) (car exp))
(define (and-rest exp) (cdr exp))
(define (expand-and exp)
  (cond ((null? exp) `true)
	((and-last? exp) (apply (and-first exp)))
	((not (and-frist exp)) `false)
	(else (expand-and (and-rest exp)))))
(define (expand-or exp)
  (cond ((null? exp) `false)
	((or-last? exp) `false)
	((or-first exp) `true)
	(else (expand-or (or-rest exp)))))
      
	  





(define (self-evaluating? exp)
  (cond ((number? exp) #t)
	((string? exp) #t)
	(else #f)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp `quote))
(define (text-of-quotation exp) (cadr exp))


(define (tag-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp)
  (tagged-list? exp `set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

(define (definition? exp)
  (tagged-list? exp `define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp);parameter list
		   (caddr exp))));body

(define (lambda? exp)
  (tagged-list? exp `lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (caddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp `if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null?  (cadddr exp)))
      (cadddr exp)
      `false))
(define (make-if predicate consequent alternative)
  (list `if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp `begin))

(define (begin-actions exp) (cdr exp))
(define (last-action? exp) (null? (cdr exp)))
(define (first-action exp) (car exp))
(define (rest-action exp) (cdr exp))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq)
  (cons `begin seq))

(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operand exp)
  (cdr exp))
(define (no-operand? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))

(define (cond? exp) (tagged-list? exp `cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq (cond-predicate clause) `else))
(define (cond-predicate clause) (car  clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clause (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      `false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last-- COND->IF"))
	    (make-if (cond-predicate first)
		     (sequence->exp (cond-actions first))
		     (expand-clauses rest))))))


