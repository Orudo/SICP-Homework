(put 


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get (get-tag exp)) ((get (get-tag exp)) exp env)) 
	((application? exp)
	 (apply (eval (operator exp) env)
		(list-of-value (operand exp) env)))))
