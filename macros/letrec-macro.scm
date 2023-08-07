

((lambda ()


(define letrec->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'letrec))))

(define letrec->compound
  (lambda (ex)
    (define recur-syms (lambda (syms)
			 (cond
			  ((null? syms) '())
			  (#t (cons (list (car syms) (quote (quote *dummy*)))
				    (recur-syms (cdr syms)))))))

    (define recur-syms-defs (lambda (syms defs)
			      (cond
			       ((null? syms) '())
			       (#t (cons (append (list 'set! (car syms)) (car defs))
					 (recur-syms-defs (cdr syms) (cdr defs)))))))
    (let ((symbols (map car (cadr ex)))
	  (definitions (map cdr (cadr ex)))
	  (body (cddr ex)))
      (append (cons 'let (list (recur-syms symbols)))
	      (recur-syms-defs symbols definitions)
	      body))))



(install-macro! 'letrec
		 letrec->compound?
		 letrec->compound)


))
