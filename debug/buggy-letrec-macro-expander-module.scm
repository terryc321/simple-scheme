

(define macro-expander-module
  ((lambda ()

     (letrec ((pp display)
	      (fourth4 (lambda (xs) (car (cdr (cdr (cdr xs))))))
	      (sixth6 (lambda (xs) (car (cdr (cdr (cdr (cdr (cdr xs))))))))
	      (known-macros '())
	      (get-known-macros (lambda () known-macros))
	      (install-macro! (lambda (name predicate procedure)
				(set! known-macros
				      (cons (list (list 'name name)
						  (list 'expand-predicate predicate)
						  (list 'expander procedure))
					    known-macros))))
	      (collect-matches (lambda (expr)
				 (let ((matches '()))
				   (letrec ((peek (lambda (xs)
						    (cond
						     ((null? xs) matches)
						     (#t (let ((pred (fourth4 (car xs))))
							   (if (pred expr)
							       (set! matches (cons (car xs) matches))
							       #f))
							 (peek (cdr xs)))))))
				     (peek macro-register))
				   (if (null? (cdr matches))
				       (car matches)
				       (begin
					 (format #t "macro expander multiple matches for expression~%")
					 (format #t "expr = ~a~%" expr)
					 (format #t "matches = ~A ~%" matches)
					 (error (multiple-matches-on-macro-expansion)))))))
	      (macro-registered? (lambda (expr)
				   (collect-matches expr)))	   
	      (expand-elems
	       (lambda (xs)
		 (cond
		  ((null? xs) '())
		  ((and (pair? xs)   ;; ------- improper lists cause upset??
			(not (null? (cdr xs)))
			(not (pair? (cdr xs))))
		   (cons (expand (car xs))
			 (cdr xs)))	   
		  (#t (cons (expand (car xs))
			    (expand-elems (cdr xs)))))))

	      (expand
	       (lambda (ex)
		 (cond
		  ;; ((not (pair? ex)) ex) ;; no macro expand symbols yet
		  ;; ((and (pair? ex) (eq? (car ex) 'quote)) ex) ;; leave quoted alone...?
		  ;; ;; quasi-quote ? unquote ? unquote-splicing ...
		  ;; ((begin->simplify? ex) (begin->simplify ex))
		  ;; ((lambda-begin->simplify? ex) (lambda-begin->simplify ex))	 
		  ;; ((define->simplify? ex) (define->simplify ex))	 
		  ;; ((if->simplify? ex) (if->simplify ex))
		  ;; ((cond->compound? ex) (cond->compound ex))
		  ;; ((let-procedure->compound? ex)  (let-procedure->compound ex))
		  ;; ((let->compound? ex)  (let->compound ex))
		  ;; ((let*->compound? ex) (let*->compound ex))
		  ;; ((letrec->compound? ex) (letrec->compound ex))
		  ;; ((and->compound? ex)   (and->compound ex))
		  ;; ((or->compound? ex)    (or->compound ex))
		  ;; ((qq->compound? ex)    (qq->compound ex))
		  (#t (expand-elems ex)))))

	      (expand-derived
	       (lambda (ex)
		 (format #t "expansion => ")
		 (newline)
		 (pp ex)
		 (newline)

		 (let ((expand-1 (expand ex)))
		   (if (equal? ex expand-1)
		       (begin
			 (format #t "final expansion => ")
			 (newline)
			 (pp ex)
			 (newline)	    
			 ex)
		       (expand-derived expand-1)))))

	      (list
	       (list 'expand-derived expand-derived)
	       (list 'macro-registered? macro-registered?)
	       (list 'install-macro! install-macro!)
	       (list 'get-known-macros get-known-macros)
	       ))))))

 
(define expand-derived (alist-lookup 'expand-derived macro-expander-module))
(define install-macro! (alist-lookup 'install-macro! macro-expander-module ))
(define macro? (alist-lookup 'macro-registered? macro-expander-module ))
(define known-macros (alist-lookup 'known-macros macro-expander-module ))


(assert (procedure? expand-derived))
(assert (procedure? install-macro!))
(assert (procedure? macro?))
(assert (procedure? known-macros))

(define macro-expander-module-loaded #t)

