

;; sixth6 = skip 5 then first
;; fourth4 = skip 3 then first
;; check if expression triggers a macro expansion possibility
;; if more than one expander applies , then that is a clash - notify user
(define macro-register
  (letrec ((fourth4 (lambda (xs) (car (cdr (cdr (cdr xs))))))
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
				(collect-matches expr))))
    (list
     (list 'macro-registered? macro-registered?)
     (list 'install-macro! install-macro!)
     (list 'get-known-macros get-known-macros))))


(define third caddr)
(define second cadr)
(define first car)


(define install-macro! (second (second macro-register)))
(define macro-registered? (second (first macro-register)))
(define known-macros (second (third macro-register)))

;;(define registry (cadr (car macro-register)))


  

