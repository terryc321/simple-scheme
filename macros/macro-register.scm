
;; this file is called load-me-first

;; sixth6 = skip 5 then first
;; fourth4 = skip 3 then first
;; check if expression triggers a macro expansion possibility
;; if more than one expander applies , then that is a clash - notify user
(define macro-register
  (letrec ((fourth4 (lambda (xs) (car (cdr (cdr (cdr xs))))))
	   (sixth6 (lambda (xs) (car (cdr (cdr (cdr (cdr (cdr xs))))))))
	   (registry '())
	   (get-registry (lambda () registry))
	   (install-macro! (lambda (name predicate procedure)
			     (set! registry
				   (cons (list (list 'name name)
					       (list 'expand-predicate predicate)
					       (list 'expander procedure))
					 registry))))
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
     (list 'get-registry get-registry))))


(define third caddr)
(define second cadr)
(define first car)


(define install-macro! (second (second macro-register)))
(define macro-registered? (second (first macro-register)))
;;(define registry (cadr (car macro-register)))


  

