

;; requires (load "macros/substitution-module.scm")
;; requires subst-comma
;; requires subst-splice


;; wrapping code in a lambda prevents namespace pollution
((lambda () 
   
(define (while->compound? ex)
  (and (pair? ex)
       (eq? (car ex) 'while)))


(define (while->compound ex)
  (let* ((tmp-value (gensym "loop"))
	 (condition (cadr ex))
	 (body (cddr ex))
	 (t1 '(letrec ((TMP (lambda ()
			      (if CONDITION
				  (begin
				    BODY
				    (TMP))
				  #f))))
		(TMP)))
	 (s1 (subst-comma t1 'TMP (gensym "loop")))
	 (s2 (subst-comma s1 'CONDITION condition))
	 (s3 (subst-splice s2 'BODY body)))
    s3))


(install-macro! 'while
		 while->compound?
		 while->compound)


))





