
;; load substitution-module first

((lambda ()

(define and->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'and))))


;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define and->compound-helper
  (lambda (ex)
    (if (null? ex)
	#t
	(if (null? (cdr ex))
	    (car ex)
	    (let ((tmp (gensym "tmp")))
	      (list 'let (list (list tmp (car ex) ))
		    (list 'if tmp tmp (and->compound-helper (cdr ex)))))))))


; strip AND operator , now only dealing with (p1 p2 p3 p4) problems
(define and->compound
  (lambda (ex) 
    (and->compound-helper (cdr ex))))


(install-macro! 'and
		 and->compound?
		 and->compound)



))


