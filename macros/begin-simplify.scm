
;; simplifier removes excess begin statements
;; (begin a) => a
(define begin->simplify? (lambda (ex) (and (pair? ex)
					  (eq? (car ex) 'begin)
					  (pair? (cdr ex))
					  (null? (cdr (cdr ex))))))

;; simply the 2nd item of list
(define begin->simplify cadr)
;;(define begin->simplify (lambda (ex) (cadr ex)));;

;; (begin           a b c d (+ 1 1) (cons 3 4) y) ==> y
;; if all these     <--------------------------> have no side effects
;; can simply ignore them and just do y itself only.

