


(define g (lambda ()
	    (cons g g)))


(g)

;; should this be 2 closures cons together?
;;
(eq? (car (g)) (cdr (g)))
;; #t ??

(pair? (g))

