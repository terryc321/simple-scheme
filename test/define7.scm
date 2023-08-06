
(define show (lambda ()
		 (display $e)
		 (newline)))

(define p (lambda args args))

(define g (lambda (x) (show) (cons x x)))

(define h (lambda (y) (show) (cons y y)))

(define i (lambda (z) (show) (cons z z)))

(define j (lambda (xx) (show) (cons xx xx)))

$e

(p (h 123) (i 456) (j 789) (g 101112))

$e

;; (cons (g 123) x) ;; fails x undefined
;; yet x is in environment now ?

x

$e




