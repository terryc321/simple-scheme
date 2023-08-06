
(define g (lambda (x) (cons x x)))

$e

(g 123)

$e

;; (cons (g 123) x) ;; fails x undefined
;; yet x is in environment now ?

x

$e


