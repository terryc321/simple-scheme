

;; if do not save environment when run set! when f is run
;; x y z bound to 1 2 3
;; result then set to x  list (1 2 3)
;; $e shows f's environment now pollutes current environment incorrectly.

(define f (lambda (x y z) (cons x (cons y (cons z '())))))

;;(define x 0)

(set! x (f 1 2 3))

x

y

;; should cause error x not found in environment

;;$e

;;; environment looks like ...
;;; ((x 1 2 3) (y . 2) (z . 3) (f closure (lambda (x y z) (cons x (cons y (cons z (quote ()))))) #(#-5#)) (a . 3)

