


;;(use-modules (guile) #:select (car) #:prefix foo:)

;; if lambda is too eager

;; if fac is not defined in environment

;; (define fac (lambda (n) (if (< n 2) 1 (* n (fac (- n 1))))))

;; (fac 5)

;; why should this work ?

;; (use-modules (guile) #:select (car) #:prefix guile:)

(define fac (lambda (n) (if (< n 2) 1 (* n (fac (- n 1))))))

fac

(fac 5)



