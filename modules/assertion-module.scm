

(define *apple-core*
  (lambda ()
    (list (list 'cons cons)
	  (list 'car car)
	  (list 'cdr cdr))))

(define assert-eq (lambda (x y) (if (eq? x y) #t (error "assert-eq failed"))))

(define assert (lambda (x) (if x #t (error "assert failed"))))

"************ assertion-module loaded ******************"


;; redefine define ... so that can only be redefined once ... almost like defconstant


;; ((lambda ()
;;    (letrec ((old-define define))
;;      (set! define 

;; (define-syntax def
;;   (syntax-rules ()
;;     ((def (f . a) _ )
;;      (list 'args= a 'body= '??? ))))



;; (def (a 1 2 3) 4 5  6 )



;;(error "something catastrophic went wrong...")


