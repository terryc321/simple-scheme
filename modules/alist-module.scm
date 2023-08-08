

;; alist-module

;; (letrec ((alist-member? (lambda (
;; given a list
;;
;; ( (key value) (key value) ... )
;; proper list
;;


(define alist-member?
  (lambda (sym xs)
    (filter (lambda (elem)
	      (eq? (car elem) sym))
	    xs)))

(define alist-lookup
  (lambda (sym xs)
    (cadr (car (alist-member? sym xs)))))


;;(alist-member? 'a '((a 2)(a 3)(a 4)(b 1)(b 2)(b 3)))

(define alist-module-loaded #t)



