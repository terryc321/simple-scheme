
(define-module (closure)
  #:use-module (list-utility)
  #:export (closure?
	    make-closure
	    closure-lam
	    closure-env
	    closure-args
	    closure-body
	    ))


;; (closure lam env)
(define (closure? x)
  (and (pair? x) (eq? (car x) 'closure)))

(define (make-closure lam env)
  (list 'closure lam env))

(define (closure-lam cloz)
  (second cloz))

(define (closure-env cloz)
  (third cloz))

(define (closure-args cloz)
  (second (closure-lam cloz)))

(define (closure-body cloz)
  (rest (rest (closure-lam cloz))))

