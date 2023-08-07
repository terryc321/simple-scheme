
(define-module (closure-test)
  #:use-module (srfi srfi-64)
  #:use-module (closure))


(test-begin "closure-test")

(let* ((body '(1 2 3 4 5))
       (args '(x y z))
       (lam `(lambda ,args ,@body))
       (env '((a . 1)(b . 2)(c . 3)))
       (cloz (make-closure lam env)))
  (test-equal lam '(lambda (x y z) 1 2 3 4 5))
  (test-equal cloz `(closure ,lam ,env))
  (test-equal env  (closure-env cloz))
  (test-equal lam  (closure-lam cloz))
  (test-equal args (closure-args cloz))
  (test-equal body (closure-body cloz))
  (test-equal cloz `(closure (lambda (x y z) 1 2 3 4 5)
			     ((a . 1)(b . 2)(c . 3)))))


(test-end "closure-test")



