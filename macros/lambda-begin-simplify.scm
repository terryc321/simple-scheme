
;; rewrites
;; (lambda args (begin ...)) =>> (lambda args ...)
;; 
(define lambda-begin->simplify?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'lambda)
	 (pair? (cdr ex))
	 (pair? (cddr ex))
	 (pair? (car (cddr ex)))
	 (eq? (car (car (cddr ex))) 'begin)
	 (null? (cdddr ex)))))



(define lambda-begin->simplify
  (lambda (ex)
    (cons 'lambda (cons (cadr ex) (cdr (car (cddr ex)))))))



;; (define ex '(lambda (a b c) (begin 1 2 3)))

;; (cdr ex)

;; (cddr ex)

;; (cdddr ex)

;; (cdr (car (cddr ex)))

;; (lambda-begin->simplify? '(lambda (a b c) (begin 1 2 3)))

;; (lambda-begin->simplify '(lambda (a b c) (begin 1 2 3)))


