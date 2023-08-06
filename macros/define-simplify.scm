
;; rewrites
;; (define (f a b c) ...) => (define f (lambda (a b c)...)
;; 
(define define->simplify? (lambda (ex) (and (pair? ex)
					    (eq? (car ex) 'define)
					    (pair? (cdr ex))
					    (pair? (cadr ex)))))


(define define->simplify
  (lambda (ex)
    (append (list 'define (caadr ex)) (list (cons 'lambda (cons (cdadr ex) (cddr ex)))))))




;; (define->simplify? '(define (f a) 1 2 3))
;; ;; => #t

;; (define->simplify '(define (f a) 1 2 3))
;; ;; => (define f (lambda (a) 1 2 3))

;; (define->simplify? '(define (f . args) 1 2 3 args 4 5 6))
;; ;; => #t

;; (define->simplify '(define (f . args) 1 2 3 args 4 5 6))
;; ;; => (define f (lambda args 1 2 3 args 4 5 6))

;; ;;definition of list
;; (define->simplify '(define (lst . args) args))
;; ;; => (define lst (lambda args args))

;; (define->simplify '(define (f . a) 1 2 3))
;; ;; => (define f (lambda a 1 2 3))

;; (define->simplify '(define (f a . b) 1 2 3))
;; ;; => (define f (lambda (a . b) 1 2 3))


