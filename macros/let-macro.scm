
;; test cases
;;
;; another form of let
;;
;; (let name ... ...body...)
;;

;; (let ((p a b c)...) ...)
(define let->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'let)
	 (pair? (cdr ex))
	 (pair? (car (cdr ex))))))

(define let->compound (lambda (expr)
			(append (list (append (list 'lambda (map car (car (cdr expr))))
					      (list (cons 'begin (cdr (cdr expr))))))
				(map (lambda (x) (cons 'begin (cdr x))) (car (cdr expr))))))

;; (let->compound '(let ((x 1 2 3)(y 4 5 6)(z 7 8 9)) (list x y z)(list y z x)(list z x y)))
;; ;; ((lambda (x y z) (begin (list x y z) (list y z x) (list z x y))) (begin 1 2 3) (begin 4 5 6) (begin 7 8 9))

;; (let->compound '(let ((x 1 2 3)) x))
;; ;; ((lambda (x) (begin x)) (begin 1 2 3))



