

(define map2 0)
(define map 0)
(set! map (lambda (f xs)
	    (if
	     (null? xs) xs
	     (cons (f (car xs)) (map f (cdr xs))))))
;; (set! map (lambda (f xs)
;; 	    (cond
;; 	     ((null? xs) xs)
;; 	     (#t (cons (f (car xs)) (map f (cdr xs)))))))


(define twice 0)
(set! twice (lambda (x) (+ x x)))


;; first stab at a macro expander
;; macro has to expand to something you can write , how work with gensyms?
;;
;;

(define expand 0)
(define macros '())
(define macro-expand 0)
(set! macro-expand (lambda (exp) exp))


expand
macros
(set! macros (cons 'a macros))
macros


;; (define cond-expander3 0)
;; (define cond-expander2 0)
;; (define cond-expander  0)
;; (set! cond-expander
;;       (lambda (exp)
;; 	(let ...)))


(define let-expander3 0)
(define let-expander2 0)
(define let-expander 0)
;;(let ((x 1 2 3 )(y 4 5 6)(z 7 8 9 ..)) ... body ...) =>>>
;;                                            (begin 1 2 3)(begin 4 5 6)(begin 7 8 9)
(set! let-expander
      (lambda (exp)
	exp))


(let-expander '(let ((x 1)) x))

(let-expander '(let ((x 1)(y 2)(z 3)) (list x y z)))

(let-expander '(let ((x 1 2 3)(y 4 5 6)(z 7 8 9)) (list x y z)))

(append '(1 2 3) '(4 5 6))

(cons 1 '(1 2 3))

(cdr (cons 1 '(1 2 3)))

(car (cons 1 '(1 2 3)))

;; through trial and error 

(map twice (list))

(map twice (list 1))

(map twice (list 1 2))

(map twice (list 1 2 3))

(map twice (list 1 2 3 4))

(map twice (list 1 2 3 4 5))

(map car '((1 2)(3 4)))

(map cdr '((1 2)(3 4)))

(define expr '(let ((x 1 2 3)(y 4 5 6)(z 7 8 9)) (list x y z)(list y z x)(list z x y)))


(map car (car (cdr expr))) ;; (x y z)
(map (lambda (x) (cons 'begin (cdr x))) (car (cdr expr))) ;; ((begin 1 2 3)(begin 4 5 6)(begin 7 8 9))
(cdr (cdr expr)) ;; body  ...(list x y z)(list y z x)(list z x y)...
(append (list 'lambda (map car (car (cdr expr)))) (list (cons 'begin (cdr (cdr expr)))))

(define let-expander (lambda (expr)
		       (append (list (append (list 'lambda (map car (car (cdr expr))))
					     (list (cons 'begin (cdr (cdr expr))))))
			       (map (lambda (x) (cons 'begin (cdr x))) (car (cdr expr))))))

(let-expander '(let ((x 1 2 3)(y 4 5 6)(z 7 8 9)) (list x y z)(list y z x)(list z x y)))

(let-expander '(let ((x 1 2 3)) x))


;; need a cond expander 
;; need a let* expander
;;---------------------------------------------------------------------------------------

;; (define expand 0)
;; (set! expand
;;       (lambda (exp)
;; 	(cond
;; 	 ((null?
	
;; -------------------------------------------------------------------------------------

;; bootstrapping problem

;; cond expander

;; (cond
;;  ((null? xs) 1 2 3)
;;  (#t 4 5 6)
;;  (#f 7 8 9))
;; =>
;; (if (null? xs) (begin 1 2 3)
;;     (if #t (begin 4 5 6)
;; 	(if #f (begin 7 8 9)
;; 	    ???)))


;; (cond
;;  ((null? xs) 1 2 3)
;;  (#t 4 5 6)
;;  (#f 7 8 9)
;;  (else 10 11 12))
;; =>
;; (if (null? xs) (begin 1 2 3)
;;     (if #t (begin 4 5 6)
;; 	(if #f (begin 7 8 9)
;; 	    (begin 10 11 12))))


(define caar (lambda (x) (car (car x))))
(define cdar (lambda (x) (cdr (car x))))

(define f 0)
(define f2 0)
(set! f (lambda (ex) ; strip cond , now only dealing with (X Y1 Y2 Y3 ..) pairs
	  (f2 (cdr ex))))

;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(set! f2 (lambda (ex)
	   (if (null? ex)
	       '()
	       (if (eq? (caar ex) 'else)
		   (cons 'begin (cdar ex))
		   (list 'if (caar ex) (append (list 'begin) (cdar ex)) (f2 (cdr ex)))))))
		 

(f '(cond
     ((null? xs) 1 2 3)
     (#t 4 5 6)
     (#f 7 8 9)
     (else 10 11 12)))


(f '(cond
     ((null? xs) 1 2 3)
     (#t 4 5 6)))


















