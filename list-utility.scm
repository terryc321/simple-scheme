
(define-module (list-utility)
  #:export (first
	    second
	    third
	    fourth
	    fifth
	    sixth
	    seventh
	    eighth
	    nineth
	    tenth
	    rest))

(define (elem n xs)
  (cond
   ((null? xs) (error "elem xs empty"))
   ((not (pair? xs)) (error "elem xs not pair"))    
   ((= n 1) (car xs))
   (#t (elem (- n 1) (cdr xs)))))

(define (rest xs)   (cdr xs))
(define (first xs)  (car xs))
(define (second xs) (elem 2 xs))
(define (third xs)  (elem 3 xs))
(define (fourth xs) (elem 4 xs))
(define (fifth xs)  (elem 5 xs))
(define (sixth xs)  (elem 6 xs))
(define (seventh xs) (elem 7 xs))
(define (eighth xs)  (elem 8 xs))
(define (nineth xs) (elem 9 xs))
(define (tenth xs)  (elem 10 xs))

