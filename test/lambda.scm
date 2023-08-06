

;;guile -e '(begin (load "machine.scm")(run))' < ../test/lambda.scm

(lambda (x) x)

((lambda (x) x) 3)

((lambda (x) (lambda (y) (+ x y))) 3)

(((lambda (x) (lambda (y) (+ x y))) 3) 4)

((((lambda (x) (lambda (y) (lambda (z) (+ x (+ y z))))) 3) 4) 5)

" sum of 3 4 5 to lambdas x y z "

((((lambda (x)
     (display "lam x = ") (display x)(newline)
     (display "lam.x.env $e = ") (display $e)(newline)
     (lambda (y)
       (display "lam y = ") (display y)(newline)
       (display "lam.y.env $e = ") (display $e)(newline)
       (define fac (lambda (n)
	      (display "fac n = ") (display n)(newline)
	      (display "env = ") (display $e)(newline)
	      (if (< n 2)
		  1
		  (* n (fac (- n 1))))))
        
       ((lambda (n) (newline) (display "fac 10 = ") (display n)(newline)) (fac 10))
       (lambda (z)
	 (display "lam z = ") (display y)(newline)
	 (display "lam.z.env $e = ") (display $e)(newline)            
	 (+ x (+ y z)))))
   3) 4) 5)

"check environment definitions"
$e



;; define f x

(define f (lambda (x) x))

(f 3)

;; recursive definition , when lambda formed - technically fac is not in scope


;; 
;; (define fac (lambda (n)
;; 	      (if (< n 2)
;; 		  1
;; 		  (* n (fac (- n 1))))))
;; (fac 10)


;; recursive problem solved 
(define fac (lambda (n)
	      (display "fac n = ") (display n)(newline)
	      (display "env = ") (display $e)(newline)
	      (if (< n 2)
		  1
		  (* n (fac (- n 1))))))
(fac 10)
(fac 100)

"check environment has not expanded beyond belief"
$e



(define fib (lambda (n)
	      (if (< n 3)
		  1
		  (+ (fib (- n 1)) (fib (- n 2))))))
(fib 5)
(fib 10)


;; argument passing
(define f (lambda () #t))
(f)

(define g1a (lambda (a) a))
(g1a 1)

(define g2a (lambda (a b) a))
(define g2b (lambda (a b) b))
(g2a 1 2)
(g2b 1 2)


(define g3a (lambda (a b c) a))
(define g3b (lambda (a b c) b))
(define g3c (lambda (a b c) c))
(g3a 1 2 3)
(g3b 1 2 3)
(g3c 1 2 3)


(define g4a (lambda (a b c d) a))
(define g4b (lambda (a b c d) b))
(define g4c (lambda (a b c d) c))
(define g4d (lambda (a b c d) d))
(g4a 1 2 3 4)
(g4b 1 2 3 4)
(g4c 1 2 3 4)
(g4d 1 2 3 4)

(define g5a (lambda (a b c d e) a))
(define g5b (lambda (a b c d e) b))
(define g5c (lambda (a b c d e) c))
(define g5d (lambda (a b c d e) d))
(define g5e (lambda (a b c d e) e))
(g5a 1 2 3 4 5)
(g5b 1 2 3 4 5)
(g5c 1 2 3 4 5)
(g5d 1 2 3 4 5)
(g5e 1 2 3 4 5)

(define g6a (lambda (a b c d e f) a))
(define g6b (lambda (a b c d e f) b))
(define g6c (lambda (a b c d e f) c))
(define g6d (lambda (a b c d e f) d))
(define g6e (lambda (a b c d e f) e))
(define g6f (lambda (a b c d e f) f))

(g6a 1 2 3 4 5 6)
(g6b 1 2 3 4 5 6)
(g6c 1 2 3 4 5 6)
(g6d 1 2 3 4 5 6)
(g6e 1 2 3 4 5 6)
(g6f 1 2 3 4 5 6)


(define g7a (lambda (a b c d e f g) a))
(define g7b (lambda (a b c d e f g) b))
(define g7c (lambda (a b c d e f g) c))
(define g7d (lambda (a b c d e f g) d))
(define g7e (lambda (a b c d e f g) e))
(define g7f (lambda (a b c d e f g) f))
(define g7g (lambda (a b c d e f g) g))
(g7a 1 2 3 4 5 6 7)
(g7b 1 2 3 4 5 6 7)
(g7c 1 2 3 4 5 6 7)
(g7d 1 2 3 4 5 6 7)
(g7e 1 2 3 4 5 6 7)
(g7f 1 2 3 4 5 6 7)
(g7g 1 2 3 4 5 6 7)

;; immediate lambda , close over environment not yet ready
;; 


;; is-odd? is not defined when is-even? is defined
(define is-even? (lambda (n)
		   (if (= n 0)
		       #t
		       (is-odd? (- n 1)))))

(define is-odd? (lambda (n)
		  (if (= n 0)
		      #f
		      (is-even? (- n 1)))))

"check is-even? is-odd? is performing correctly"

(is-even? 10)

(is-odd? 9)

(is-even? 33)

(is-odd? 2)


;; setting the definitions should make no difference

(set! is-even? (lambda (n)
		   (if (= n 0)
		       #t
		       (is-odd? (- n 1)))))
(set! is-odd? (lambda (n)
		  (if (= n 0)
		      #f
		      (is-even? (- n 1)))))

"again after is-even? is-odd? have been set! should be identical"

(is-even? 10)

(is-odd? 9)

(is-even? 33)

(is-odd? 2)

(define a 1)

a

(set! a 2)

a

(define twice (lambda (x) (+ x x)))

;;(map twice '(1 2 3 4 5))

;;(set! dummy 0)

(twice 10)

(twice (twice 10))

(twice (twice (twice 10)))

(twice (twice (twice (twice 10))))

"************* all done ******* "

