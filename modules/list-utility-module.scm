

(define list-utility-module
  ((lambda ()
     
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

     (list
      (list 'first first)
      (list 'rest rest)      
      (list 'second second)
      (list 'third third)
      (list 'fourth fourth)
      (list 'fifth fifth)
      (list 'sixth sixth)
      (list 'seventh seventh)
      (list 'eighth eighth)
      (list 'nineth nineth)
      (list 'tenth tenth))
     )))


(define first (alist-lookup 'first list-utility-module ))
(define rest (alist-lookup 'rest list-utility-module ))
(define second (alist-lookup  'second list-utility-module ))
(define third (alist-lookup  'third list-utility-module ))
(define fourth (alist-lookup  'fourth list-utility-module ))
(define fifth (alist-lookup  'fifth list-utility-module ))
(define sixth (alist-lookup  'sixth list-utility-module ))
(define seventh (alist-lookup  'seventh list-utility-module ))
(define eighth (alist-lookup  'eighth list-utility-module ))
(define nineth (alist-lookup  'nineth list-utility-module ))
(define tenth (alist-lookup  'tenth list-utility-module ))

(assert-eq 'a (first (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j )))
(assert-eq 'b (second (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'c (third (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'd (fourth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'e (fifth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'f (sixth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'g (seventh (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'h (eighth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'i (nineth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))
(assert-eq 'j (tenth (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))

(assert (equal? (rest (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j))
		(list 'b 'c 'd 'e 'f 'g 'h 'i 'j)))

(define list-utility-module-loaded #t)


