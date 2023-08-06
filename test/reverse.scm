



;; (define reverse2 0)
;; (define reverse 0)
;; (set! reverse (lambda (xs)
;; 		(if
;; 		 (null? xs) xs
;; 		 (reverse2 xs '()))))
;; (set! reverse2 (lambda (xs ys)
;; 		 (if
;; 		  (null? xs) ys
;; 		  (reverse2 (cdr xs) (cons (car xs) ys)))))



(define reverse
  (lambda (xs)
    (define reverse2
      (lambda (xs ys)
	(if
	 (null? xs) ys
	 (reverse2 (cdr xs) (cons (car xs) ys)))))
    (if
     (null? xs) xs
     (reverse2 xs '()))))



;; awaiting macro expander
;; (set! reverse (lambda (xs)
;; 	    (cond
;; 	     ((null? xs) xs)
;; 	     (#t (cons (car xs) (reverse (cdr xs)))))))


(reverse (list))
(reverse (list 1))
(reverse (list 1 2))
(reverse (list 1 2 3))     
(reverse (list 1 2 3 4 5 6 7 8 9 10))

(reverse (list 'a 'b 'c 'd 'e 'f 'g))

(reverse `(a1 b2 c3 d4 e5 f6 g7))






