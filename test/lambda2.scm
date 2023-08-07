
;; suppose environment not saved between procedure calls


(define f (lambda (x) (list x x)))

(define list (lambda args args))

;; we want this to fail , symbol 'x' not found 
;;(list (f 5) x)

;; if f environment is still active when return to list line-12 x will have value 5

;;

;;(if (f 5) x x)
;; if this outputs 5 then environment is not properly restored after calling f 

(begin 1 2 3 4 (f 5) 6 7 8 9 (f 10)) ;; (10 10)

(f (begin 1 2 3 4 (f 5) 6 7 8 9 (f 10))) ;; ((10 10)(10 10))


((lambda (a . b) b) 1 2 3 4 5) ;; (2 3 4 5)



