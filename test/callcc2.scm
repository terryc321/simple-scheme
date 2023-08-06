


(callcc (lambda (r) 3))

(callcc (lambda (r) r))

(callcc (lambda (r) (r 4)))

(define f (lambda (return)
	    (return 2)
	    3))

(f (lambda (x) x));; => 3

;;(call-with-current-continuation f) ==> 2
(callcc f)

10
20
30

;; callcc one arg fn
;;

(define control #f)
(define next #f)
(define current #f)
(define traverse (lambda (xs)
		   (if (null? xs) "you fell off the end"
		       (begin
			 (format #t "item = ~a~%" (car xs))
			 (callcc (lambda (r)
				   (set! current (lambda () (r (traverse xs))))
				   (set! next (lambda () (r (traverse (cdr xs)))))))
			 ))))

		       ;; ;;(format #t "env = ~a~%" $e)
		       ;; (control (car xs))
		       ;; (traverse (cdr xs))))))))



;; control

(traverse `(1 2 3 4 5))

(next)

(next)

(next)

(next)

(traverse `(1 2 3 4 5))

(next)

(current)

(next)

(current)


;; control

;; (control #t)


;; $e

;; (traverse '())



"check"

"*************** done ******************"


