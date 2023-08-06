
;; call itself  , this time implicit begin

;; Still ?? stack leak in version with - (define eval- ... exp -env -cont )
;; with

;;
;; ./machine < ../test/loop2.scm
;; see sicp model to be 
;;

(define self (lambda (n)
	       (if (= 0 (mod n 100000)) (begin (format #t "\r n = ~a  : stk height = ~a    " n (length $s) ))
		   #f)
	       ;;(newline)(display "env = ") (display $e) (newline)	       
	       (self (+ n 1))))

(self 1)



