

;; quasi-quote

($qq a) ;; `a a

($qq (a b c)) ;; `(a b c) => (a b c)

($qq (a ($qs (list 'b 'c)) ($qc 'd))) ;; `(a ,@(list 'b 'c) ,d) 

($qq (a ($qs (list 1 2)) ($qc (list 3 4)))) ;; (a 1 2 (3 4))

(let ((tmp (gensym "loop"))
      (tmp2 (gensym "terry")))
  (list tmp tmp2))


;; while macro


;;$qq instead of ` or quasiquote
;;$qc instead of , or unquote
;;$qs instead of ,@ or unquote-splicing

(define (while condition body)
  (let ((tmp (gensym "loop")))
    ($qq (letrec ((($qc tmp) (lambda ()
		      (if ($qc condition)
			  (begin
			    ($qs body)
			    (($qc tmp)))))))
	   (($qc tmp))))))


;; ;; loop n 1 thru 10
;; (let ((n 1))
;;   (while '(< n 10) '((begin (format #t "n = ~A~%" n)
;; 			    (set! n (+ n 1))))))


;; (define while (lambda (condition . body)
;; 		body))



  ;; (let ((tmp (gensym "loop")))
  ;;   ($qq (letrec ((($qc tmp) (lambda ()
  ;; 		      (if ($qc condition)
  ;; 			  (begin
  ;; 			    ($qs body)
  ;; 			    (($qc tmp)))))))
  ;; 	   (($qc tmp))))))


;; loop n 1 thru 10
;; (let ((n 1))
;;   (while '(< n 10) '(begin (format #t "n = ~A~%" n)
;; 			   (set! n (+ n 1)))))

;;(while 1 2 3 )


;;(while '((lambda (x) x)) '((lambda (y) y)))

;; "check"

;; ($qq (a b c))

;; "******************** done ****************************"


(while #t '(begin 1 2 3))

