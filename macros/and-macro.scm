

(define and->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'and))))


;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define and->compound-helper
  (lambda (ex)
    (if (null? ex)
	#t
	(if (null? (cdr ex))
	    (car ex)
	    (let ((tmp (gensym "tmp")))
	      (list 'let (list (list tmp (car ex) ))
		    (list 'if tmp tmp (and->compound-helper (cdr ex)))))))))


; strip AND operator , now only dealing with (p1 p2 p3 p4) problems
(define and->compound
  (lambda (ex) 
    (and->compound-helper (cdr ex))))


;; test suite ?
;; and expands into let 

;; (and->compound '(and))
;;  ;; #t
;; (and->compound '(and #t))
;;  ;; #t
;; (and->compound '(and #f))
;;  ;; #f
;; (and->compound '(and #t #t))
;;  ;; (let ((tmp6995 #t)) (if tmp6995 tmp6995 #t))
;; (and->compound '(and #t #f))
;;  ;; (let ((tmp7048 #t)) (if tmp7048 tmp7048 #f))
;; (and->compound '(and #t #t))
;;  ;; (let ((tmp7101 #t)) (if tmp7101 tmp7101 #t))
;; (and->compound '(and #f #f))
;;  ;; (let ((tmp7154 #f)) (if tmp7154 tmp7154 #f))
;; (and->compound '(and #f #f #t))
;;  ;; (let ((tmp7208 #f)) (if tmp7208 tmp7208 (let ((tmp7209 #f)) (if tmp7209 tmp7209 #t))))
;; (and->compound '(and #f #f #f))
;;  ;; (let ((tmp7263 #f)) (if tmp7263 tmp7263 (let ((tmp7264 #f)) (if tmp7264 tmp7264 #f))))
;; (and->compound '(and #f #f #f #t))
;;  ;; (let ((tmp7319 #f)) (if tmp7319 tmp7319 (let ((tmp7320 #f)) (if tmp7320 tmp7320 (let ((tmp7321 #f)) (if tmp7321 tmp73
;; 													 21 #t))))))
;; (and->compound '(and #f #f #f #f))
;;  ;;(let ((tmp7376 #f)) (if tmp7376 tmp7376 (let ((tmp7377 #f)) (if tmp7377 tmp7377 (let ((tmp7378 #f)) (if tmp7378 tmp7378 #f))))))


