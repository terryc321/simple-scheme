

((lambda ()

(define or->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'or))))

;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define or->compound-helper
  (lambda (ex)
    (if (null? ex)
	#f
	(if (null? (cdr ex))
	    (car ex)
	    (let ((tmp (gensym "tmp")))
	      (list 'let (list (list tmp (car ex) ))
		    (list 'if tmp tmp (or->compound-helper (cdr ex)))))))))

       
;; strip OR , now only dealing with (p1 p2 p3 p4) problems
(define or->compound
  (lambda (ex) 
    (or->compound-helper (cdr ex))))


(install-macro! 'or
		 or->compound?
		 or->compound)


))

;; (or->compound '(or))
;;  ;; #f
;; (or->compound '(or #t))
;;  ;; #t
;; (or->compound '(or #f))
;;  ;; #f
;; (or->compound '(or #t #t))
;;  ;; (let ((tmp7824 #t)) (if tmp7824 tmp7824 #t))
;; (or->compound '(or #t #f))
;;  ;; (let ((tmp7877 #t)) (if tmp7877 tmp7877 #f))
;; (or->compound '(or #t #t))
;;  ;; (let ((tmp7930 #t)) (if tmp7930 tmp7930 #t))
;; (or->compound '(or #f #f))
;;  ;; (let ((tmp7983 #f)) (if tmp7983 tmp7983 #f))
;; (or->compound '(or #f #f #t))
;;  ;; (let ((tmp8037 #f)) (if tmp8037 tmp8037 (let ((tmp8038 #f)) (if tmp8038 tmp8038 #t))))
;; (or->compound '(or #f #f #f))
;;  ;; (let ((tmp8092 #f)) (if tmp8092 tmp8092 (let ((tmp8093 #f)) (if tmp8093 tmp8093 #f))))
;; (or->compound '(or #f #f #f #t))
;;  ;; (let ((tmp8148 #f)) (if tmp8148 tmp8148 (let ((tmp8149 #f)) (if tmp8149 tmp8149 (let ((tmp8150 #f)) (if tmp8150 tmp81
;; 													 50 #t))))))
;; (or->compound '(or #f #f #f #f))
;; ;; (let ((tmp8205 #f)) (if tmp8205 tmp8205 (let ((tmp8206 #f)) (if tmp8206 tmp8206 (let ((tmp8207 #f)) (if tmp8207 tmp8207 #f))))))
