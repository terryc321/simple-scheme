
;; need to load cond if and

;; interesting as this didnt seem to work
;; let around defmacro
;; rw4 not defined ---- this doesnt work
;; and is not in scope ... 

;; compiling AND
;; is AND expression a proper list ?
;; if so know how many terms , so no need to test if its the end of a expression
;; 


;; (let ((rw4 (lambda (e)
;; 	       (cond
;; 		((null? e) #t)
;; 		(#t `(if ,(car e)
;; 			 (and ,@(cdr e))
;; 			 #f))))))
;;   (defmacro and args
;;     `',(rw4 args)))

(defmacro and2 args
  (let ((rw4 (lambda (e)
	       (cond
		((null? e) #t)
		((null? (cdr e))
		 (car e))
		(#t `(if ,(car e)
			 (and2 ,@(cdr e))
			 #f))))))
    `',(rw4 args)))


;; <and3> and <and> are going to try to expand to each other
(defmacro and3 args
  (let ((rw4 (lambda (e)
	       (cond
		((null? e) #t)
		((null? (cdr e))
		 (car e))
		(#t `(if ,(car e)
			 (and ,@(cdr e))
			 #f))))))
    (rw4 args)))


(and2)

(and2 #t)

(and2 #f)

(and2 #t #f)

(and2 #f #f)

(and2 #t #t)

(defmacro and args
  (let ((rw4 (lambda (e)
	       (cond
		((null? e) #t)
		((null? (cdr e))
		 (car e))
		(#t `(if ,(car e)
			 (and ,@(cdr e))
			 #f))))))
    (rw4 args)))


(and)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t #f)

(and #t #t #t #t #t #t)

(and #f #t #t #t #t #t)
(and #t #f #t #t #t #t)
(and #t #t #f #t #t #t)
(and #t #t #t #f #t #t)
(and #t #t #t #t #f #t)
(and #t #t #t #t #t #f)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t)




