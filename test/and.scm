
;; and macro
;; due to lazy evaluation?
;; all things must be truthy to be true
;; one false element entire thing is false

;; need to fix printing of environment
;; in mit-scheme leads to infinite loop

;; requires cond macro to be loaded first

(defmacro and args
  (define rw3 (lambda (e)
	       (cond
		((null? e) #t)
		(#t `(if ,(car e)
			 (and ,@(cdr e))
			 #f)))))
  `',(rw3 args))
                                                                                                                             
(and)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t)

;; interesting as this didnt seem to work
;; let around defmacro
;; rw4 not defined ---- this doesnt work
(let ((rw4 (lambda (e)
	       (cond
		((null? e) #t)
		(#t `(if ,(car e)
			 (and ,@(cdr e))
			 #f))))))
  (defmacro and args
    `',(rw4 args)))
    
(and)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t)
                                                                                                                    

(defmacro and args
  (define rw5 (lambda (e)
		(cond
		 ((null? e) #t)
		 (#t `(if ,(car e)
			  (and ,@(cdr e))
			  #f)))))
  `',(rw5 args))

    
(and)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t)


;; let surrounding and macro does not work ?
;;
(defmacro and args
  (define rw6 (lambda (e)
		(cond
		 ((null? e) #t)
		 (#t `(if ,(car e)
			  (and ,@(cdr e))
			  #f)))))
  (rw6 args))

    
(and)

(and #t)

(and #f)

(and #t #f)

(and #f #f)

(and #t #t)

