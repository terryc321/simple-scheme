

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

