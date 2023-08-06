

;; or
;; or defaults to false

;; or2 ... macro expands 
(defmacro or2 args
  (define rw5 (lambda (e)
		(cond
		 ((null? e) #f)
		 (#t (let ((tmp (gensym "tmp")))
		       `(let ((,tmp ,(car e)))
			  (if ,tmp
			      ,tmp
			      (or2 ,@(cdr e)))))))))
  `',(rw5 args))


(or2)

(or2 #t)

(or2 #f)

(or2 (+ 1 2) (+ 3 4) (+ 5 6))

(or2 #t #f)

(or2 #f #f)

(or2 #t #t)


;; nice use of gensym s to avoid variable capture
(defmacro or args
  (define rw6 (lambda (e)
		(cond
		 ((null? e) #f)
		 (#t (let ((tmp (gensym "tmp")))
		       `(let ((,tmp ,(car e)))
			  (if ,tmp
			      ,tmp
			      (or ,@(cdr e)))))))))
  (rw6 args))

    
(or)

(or #t)

(or #f)

(or #t #f)

(or #f #f)

(or #t #t)


;; this works , but rw6 is liable to be redefined , thereby rendering or3 unstable
(define rw6 (lambda (e)
	      (cond
	       ((null? e) #f)
	       (#t (let ((tmp (gensym "tmp")))
		     `(let ((,tmp ,(car e)))
			(if ,tmp
			    ,tmp
			    (or3 ,@(cdr e)))))))))
(defmacro or3 args
  `',(rw6 args))

(or3)

(or3 #t)

(or3 #f)

(or3 (+ 1 2) (+ 3 4) (+ 5 6))

(or3 #t #f)

(or3 #f #f)

(or3 #t #t)

(define rw6 'broken)

(or #f #f #f #f 1 2 3)

(or)

(or #t)

(or #f)

(or #f #t)

(or #f #f #f #f #t)

(or #f #f #f #f #t #t #t #t #t)


    
