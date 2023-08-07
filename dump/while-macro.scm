

;; recursively substitute sym in expression for val
;; this substitution can be done in any order
;; can compose
;; can this cope with improper pairs ? vectors? hash-tables?
(define (subst-comma ex sym val)
  (define (recur x)
    (cond
     ((not (pair? x)) (if (eq? x sym)
			  val
			  x))     
     (#t (cons (recur (car x))
	       (recur (cdr x))))))
  (recur ex))


(define (subst-splice ex sym val)
  ;; 
  (define (f x)  
    (cond
     ((not (pair? x)) x)
     ((eq? (car x) sym)
      (append val (f (cdr x))))
     (#t (cons (f (car x))
	       (f (cdr x))))))
  ;; 
  (define (g x)  
    (cond
     ((eq? x sym) val)
     (#t (f x))))
  
  ;; just call g
  (g ex))





;; (define while->compound?
;;   (lambda (ex)
;;     (and (pair? ex)
;; 	 (eq? (car ex) 'while))))

;; (define while->compound ...)
  
;;   (lambda (ex)
;;     (if (null? ex)
;; 	''()
;; 	(if (eq? (caar ex) 'else)
;; 	    (cons 'begin (cdar ex))
;; 	    (list 'if (caar ex) (append (list 'begin) (cdar ex)) (cond-helper (cdr ex)))))))


;; (while (< n 10) (format #t "n = ~a~%" n) #t)

;; =>>
;; (let ((tmp (gensym "loop")))
;; `(letrec ((,tmp (lambda ()
;; 		  (if ,condition
;; 		      (begin
;; 			,@body
;; 			(,tmp))
;; 		      #f))))
;;    (,tmp)))

;; >>>>>>>>>>>>>>>>
;; lets focus on the quasi-quoted expression
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; `(letrec ((,tmp (lambda ()
;; 		  (if ,condition
;; 		      (begin
;; 			,@body
;; 			(,tmp))
;; 		      #f))))
;; (,tmp))
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; lets now isolate LOCATION of substitution
;; with the MANNER of substitution
;; ,tmp  where see ,tmp want to COMMA substitute
;; ,@body where see ,@body want to SPLICE substitute
;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; TMP : COMMA substitute       : value of                  : EVAL of (gensym "loop")
;; BODY : SPLICE substitute     : value of the list 3 items : (< n 10)
;; CONDITION : COMMA substitute : value of the list 2 items : ((format #t "n = ~a~%" n) #t)

;; for ease of use the substitution happens only on SYMBOLS

;; (define template '(letrec ((TMP (lambda ()
;; 		  (if CONDITION
;; 		      (begin
;; 			BODY
;; 			(TMP))
;; 		      #f))))
;; 		    (TMP)))

(define (atomic? ex)
  (or (symbol? ex) (number? ex)))

(define (improper-pair? ex)
  (and (pair? ex)(atomic? (cdr ex))))


;; (append (list a) (list b) BODY (list d))
;; >>> should flatten it all down...
;;
;;
;; list (a b BODY d) , splice into BODY another list (g h i)
;; to make (a b g h i d) as result
;;
;; if not a list just itself ?
;; ;; 
;; (define (subst-splice ex sym val)
;;   (define (recur x)
;;     (format #t "subst-splice:recur:x=~a~%" x)
;;     (cond
;;      ((null? x) '())
;;      ((pair? x) (apply append (cons (recur (car x))
;; 				    (recur (cdr x)))))
;;      ((eq? ex sym) val)
;;      (#t (list x))))
;;   ;; recurse down expression
;;   (recur ex))



(define template '(letrec ((TMP (lambda ()
				  (if CONDITION
				      (begin
					BODY
					(TMP))
				      #f))))
		    (TMP)))

template

"***subst ***" 
(subst-comma template 'TMP (gensym "loop"))

"***done subst"



;; (define (compose . exprs)
;;   ...)


;;(compose ...)
;;
;;(while (< n 10) (format #t "n = ~a~%" n) #t)

;; (define (cadr x) (car (cdr x)))
;; (define (cddr x) (cdr (cdr x)))

;; improper lists
;; map ?
 


(define (while->compound ex)
  (let* ((tmp-value (gensym "loop"))
	 (condition (cadr ex))
	 (body (cddr ex))
	 (t1 '(letrec ((TMP (lambda ()
			      (if CONDITION
				  (begin
				    BODY
				    (TMP))
				  #f))))
		(TMP)))
	 (s1 (subst-comma t1 'TMP (gensym "loop")))
	 (s2 (subst-comma s1 'CONDITION condition))
	 (s3 (subst-splice s2 'BODY body)))
    s3))


(define p1 (while->compound '(while (< n 10) (format #t "n = ~a~%" n) #t)))

p1



;; (let ((body '((format #t "n = ~a~%" n) #t)))
;;   (subst-splice p1 'BODY body))



;; >>>>>>>> flat list contains word BODY replace with 1 2 3
;; this is called splicing

;; (define p1 (list 'a 'b 'BODY 'c 'd))

;; ;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;; ;;            { A, *->{ B,*->{ BODY ,*-> {C , *-> {D , XXX NIL XXX }
;; (define (f x sym val)  
;;   (cond
;;    ((not (pair? x)) x)
;;    ((eq? (car x) sym)
;;     (append val (f (cdr x) sym val)))
;;    (#t (cons (f (car x) sym val)
;; 	     (f (cdr x) sym val)))))

;; (define (g x sym val)  
;;   (cond
;;    ((eq? x sym) val)
;;    (#t (f x sym val))))
;;
;;
;;-------- this is a winner f , g combination , i like it


;; (g p1 'BODY '(1 2 3)) ;; expect (a b 1 2 3 c d)

;; ;; if thing being substituted is the thing itself , just the substitution.
;; (g 'BODY 'BODY '(1 2 3)) ;; expect (1 2 3)

;; (define (subst-splice ex sym val)
;;   ;; 
;;   (define (f x)  
;;     (cond
;;      ((not (pair? x)) x)
;;      ((eq? (car x) sym)
;;       (append val (f (cdr x))))
;;      (#t (cons (f (car x))
;; 	       (f (cdr x))))))
;;   ;; 
;;   (define (g x)  
;;     (cond
;;      ((eq? x sym) val)
;;      (#t (f x))))
  
;;   ;; just call g
;;   (g ex sym val))



























   
