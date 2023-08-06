

;; already f in environment
;; so when define f is called
;; expect x in scope ? no.

;; this falls over
;; using toplevel define
;; (let ((x 5)) (define f (lambda (y) (+ x y))))
;; (f 3)
;; might expect 5 + 3 or 8 

;; why programmable evaluator is useful
;; can just reprogram it to see how its working
;; or why broken...

(evil
(define (eval-application exp env cont)
  (let ((op (car exp))
	(unargs (cdr exp)))
    (base-eval op
	       env
	       (lambda (fun)
		 (cond
		  ((macro-closure? fun)
		   (eval-app-macro fun unargs env cont))
		  ((primitive? fun) ;; evaluate arguments and apply primitive
		   (eval-seq unargs
			     env
			     (lambda (vals)
			       (eval-primitive-op fun vals env cont))))
		  (#t
		   (eval-seq unargs ;; compound procedure with arglist fandangyl
			     env
			     (lambda (vals)
			       (eval-compound-op fun vals env cont))))))))))

;; ------- make new procedure ---------

;; (lambda (expanded-expression)
;; 	 ;;(format #t "\nmacro-expansion : ~a~%" expanded-expression)		  
;; 	 (base-eval expanded-expression env cont)))))
;; instead of base-eval if we just pass
;;(cont expanded-expression) <-- equivalent to macro-expand-1 
;;(format #t "macro mlam : ~a~%" op)

;; (newline)
;; (write "op :")
;; (write op)
;; (newline)

;; (write "cdr.lambda :")
;; (write (cdr op))
;; (newline)

;; op should be a closure


(evil
 (begin

   (define (closure? x)
     (and (pair? x)
	  (eq? (car x) 'closure)))

   (define (make-closure lam env)
     (list 'closure lam env))

   (define (eval-lambda exp env cont)
     (cont (make-closure exp env)))

   (define (closure-lambda lam)
     (second lam))

   (define (closure-environment lam)
     (third lam))

   ;; 
   (define (lambda-args lam)
     (second lam))

   ;; (lambda arglist body....)
   (define (lambda-body lam)
     (rest (rest lam)))

   
 ;; ---------- function application -------------
(define (eval-application exp env cont)
  (let ((op (car exp))
	(unargs (cdr exp)))
    (base-eval op
	       env
	       (lambda (fun)
		 (cond
		  ((macro-closure? fun)
		   (eval-app-macro fun unargs env cont))
		  ((primitive? fun) ;; evaluate arguments and apply primitive
		   (eval-seq unargs
			     env
			     (lambda (vals)
			       (eval-primitive-op fun vals env cont))))
		  (#t
		   (eval-seq unargs ;; compound procedure with arglist fandangyl
			     env
			     (lambda (vals)
			       (eval-compound-op fun vals env cont)))))))))



   
 ;; --------- here eval implicit begin
 ;; eval-begin for expressions without explicit begin at start of exp
(define (eval-implicit-begin exp env result cont)  
    (cond
     ((null? exp) (cont result))
     (#t (base-eval (car exp)
		    env
		    (lambda (result2)
		      (eval-implicit-begin (cdr exp) env result2 cont))))))


 ;; ------------- function arg passing --------
 (define (function-arg-passing cloz vals env cont)
   
  (define (eam-case123 lam-args vals lam-body lam-env cont)
    
    (let* ((bindings (arglist-bind lam-args vals))
	   (new-env (extend-env bindings lam-env))) ;; *** <<=== bug fix !!***

      (format #t "bindings created: ~a~%" bindings)
      (format #t "lambda new env: ~a~%" new-env)
      (format #t "body : ~a~%" lam-body)    
      ;;(format #t "env : ~a~%" (take $env 3)) ;; limit env to 3 entries ...
      
      ;;(cont mlam-body) ;; --------- pass answer as body of macro unevaluated ...
      (eval-implicit-begin  lam-body new-env #f  cont)))
  
  (cond
   ((not (closure? cloz)) ; spew errors
    (format #t "ERROR : ~a is not a closure~%" cloz)
    (error (list "not a closure" cloz)))
   (#t  
    (let* ((lam (closure-lambda cloz))
	   (lam-env (closure-environment cloz))
	   (lam-args (lambda-args lam))
	   (lam-body (lambda-body lam)))
      (eam-case123 lam-args vals lam-body lam-env cont)))))))




;; redirect old eval-compound-op to use our new procedure
(evil
 (define (eval-compound-op op vals env cont)
  (function-arg-passing op vals env cont)))



;; (let ((x 5))
;;   (define f (lambda (y) (+ x y))))
;; (f 3)

((lambda (x) x) 5)


(((lambda (x) (lambda (y) (+ x y))) 3) 5)
;; should be 8


(define f ((lambda (x) (lambda (y) (+ x y))) 5))
(f 3)
