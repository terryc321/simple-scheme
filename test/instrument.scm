
;; instrument.scm
;; instrumenting routines
;;



;; trying understand how underlying eval works with interpreter level on top
;; trying to call down to install some code

(evil (define f (lambda (x) (+ x x))))

(define twice (make-prim (evil f)))

(twice 5)


;; been able to define a function
;; evaluate it in the body of

(evil make-prim)

;; underlying machine has to box primitive procedures
;; to enable them to be distinguished from machine on top's procedures
;;

;; value EM ... underlying machine
(evil (define *tracing* '()))
(evil (define *tracing-all* #f))

(define *tracing* (evil *tracing*))

(define trace-all (lambda () (evil (set! *tracing-all* #t))))
(define trace-none (lambda () (evil (set! *tracing-all* #f))))


;; (trace fib)
;; add fib symbol to *tracing* list
;; (trace)
;; clear the *tracing* list
;; (untrace fib)
;; remove fib from *tracing* list


;; since not really going to do a lot of tracing
;; just cons onto front of *tracing* list
(evil (define eval-trace
	(lambda (exp env cont)
	  (define eval-trace2
	    (lambda (exp env cont)
	      (cond
	       ((null? exp) (cont #t))
	       ((and (pair? exp) (symbol? (car exp)))
		(begin
		  (set! *tracing* (cons (car exp) *tracing*))
		  (eval-trace2 (cdr exp) env cont)))
	       (#t (eval-trace2 (cdr exp) env cont))))) ;;err surely?
	  (cond
	   ((null? (cdr exp)) ;; turn off all tracing as (trace)
	    (set! *tracing* '())
	    (cont '()))
	   (#t
	    (eval-trace2 (cdr exp) env cont))))))


;; trace everything or something
(evil (define (tracing? exp)
	(or *tracing-all*
	    (and (symbol? exp)
		 (member exp *tracing*)))))

;; install handler for trace expressions
(evil (install-handler! 'trace
			(lambda (exp env cont)
			  (and (pair? exp)
			       (eq? (car exp) 'trace)))
			eval-trace))



;; now need to change the eval-application so we can see 
(evil (define (eval-application exp env cont)
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
				     (eval-primitive-op fun vals env
							(lambda (r)
							  (if (tracing? op)
							      (begin
								(newline)
								(pretty `(,op ,@vals))
								(display "=>")
								(pretty r)
								(cont r))
							      (cont r)))))))
			(#t
			 (eval-seq unargs ;; compound procedure with arglist fandangyl
				   env
				   (lambda (vals)
				     (eval-compound-op fun vals env
						       (lambda (r)
							 (if (tracing? op)
							     (begin
							       (newline)
							       (pretty `(,op ,@vals))
							       (display "=>")
							       (pretty r)
							       (cont r))
							     (cont r)))))))))))))


;; usage
;; (trace-all)
;; (load "../test/fibonacci.scm")
;; (fib 10)
;; shows all computations
;; 






;; untrace -- left as an exercise for reader ...
;; (evil (define eval-untrace
;; 	(lambda (exp env cont)
;; 	  (define eval-untrace2
;; 	    (lambda (exp env cont)
	      
;; 	  (eval-untrace2 (cdr exp) env cont))))



;; ;; dynamic variables ... how in lexical scoped lisp ...hmmm...
;; ;; also implement tracing
;; (evil (define (base-eval-pair exp env cont)
;; 	(cond
;; 	 ((primitive? exp)        (cont exp))
;; 	 ((closure? exp)          (cont exp))
;; 	 ((macro-closure? exp)    (cont exp))
;; 	 ;; trace
;; 	 ((eq? (car exp) 'trace) (eval-trace exp env cont))
	 
;; 	 ;; prim-eval ?
;; 	 ;;((eq? (car exp) 'prim-eval) (prim-eval exp env cont))
;; 	 ((eq? (car exp) 'evil) (cont (eval (second exp) (interaction-environment))))
	 
;; 	 ;; does callcc work ? i dont think so ...
;; 	 ((eq? (car exp) 'callcc)  (eval-callcc          exp env cont))
;; 	 ((eq? (car exp) 'quote)  (eval-quote            exp env cont))
;; 	 ((eq? (car exp) 'define) (eval-define           exp env cont))

;; 	 ((and (eq? (car exp) 'set!)
;; 	       (eq? (second exp) 'n))
;; 	  (display "\nvariable N is protected\n")
;; 	  (cont 123))
	 
;; 	 ((eq? (car exp) 'set!)	 (eval-set!             exp env cont))
;; 	 ((eq? (car exp) 'quasiquote)      (eval-quasiquote  exp env cont))	
;; 	 ;; lambda and mlambda code should be 
;; 	 ((eq? (car exp) 'lambda) (eval-lambda exp env cont))
;; 	 ((eq? (car exp) 'mlambda) (eval-mlambda exp  env   cont))
;; 	 ;; 
;; 	 ((eq? (car exp) 'begin)  (eval-begin  exp env cont))
;; 	 ((eq? (car exp) 'if) (eval-if  exp env cont))
;; 	 ((eq? (car exp) 'load)   (eval-load exp env cont))
;; 	 ;; defmacro is just (define some-name (mlambda ...))
;; 	 ((eq? (car exp) 'defmacro) (eval-defmacro       exp  env    cont))
;; 	 ;; spent ages debugging macros only find let was undefined ...
;; 	 ((eq? (car exp) 'let)    (eval-let  exp  env  cont))
;; 	 ;; trace every application
;; 	 (#t (if (and (pair? exp)
;; 		      (symbol? (car exp))
;; 		      (member (car exp) *tracing*))
;; 		 ;; if tracing this expression ... trace it ...
;; 		 (eval-application exp env (lambda (r)
;; 					     (newline)
;; 					     (display exp)
;; 					     (display "=>")
;; 					     (display r)
;; 					     (cont r)))
;; 		 ;; otherwise no tracing ...
;; 		 (eval-application exp env cont))))))



;; ;; (install-handler 'and (define (eval-and exp env cont....)))

;; ;; redefined the meaning of 3 to be 911
;; ;; redefined the meaning of n to be 123

;; (evil (define old-base-eval base-eval))
;; (evil (define (base-eval exp env cont)
;; 	(cond
;; 	 ((and (number? exp) (= exp 3)) (cont 911))  ;; given 3 always return 911
;; 	 ((and (symbol? exp) (eq? exp 'n)) (cont 123)) ;; n always 123 on evaluation ...
;; 	 ((pair? exp)    (base-eval-pair exp env cont))
;; 	 ;; most things evaluate to themselves ....
;; 	 ((null? exp)              (cont exp))
;; 	 ((number? exp)		 (cont exp))
;; 	 ((boolean? exp)		 (cont exp))
;; 	 ((string? exp)		 (cont exp))
;; 	 ((procedure? exp)         (cont exp))
;; 	 ((symbol? exp)		 (eval-var exp env cont))	
;; 	 (#t (cont 'to-be-written-for-base-eval--)))))

;; (list n 3)

;; (evil (set! base-eval old-base-eval))








