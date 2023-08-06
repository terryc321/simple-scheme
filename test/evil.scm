

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

;; ;; first false entry - whole thing is false
;; ;; (and)
;; ;; remember to return to cont continue
;; ;; otherwise we drop out of the interpreter
;; ;; back to lower level
;; (evil (define eval-and
;; 	(lambda (exp env cont)
;; 	  (define eval-and2
;; 	    (lambda (exp env cont)
;; 	      (newline)
;; 	      (display "eval-and2: exp= ")
;; 	      (display exp)
;; 	      (if (null? exp)
;; 		  (cont #t)
;; 		  (base-eval (car exp) env (lambda (r)
;; 					     (if r
;; 						 (eval-and2 (cdr exp) env cont)
;; 						 (cont #f)))))))
;; 	  (eval-and2 (cdr exp) env cont))))


;; (evil (define (base-eval-pair exp env cont)
;; 	(cond
;; 	 ((primitive? exp)        (cont exp))
;; 	 ((closure? exp)          (cont exp))
;; 	 ((macro-closure? exp)    (cont exp))
;; 	 ;; newly installed and expression ...
;; 	 ((eq? (car exp) 'and) (eval-and exp env cont))
	 
;; 	 ;; prim-eval ?
;; 	 ;;((eq? (car exp) 'prim-eval) (prim-eval exp env cont))
;; 	 ((eq? (car exp) 'evil) (cont (eval (second exp) (interaction-environment))))
	 
;; 	 ;; does callcc work ? i dont think so ...
;; 	 ((eq? (car exp) 'callcc)  (eval-callcc          exp env cont))
;; 	 ((eq? (car exp) 'quote)  (eval-quote            exp env cont))
;; 	 ((eq? (car exp) 'define) (eval-define           exp env cont))
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
;; 	 ;; 
;; 	 (#t (eval-application exp env cont)))))



;; ;; also implement tracing
;; (evil (define (base-eval-pair exp env cont)
;; 	(cond
;; 	 ((primitive? exp)        (cont exp))
;; 	 ((closure? exp)          (cont exp))
;; 	 ((macro-closure? exp)    (cont exp))
;; 	 ;; newly installed and expression ...
;; 	 ((eq? (car exp) 'and) (eval-and exp env cont))
	 
;; 	 ;; prim-eval ?
;; 	 ;;((eq? (car exp) 'prim-eval) (prim-eval exp env cont))
;; 	 ((eq? (car exp) 'evil) (cont (eval (second exp) (interaction-environment))))
	 
;; 	 ;; does callcc work ? i dont think so ...
;; 	 ((eq? (car exp) 'callcc)  (eval-callcc          exp env cont))
;; 	 ((eq? (car exp) 'quote)  (eval-quote            exp env cont))
;; 	 ((eq? (car exp) 'define) (eval-define           exp env cont))
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
;; 	 (#t (eval-application exp env (lambda (r)
;; 					 (newline)
;; 					 (display exp)
;; 					 (display "=>")
;; 					 (display r)
;; 					 (cont r)))))))

;; ;; also implement tracing
;; (evil (define (base-eval-pair exp env cont)
;; 	(cond
;; 	 ((primitive? exp)        (cont exp))
;; 	 ((closure? exp)          (cont exp))
;; 	 ((macro-closure? exp)    (cont exp))
;; 	 ;; newly installed and expression ...
;; 	 ((eq? (car exp) 'and) (eval-and exp env cont))
	 
;; 	 ;; prim-eval ?
;; 	 ;;((eq? (car exp) 'prim-eval) (prim-eval exp env cont))
;; 	 ((eq? (car exp) 'evil) (cont (eval (second exp) (interaction-environment))))
	 
;; 	 ;; does callcc work ? i dont think so ...
;; 	 ((eq? (car exp) 'callcc)  (eval-callcc          exp env cont))
;; 	 ((eq? (car exp) 'quote)  (eval-quote            exp env cont))
;; 	 ((eq? (car exp) 'define) (eval-define           exp env cont))
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
;; 	 (#t (eval-application exp env cont)))))

;; ;; (install-handler 'and (define (eval-and exp env cont....)))


