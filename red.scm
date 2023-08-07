
;; red.scm

;; toplevel define actually changes environment
;; ----- letrec ------
;; internal define should really be a letrec
;; so after leave function , those definitions are no longer in scope
;; -------- macros ---------
;; what is the value of {begin} or {define} or {lambda} , actual word ?
;; these are just control words , eg. {let} {let*} {letrec} ...
;; do they have a value?
;; can be rewritten to lambda but lose debugging ...
;; --------------------
;; what if set-car! set-cdr! was not available on a functional machine perhaps
;; like ability to go forward and backward through a computation
;; if an error occurred , unwind computation see where it went wrong and why
;; since its all functional
;; it should be more reliable ? easier to debug ?
;;
;; no side effects
;; what is purpose of a sequence of operations then
;; just do the last thing
;; or not at all ?
;;

;;------------------------------------------------------------------
;; defmacro in terms of define-syntax

;; (define-syntax rl 
;;   (syntax-rules ()
;;     ((rl) (begin (load "red.scm")
;; 		 (black 'a
;; 			'(set! a 4)
;; 			'a
;; 			'(set! a 5)
;; 			'a
;; 			)))))

;; evil -- allows us to evaluate in underlying machine instructions
;; think instead of cont
;; pass fail may be better
;; for error recovery ??

(define-syntax rk
  (syntax-rules ()
    ((rl) (begin (load "red.scm")
		 (apply black `(
				(load "../test/define.scm")			
				))))))




(define-syntax rl 
  (syntax-rules ()
    ((rl) (begin (load "red.scm")
		 (apply black `(

				;;(load "../test/if.scm")				
				;;(load "../test/cond.scm")
				(load "../test/factorial.scm")
				(load "../test/fibonacci.scm")
				;; used evil to install eval-and 
				(load "../test/and.scm")
				(load "../test/or.scm")
				(load "../test/swap.scm")
				(load "../test/let.scm")
				(load "../test/errors.scm")
				;;(load "../test/callcc.scm")
				;;(load "../test/evil.scm")
				;; tracing ... (trace fib)
				(load "../test/tracing.scm")
				;; instrumentation ...
				;;
				;;(load "../test/and.bug1.scm")
				;;(load "../test/and.bug2.scm")
				
				;; 
				(set! a 4)
				a
				b
				c
				(set! a 5)
				a
				))))))



(define error (lambda (x) (write "HALT (straight-use-package 'paredit)")(write x)))
(define exit (lambda (x) (write "DONE") (write x)))
;; display throws away string quotes
;; write leaves string quotes in 

(define (tag x y)
  (cons x y))


(define rest cdr)
(define first car)
(define (second x) (car (cdr x))) ;; 1 cdr
(define (third x) (car (cdr (cdr x)))) ;; 2 cdrs
(define (fourth x) (car (cdr (cdr (cdr x))))) ;; 3cdrs
;;#lang racket
;;(require rnrs/mutable-pairs-6)


(define (primitive? op)
  (and (pair? op)
       (eq? (car op) 'primitive)
       (procedure? (cdr op))))



;; eval-begin for expressions without explicit begin at start of exp
;;---------------- error ----------------------------
(define (eval-implicit-begin exp env result cont)  
    (cond
     ((null? exp) (cont result))
     (#t (base-eval (car exp)
		    env
		    (lambda (result2)
		      (eval-implicit-begin (cdr exp) env result2 cont))))))
  
;; exp = (begin ...)
(define (eval-begin exp env cont)
  ;; strip begin ... 
  (eval-implicit-begin (cdr exp) env #f cont))



;; ---------- evaluate sequence ---------------
(define (eval-seq xs env cont)
  (define (eval-seq2 xs ys env cont)
    (cond
     ((null? xs) (cont (reverse ys)))
     (#t (base-eval (car xs)
		    env
		    (lambda (result)
		      (eval-seq2 (cdr xs) (cons result ys) env cont))))))
    (eval-seq2 xs '() env cont))



;; main problem is pretty printing the output
;; first lets see if we can
;; '<closure>
;; '<mclosure>
;; 
;; 
;;

;; (set! cons mcons)
;; (set! cdr mcdr)
;; (set! car mcar)
;; (set! set-cdr! set-mcdr!)
;; (set! set-car! set-mcar!)
;; (set! list mlist)

;; interplay between interpreter language want to write
;; and underlying language
;;  racket forbids redeclare of cons car cdr set-car! set! set-cdr!
;;  in interests of efficiency?
;;


;; blue.rkt
;; requires language selected as racket - dont know what this means 
;;

;; ------------------------------------------------------------------
;; mit-scheme specific stuff
;; common lisp format -- like print hello world 
;; ------------------------------------------------------------------
;;
;; mit-scheme has load-option to load some extra functionality
;;(load-option 'format)
;;
;;



  
;; ;;
;; ;; -----------------------------------------------------------------
;; (defmacro rl ()
;;   `(load "macro.scm"))

;; (defmacro rb ()
;;   `(begin (rl) (black)))

;; (defmacro rk ()
;;   `(begin (rl) (black ;;'(load "../test/factorial.scm")
;; 		      '(load "../test/quasi.scm")
;; 		      ;;(load "../test/cond.scm")		      ;;(load "../test/swap2.scm")
;; 	      		      )))

;; (defmacro ri ()
;;   `(begin (rl) (black '(load "../test/identity.scm"))))
;; ;;-------------------------------------------------------------------


;; bug : redefining routine later on in program , deletes one looking at and causes spurious behaviour
;; layers :: symbol to be locked , unabled to be redefined

;; (define (cadr x) (car (cdr x)))
;; (define (caar x) (car (car x)))
;; (define (cdar x) (cdr (car x)))
;; (define (caddr x) (car (cdr (cdr x))))
;; (define (cadddr x) (car (cdr (cdr (cdr x)))))


;; (define (first xs) (car xs))
;; (define (second xs) (car (cdr xs)))
;; (define (third xs) (car (cdr (cdr xs))))

(define (rest-first xs) (cdr (car xs)))
(define (first-first xs) (car (car xs)))
;;(if rest #f ...) ;; but this will fail if rest is not defined ? why? 


;; again redefining implementation of environment
;; (<environment> . env-list)
;;                    each entry is a cons pair
;;                                    key . value
;;                     cdr env-list is rest environment chain
;;                     car env-list is first key . value pair
;;

;;(define *the-primitives* (list))
(define $env #f)

;; how do i build a debugger ?
;; if something goes wrong in the program , what should i be able to do ?
;; consequence of no set-car! set-cdr!
;; (set! a 3)
;; copy entire environment ?
;; mutability ?

;;---------------------------------------------------------------------
;; probably a better way to do this
;; ( environment-tag environment )
;;  environment is a proper list with key value pairs 
;; 
;; ( environment-tag ...)
(define (enviro? exp)
  (and (pair? exp) (eq? (first exp) '<enviro>)))

(define (enviro-unpack ev)
  (if (enviro? ev)
      (second ev)
      (error (list 'not 'an 'enviro ev))))

;; (define (make-enviro key-values)
;;   (tag '<enviro> key-values))
;;

;; again same bug , make it a proper list
;; to extrac key-values
;; (second <environment>) => key-values
(define (make-enviro key-values)
  (list '<enviro> key-values))


;; if environment lookup returns the pair
;; then know its defined
;; if want to modify value can do
;; if not defined returns symbol 'not-found
;; unpack environment first

;; -----------------------------------------------------------------
(define (enviro-lookup exp env cont)
  (define (env-recur x ys cont)
    (cond
     ((null? ys) (error (list x 'not-found 'in 'environment env)))
     ((eq? exp (first-first ys)) (cont (rest (first ys))))
     (#t (env-recur x (rest ys) cont))))
  (env-recur exp (enviro-unpack env) cont))


;;-----------------------------------------------------------------
;; modify environemnt
;; exp
;; val = new value want installed
;; env is tagged environment
;; cont = accept result of success/failure
;;
;; destructive modification
;; why environment must be a list in a list
;; since if a new head is required ??
;; where is this argument going ??
;;
;;-----------------------------------------------------------------
(define (enviro-modify sym val env cont)
  ;; ys represents a proper list of key value pairs
  (define (env-recur ys)
    (cond
     ((null? ys) (error (list 'enviro-modify 'symbol sym 'not-found 'in 'environment env)))
     ((eq? sym (first-first ys)) (begin
				   (set-car! ys (cons sym val))
				   (cont val)))
     (#t (env-recur (rest ys)))))
  (env-recur (enviro-unpack env)))

;; ----------------------------------------------------------------

;; (define (enviro-modify sym val env cont)
;;   (enviro-pair-lookup sym
;; 		      env
;; 		      (lambda (key-value)
;; 			(if (pair? key-value)
;; 			    (begin (set-cdr! key-value val)
;; 				   (cont key-value))
;; 			    (cont #f)))))

(define (enviro-extend sym val env cont)
  ;;(set! $env (tag '<environment> (cons (cons sym val) (environment-unpack env))))
  (set-cdr! env (cons (cons sym val) (enviro-unpack env)))
  (cont val))

;; if pair comes back from environment modified , then its been modified
;; if not , then extend environment 
(define (enviro-modify-or-extend sym val env cont)
  (define (env-recur ys)
    (cond
     ((null? ys) (enviro-extend sym val env cont)) ;; pass-through to enviro-extend
     ((eq? sym (first-first ys)) (begin
				   (set-car! ys (cons sym val))
				   (cont val)))
     (#t (env-recur (rest ys)))))
  (env-recur (enviro-unpack env)))



;; --------------------------------------------------------------------------------

;; lookup in environment
;; $env is a cheat into environment 

;racket complaining set-cdr! set-car! not allowed
;if using environment passed by value
;how change value of variable ? copy entire environment ?


;; ;; used by set!
;; (define (modify-or-extend-env sym val e cont)
;;   (cond
;;    ((null? e)  ; add key . val to front environment chain
;;     (set! $env (cons (cons sym val) $env))
;;     (cont val))    
;;    ((eq? sym (caar e)) ; set val slot in key . val pair
;;     (set-cdr! (car e) val)
;;     (cont val))
;;    (#t (modify-or-extend-env sym val (cdr e) cont))))


;; (define my-special-counter
;;   (let ((code 0))
;;     (lambda ()
;;       (set! code (+ code 1))
;;       code)))


;; (define (base-eval exp env cont)
;;   (cond
;;    ((pair? exp)    (base-eval-pair exp env cont))
;;    ;; most things evaluate to themselves ....
;;    ((null? exp)              (cont exp))
;;    ((number? exp)		 (cont exp)) ;; ok
;;    ((boolean? exp)		 (cont exp)) ;; ok
;;    ((string? exp)		 (cont exp)) ;; ok
;;    ((procedure? exp)         (cont exp))
;;    ((symbol? exp)		 (eval-var exp env cont))	
;;    (#t (cont 'to-be-written-for-base-eval--))))


;; break base eval into pairs or atomics
;; 
;; (define (base-eval-pair exp env cont)
;;   (cond
;;    ((primitive? exp)        (cont exp))
;;    ((closure? exp)          (cont exp))
;;    ((macro-closure? exp)    (cont exp))
   
;;    ;; prim-eval ?
;;    ;;((eq? (car exp) 'prim-eval) (prim-eval exp env cont))
;;    ((eq? (car exp) 'evil) (cont (eval (second exp) (interaction-environment))))
   
;;    ;; does callcc work ? i dont think so ...
;;    ((eq? (car exp) 'callcc)  (eval-callcc          exp env cont))
;;    ((eq? (car exp) 'quote)  (eval-quote            exp env cont))
;;    ((eq? (car exp) 'define) (eval-define           exp env cont))
;;    ((eq? (car exp) 'set!)	 (eval-set!             exp env cont))
;;    ((eq? (car exp) 'quasiquote)      (eval-quasiquote  exp env cont))	
;;    ;; lambda and mlambda code should be 
;;    ((eq? (car exp) 'lambda) (eval-lambda exp env cont))
;;    ((eq? (car exp) 'mlambda) (eval-mlambda exp  env   cont))
;;    ;; 
;;    ((eq? (car exp) 'begin)  (eval-begin  exp env cont))
;;    ((eq? (car exp) 'if) (eval-if  exp env cont))
;;    ((eq? (car exp) 'load)   (eval-load exp env cont))
;;    ;; defmacro is just (define some-name (mlambda ...))
;;    ((eq? (car exp) 'defmacro) (eval-defmacro       exp  env    cont))
;;    ;; spent ages debugging macros only find let was undefined ...
;;    ((eq? (car exp) 'let)    (eval-let  exp  env  cont))
;;    ;; 
;;    (#t (eval-application exp env cont))))




;;;
;;(install-handler! (lambda (exp env cont) ) (lambda (exp env cont) ))


  ;; 
  ;; ;; ---- try unify all environment access and setting variables 	
  ;; 
  ;; ((eq? (car exp) 'macroexpand-1) (eval-macroexpand-1       exp      cont))
  ;; ;;((eq? (car exp) 'macroexpand) (eval-macroexpand       exp      cont))
  ;; ((eq? (car exp) 'mlambda) (eval-mlambda exp              cont))	
  ;; 
  ;; ;;((eq? (car exp) 'let*)   (eval-let* exp                  cont))
  ;; ((eq? (car exp) 'load)   (eval-load exp                  cont))
  
  ;; ;; ;;((eq? (car exp) 'cond)	 (meta-apply 'eval-cond (cdr exp) env cont))
  
  ;; ;; ((operator? exp 'let*)	 (meta-apply 'eval-let*
  ;; ;; 				     (car (cdr exp)) (cdr (cdr exp)) env cont))
  ;; ;; ((operator? exp  'letrec) (meta-apply 'eval-letrec
  ;; ;; 				     (car (cdr exp)) (cdr (cdr exp)) env cont))
  ;; ;; ((operator? exp 'EM)	 (meta-apply 'eval-EM exp env cont))
  ;; ;; ((operator? exp 'exec-at-metalevel)
  ;; ;;  (meta-apply 'eval-EM exp env cont))
  ;; ;; ((operator? exp 'primitive-EM)
  ;; ;;  (meta-apply 'eval-primitive-EM exp env cont))

  ;; ;; ((operator? exp 'reset)	 (meta-apply 'eval-reset exp env cont))

  ;; ;; ((operator? exp  'exit)	 (meta-apply 'eval-exit exp env cont))
  ;; ;; ((operator? exp 'load)	 (meta-apply 'eval-load exp env cont))
  ;; ;; ((operator? exp 'and)	 (meta-apply 'eval-and (cdr exp) env cont))
  ;; ;; ((operator? exp 'or)	 (meta-apply 'eval-or (cdr exp) env cont))
  ;; ;; ((operator? exp 'delay)	 (meta-apply 'eval-delay exp env cont))
  ;; ;; ((operator? exp 'cons-stream)
  ;; ;;  (meta-apply 'eval-cons-stream exp env cont))
  ;; (else (eval-application exp cont))))


;; --------------------------------------------------------------------------------------------
;; pretty-print
;; if something is liable to lead to infinite loop ...
;;
;; --------------------------------------------------------------------------------------------

;; --------------------------------------------------------------------------------------------
;; delay => promise 
;; much like lambda => closure
;;
;; --------------------------------------------------------------------------------------------



;; --------------------------------------------------------------------------------------------
;; expression 
;; (load "filename" . . . )
;; filename turned into a port  
;; read from port until done or error occurs -- no way to recover mind
;; then closes the file
;;
;; different ways different scheme implements these
;; close-port close-input-port close-output-port
;;

(define (eval-load exp env cont)
  (let ((current-port #f))
    (define (reader nth)
      (let ((input (read current-port)))
	(if (eof-object? input)
	    (begin
	      (close-input-port current-port)
	      (cont 'done))
	    (begin
	      (newline)
	      (display "in") (display "[") (display nth) (display "] : ")  	  
	      ;;(display input)
	      (pretty input)
	      (newline)
	      (base-eval input
			 env
			 (lambda (result)
				 (display "out") (display "[") (display nth) (display "] : ")
				 ;;(display result)
				 (pretty result)
				 (newline)
				 (reader (+ nth 1))))))))
    (let ((filename-expr (cadr exp)))
      (base-eval filename-expr
		 env
		 (lambda (filename) ;; hopefully an existing file ? will it be found on users computer?
		   (set! current-port (open-input-file filename))
		   (if current-port
		       (reader 0)
		       (cont 'fail))
		   )))))


















;; ----------------------------------------------------------------------------------------
;; poplog programming language ... 
;; register machine
;; e r k
;;
;; mutuallly recursive functions / defines
;;
;; ------------------------------------------------------------------------------------------
;; common bugs
;; traversal of expression , done manually instead of through a
;;
;; nested quasi - quoted expressions ?
;;
;; quasi quote
;; (quasiquote x)
;; cps version of quasi quote ?
;;
;;
;; all sorts of ways quoting can go wrong ...
;;
;; recursively
;;
;; (quasiquote (unquote x)) = x
;;
;; 
;; `( ... , ... )
;;
;;   ( ... ,@X  ...) splice may become 
;;        (append ? ? ? X ? ? ?)
;;    so leave that for runtime ??
;;
;; what about 
;;    pair symbols a . b  ?
;;
;; still not sure about splicing ... not implemented yet
;; (qq ((unq q)(unq q)(unqs a)))
;;     ((unq q)(unq q)(unqs a)))
;;     car = (unq q)
;;     
;; ------------------- quasi quote think got fixed somehow ........................
(define (eval-quasiquote exp env cont)
  (define (qq-recur exp)
    (cond
     ((pair? exp)
      (cond
       ((eq? (car exp) 'unquote)
	(base-eval (cadr exp) env (lambda (r) r)))
       ((and (pair? (first exp))     ;; (,@a ?)
	     (eq? (first (first exp)) 'unquote-splicing))
	(base-eval (second (first exp)) ;; eval thing being splice
		   env
		   (lambda (r)
		     (append r (qq-recur (cdr exp))))))
       (#t
	(cons (qq-recur (car exp))
	      (qq-recur (cdr exp)))
	)))
     (#t exp)))
  (cont (qq-recur (cadr exp))))


;; quasi qutoe in first position??

;; splicing
;; `(a b ,@c d)
;;  c gets evaluated must return a list
;; then this is cons'ed into
;; parent must know if expression is a unquote-splicing

;;
;; first draft without splicing
;;
;; (define (eval-quasiquote exp cont)
;;   (define (qq-recur exp)
;;     (cond
;;      ((pair? exp)
;;       (cond
;;        ((eq? (car exp) 'unquote)
;; 	(base-eval (cadr exp) (lambda (r) r)))
;;        (#t
;; 	(cons (qq-recur (car exp))
;; 	      (qq-recur (cdr exp)))
;; 	)))
;;      (#t exp)))
;;   (cont (qq-recur (cadr exp))))


;; let* 
;; (let* ((a 1 2 3 ...)(b 2 ...)(c 3 ...)) ...body...)
;; ------------------------------------------------------------------------------------
;; macro expand to be
;; (let ((a 1 2 3 ...))
;;    (let ((b 2 ...))
;;       (let ((c 3 ...))
;;           ...body...
;; 
;; pass result back to base-eval 
;; 
;; ------------------------------------------------------------------------------------
;; tip : may be helpful on eval-something to pass full exp expression to save confusion ... 
;; (define (eval-let* exp cont)
;;   (let ((let-args (cadr exp))   ; ((a 1 2 3...)(b 2 ...)(c 3 ...)
;; 	(let-body (cddr exp)))  ; (...body...)
;;     (define (recur xs)
;;       (cond
;;        ((null xs))))
;;     ))
  


;; ------------------------------------------------------------------------------------
;; let 
;; (let ((a 1 2 3 ...)(b 2 ...)(c 3 ...)) ...body...)
;; ------------------------------------------------------------------------------------
;; can be rewritten as lambdas for implementation but its a ballache to debug
;; for human reasoning , why going this route
;; eventually discover what this meta-continuation meta-apply is all about
;; 
;; (cadr : ((a 1 2 3)(b 2 3 4 )(c 4 5 6))
;; (cddr : ...body...
;; same as 
;; ((lambda (a b c) ...body ...) 
;;           (begin 1 2 3 ...)
;;           (begin 2 ...)
;;           (begin 3 ...))
;;
;; evaluate implicit bodies of 
;; extend environment
;; execute body of let
;; save result
;; restore environment
;; return result                               
;;
;; ------------------------------------------------------------------------------------
;;
;; (define (eval-let exp cont)
;;   (let* ((let-args (cadr exp)) ;; ((a 1 2 3)(b 2 ...)(c 3 ...))
;; 	 (let-body (cddr exp)) ;; ...body ...
;; 	 (let-args-syms (map car let-args)) ;;  (a b c)
;; 	 (let-args-body (map cdr let-args)) ;; ((1 2 3) (2 ..)( 3 ...))
;; 	 (let-args-body-vals (map (lambda (s) (eval-begin s (lambda (v) v)))
;; 				  let-args-body)) ;; implicit begin on let-args-bodies	 
;; 	 )
    
;;     ;; helpers
;;     (define (bind args vals)
;;       (cond
;;        ((null? args) '()) ;; done -- maybe we threw inputs away ?
;;        (#t
;; 	(cons (cons (car args) (car vals))
;; 	      (bind (cdr args) (cdr vals))))))
;;     ;; temporary extend environemnt
;;     (define (extend-env partial-env env)
;;       (append partial-env env))
;;     ;; 
;;     (let ((ext-env (extend-env (bind let-args-syms let-args-body-vals) $env))
;; 	  (old-env $env))
;;       (set! $env ext-env)
;;       (eval-begin let-body		  
;; 		  (lambda (result)
;; 		    (set! $env old-env)
;; 		    (cont result))))))




;; ------------------------------------------------------------------------------------
(define (eval-let exp env cont)
  (let* ((let-args (cadr exp)) ;; ((a 1 2 3)(b 2 ...)(c 3 ...))
	 (let-body (cddr exp)) ;; ...body ...
	 (let-args-syms (map car let-args)) ;;  (a b c)
	 (let-args-body (map cdr let-args)) ;; ((1 2 3) (2 ..)( 3 ...))
	 (let-args-body-vals (map (lambda (s) (eval-implicit-begin s env #f (lambda (v) v)))
				  let-args-body)) ;; implicit begin on let-args-bodies	 
	 )    
    (let ((new-env (extend-env (arglist-bind let-args-syms let-args-body-vals) env)))
      (eval-implicit-begin let-body
			   new-env
			   #f
			   (lambda (result)
			     (cont result))))))


;;-------------------------------------------------------------------------------------
;; let used these internal helpers -- now these are global helpers
    ;; (define (bind args vals)
    ;;   (cond
    ;;    ((null? args) '()) ;; done -- maybe we threw inputs away ?
    ;;    (#t
    ;; 	(cons (cons (car args) (car vals))
    ;; 	      (bind (cdr args) (cdr vals))))))
    ;; ;; temporary extend environemnt
    ;; (define (extend-env partial-env env)
    ;;   (append partial-env env))
    ;; ;; 



;; -------------------------------------------
;; () ....... empty application ??
;; (f) ........ no arguments
;; (f 1) .... one arg
;; (f 1 2 ) .. two arg
;; (f 1 2 3 4 5  ... ) arbitrary number args 
;; (f 1 2 3 )
;; ((lambda args 1 2 3) ... body ...)
;; evaluate all items in expression
;; look at function operator , is it primitive ?
;; if primitive call primitive application 
;; if compound call compound application


(define (eval-primitive-op op vals env cont)
  ;;(format #t "eval-primitive-op op=~a\n vals=~a\n env=~a\n" op vals env)
  (let ((proc (cdr op)))
    (cont (apply proc vals))))

;; ;; extend environment with one argument and values vals
;; (define (extend-env arg vals env)
;;   (set-car! env (list (cons (cons arg vals) (car env)))))
;; ;; (cond
;; ;;  ((null? args) env)
;; ;;  (#t (append (list args vals) env))))
;; (define (restore-env env)
;;   (set-car! env (cdar env)))


;; most general binding is take all arguments and swallow them into one argument
;; let the apparatus sort it out
;;
;; (lambda args body)
;;         vals
;; extend environment with args bound to vals
;; execute body of lambda with implicit begin
;; restore environment

;; args is just a single thing
;; (define f (lambda args ...))
;; (f 1 2 3 4 5)
;; ------------------------------------------------------
;; env : (a 1 b 2 c 3 d 4 ...)
;; extended env : (args (1 2 3 4 5) a 1 b 2 c 3 d 4 ...)
;; ------------------------------------------------------


;; many ways goes wrong in calling procedure
;; case 1 (lambda (x y z) ...)      x y z match to nth arguments
;; case 2 (lambda x .... )          x slurps everything
;; case 3 (lambda (x y . z) ...)    z is &rest argument
;; case 4 (lambda (x &optional &default ...) ...) ?? huh ...

;; bind purpose is to return partial environment
;; with var val in flat list
;; 
;; -------------------------------------------------------------------------------
;; function application
;; (closure (lambda <arglist> ...body...) <an-environment-of-bindings>)
;;
;; (lambda args args)
;; (lambda (x) x)
;; (lambda (x y z . rst) ...)   rst slurp all other args
;;
;; default or optional arguments ??
;; (lambda ((x 1) (y 2) (z 3)) ...)
;;--------------------------------------------------------------------------------

;; share arglist-bind between eval-compound and eval-macro,,,
;;
;;
(define (arglist-bind args vals)
  (cond
   ((symbol? args) (cons (cons args vals) '())) ;; slurp + done
   ((null? args) '()) ;; done -- maybe we threw inputs away ?
   (#t
    (cons
     (cons (car args) (car vals))
     (arglist-bind (cdr args) (cdr vals))))))

(define (extend-env partial-env env)
  ;; (write "\npartial-env:")
  ;; (write partial-env)
  (make-enviro (append partial-env (enviro-unpack env))))


;; share same arg passing routine as macros
(define (eval-compound-op op vals env cont)
  (shared-arg-passing op vals env cont))

;; (define (eval-compound-op op vals env cont)
;;   (define (eco-case123 lam-arglist vals lam-body env cont)
    
;;     (let ((new-env (extend-env (arglist-bind lam-arglist vals) env)))
;;       ;; (format #t "lambda env : ~a~%" $env)
;;       ;; (format #t "lambda body : ~a~%" lam-body)
;;       ;; (write "\nnew-env:")
;;       ;; (write new-env)
;;       ;; (write "\nlam body:")
;;       ;; (write lam-body)      
;;       (eval-implicit-begin lam-body
;; 			   new-env
;; 			   #f
;; 			  (lambda (result)		    
;; 			    (cont result))
;; 			  )))
;;   ;; use internal eco-case123
;;   (let* ((lam (cadr op))
;; 	 (lam-arglist (cadr lam))
;; 	 (lam-body (cddr lam)))
;;     (eco-case123 lam-arglist vals lam-body env cont)))
;; ;;--------------------------------------------------------------------------------
(define scheme-procedure? procedure?)



;;
;; what does this do ?
;; know op is a macro closure
;; unargs are textual data passed to macro
;;   corrollary unargs is vals in function world 
;; pass arguments across to macro closure
;; execute the macro closure
;; generate an expansion
;; must return an s expression that can be written down

;; each entry in environment is a cons pair
;;

;; --- eval-app-macro  + eval-compound-op  are the same routine
;; except eval-app-macro has a last base-eval exp env cont on returned expanded expression


;; can we share macro expansion and 
(define (shared-arg-passing cloz args env cont)
  (define (eam-case123 mlam-args unargs mlam-body mlam-env cont)
    
    (let* ((bindings (arglist-bind mlam-args unargs))
	   (new-env (extend-env bindings mlam-env)))  ;; <<<=== * bug fix here **
      ;; (format #t "macro-body : ~a~%" mlam-body)    
      ;; (format #t "macro env : ~a~%" (take $env 3)) ;; limit env to 3 entries ...
      ;; (format #t "macro-bindings : ~a~%" bindings)    
      ;;(cont mlam-body) ;; --------- pass answer as body of macro unevaluated ...
      (eval-implicit-begin
       mlam-body
       new-env
       #f
       cont)))
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
  
  (let* ((mlam (cadr cloz))
	 (mlam-env (caddr cloz))
	 (mlam-arg (cadr mlam))
	 (mlam-body (cddr mlam)))
    (eam-case123 mlam-arg args mlam-body mlam-env cont)))



;; macros do an extra evaluation after expansion
;; macroexpand-1
;; macroexpand
;; these cant be too much effort to get working ?
(define (eval-app-macro op unargs env cont)
  (shared-arg-passing op unargs env (lambda (expanded)
				      (base-eval expanded env cont))))




;; (define (eval-app-macro op unargs env cont)
;;   (define (eam-case123 mlam-args unargs mlam-body env cont)
    
;;     (let* ((bindings (arglist-bind mlam-args unargs))
;; 	   (new-env (extend-env bindings env)))
;;       ;; (format #t "macro-body : ~a~%" mlam-body)    
;;       ;; (format #t "macro env : ~a~%" (take $env 3)) ;; limit env to 3 entries ...
;;       ;; (format #t "macro-bindings : ~a~%" bindings)    
;;       ;;(cont mlam-body) ;; --------- pass answer as body of macro unevaluated ...
;;       (eval-implicit-begin
;;        mlam-body
;;        new-env
;;        #f
;;        (lambda (expanded-expression)
;; 	 ;;(format #t "\nmacro-expansion : ~a~%" expanded-expression)		  
;; 	 (base-eval expanded-expression env cont)))))
;;   ;; instead of base-eval if we just pass
;;   ;;(cont expanded-expression) <-- equivalent to macro-expand-1 
;;   ;; (format #t "macro mlam : ~a~%" op)
;;   (let* ((mlam (cadr op))
;; 	 (mlam-arg (cadr mlam))
;; 	 (mlam-body (cddr mlam)))
;;     (eam-case123 mlam-arg unargs mlam-body env cont)))


;; tidy stuff up above
;; equivalent macro-expand-1



;; ----------------------------------------------------------------------------------------------
;; what is macroexpand 1 then ?
;; what is macroexpand ?
(define (eval-macroexpand-1 exp cont)
  (let ((expr-to-be-expanded (second exp)))
    (base-eval (second exp)
	       (lambda (r)
		 (cont r)))))


;; -------------------------------------------------------------------------------------------------

(define *handlers* '())

;; last installed handler will be first one to be checked L I F O lifo
(define (install-handler! name predicate action)
  (set! *handlers* (cons (list name predicate action)
			 *handlers*)))

;; (install-handler! 'cons
;; 		  (lambda (exp env cont) (and (pair? exp) (eq? (car exp) (symbol? exp)))
;; 		  (cont (cons al-var) 

;; ;; just a matter of finding a predicate allows the expression
;; ;; then run the action part of the handler
(define (base-eval exp env cont)
  (define (recur ys)
    (cond
     ;; no handler - show message
     ;; some acorn of error handler here ...
     ;; hand off to eval-application if no handlers found
     ((null? ys) (eval-application exp env cont))
     (((second (first ys)) exp env cont)  ;; if predicate matches - do the action
      ((third (first ys)) exp env cont))
     ;; try next handler
     (#t (recur (cdr ys)))))
  (recur *handlers*))





;; (define (base-eval exp env cont)
;;   (define (recur ys)
;;     (cond
;;      ;; no handler - show message
;;      ;; some acorn of error handler here ...
;;      ((null? ys) (base-eval 'message
;; 			    (extend-env (list (cons 'message (list 'no-handler-for-expression exp))
;; 					      (cons 'exp exp)
;; 					      (cons 'env env)
;; 					      (cons 'cont cont))
;; 					$env)
;; 			    cont))
;;      (((second (first ys)) exp env cont)  ;; if predicate matches - do the action
;;       ((third (first ys)) exp env cont))
;;      ;; try next handler
;;      (#t (recur (cdr ys)))))
;;   (recur *handlers*))


;; -------------------------------------------------------------------------------------------------------
;; evil x y z ...
;; implicit begin
(define (eval-evil exp env cont)
  (cont (eval (second exp) (interaction-environment))))



  ;; (define (recur exp env cont)
  ;;   (cond
  ;;    ((null? exp) (cont '()))
  ;;    ((null? (cdr exp))
  ;;     (base-eval (car exp) env (lambda (r) ;; last one
  ;; 				 (cont (eval r (interaction-environment))))))
  ;;    (#t (base-eval (car exp) env (lambda (r)
  ;; 				    (eval r (interaction-environment))
  ;; 				    (recur (cdr exp) env cont))))))
  ;; (recur (cdr exp) env cont))



;; ------------------------------------------------------------------------------------------------------
		  

;; (op arg1 arg2 arg3)
;; (op could a number of things
;; .................
;; evaluate operator
;; if op is macro then pass to symbolic handler
;; otherwise evaluate the arguments
;; pass to appropriate handler
;;
;; could be a compound function lambda 
;; could be a underlying scheme procedure
;; could be a macro macro-closure
(define (eval-application exp env cont)
  ;;(format #t "ev-app ***~a~%" exp)
  
  (let ((op (car exp))
	(unargs (cdr exp)))
    
    ;; (newline)
    ;; (write "eval-application op :")
    ;; (write op)
    ;; (newline)

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


;; -------------------------------------------------------------------------------------------------


;; debugging - who calls ? who does this call ?
;; what does this mean? where is it used?
(define (compound? op)
  (and (not (primitive? op))
       (not (macro-closure? op))
       (closure? op)))


;; -------------------------------------------------------------------------------------------
;;
;; semantic define at top is scheme define
;; lambda , define are now different from external meanings 
;;
;; no macros involved
;;
;; (define (f a)
;;    (let ((lambda 3)
;;          (define 4))
;;      (cons define (+ lambda define))))
;;
;; -------------------------------------------------------------------------------------------
;;  macro expansion fully expands any expansion until output is functional code only 
;;  
;;  macros are involved funky-macro
;;
;; (define (f a)
;;    (let ((lambda 3)
;;          (define 4))
;;      (funky-macro (cons define (funky-macro lambda define))))
;;
;; -------------------------------------------------------------------------------------------
;; can a name both be a macro and a function ?
;;
;; -------------------------------------------------------------------------------------------


;; -----------------------------------------------------------------------------------------------
;; (lambda args body) <--- most general form , can always rebind 
;; (lambda (a b c ... ) body)
;; (lambda (a b c) body)
;;
;; at definition time all need is record environment and remember the lambda definition
;;

;; -----------closures + lambda ------------------------------
;; surely there is a more complex way to do this ...
(define (closure? x)
  (and (pair? x)
       (eq? (car x) 'closure)))

;; bug ! 
;; (define (make-closure lam env)
;;   (tag 'closure (cons lam env)))
;;
;; make it a proper list , then if want to extract environment
;; (third <the-closure>) => env
;;
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


;; ---------- macro-closure + mlambda ---------------------------------
(define (macro-closure? x)
  (and (pair? x)
       (eq? (car x) 'macro-closure)))

(define (make-macro-closure mlam env) 
  (list 'macro-closure mlam env))

(define (eval-mlambda exp env cont)
  (cont (make-macro-closure exp env)))




;; ----------- primitive code ---------------------------------------------
;; where code can be internal routine in python or scheme
;; or can be machine code
;; interpreted vs compiled ?
(define (make-primitive code)
  (tag 'primitive code))
(define make-prim make-primitive)




;; -----------------------------------------------------------------------------------------------
;;
;; (define a 4) really ugly also , no idea what is going on there ...
;; (set! a 3) really ugly behind scenes , environment is flat
;;
;; simplest philosophy possible
;;
;; -----------------------------------------------------------------------------------------------
;; some mechanism to give a symbol some special powers
;; some generic way to attach or detach hooks onto the-thing
;;
;; ----------------------------------------------------------------------------------------------

(define my-alist-lookup
  (lambda (x ys)
    (cond
     ((null? ys) #f)
     ((eq? x (car ys)) ys)
     (#t (my-alist-lookup x (cdr (cdr ys)))))))


;; evaluate 2nd argument -- cadr here because leading set! keyword stripped before this ?
;;  why not used 2nd here or something, or even 3rd
;;  1   2  3     3rd thing is the value to be evaluated ??  
;; (set! a 3)
;; define expression stripped define before
;; just given stripped (a 3)
;; car  : a
;; cadr : 3

;; traverse through until find symbol in environment and change corresponding value slot
;; e is a env traversal pointer
;; starts at head $env
;; works its way to nil at end
;; (  (a . 3) (b . 4) ...)
;;      (caar : a
;;      (cdar : 3
;; 
;; (define (modify-env sym val e cont)
;;   (cond
;;    ((null? e) (error 'modify-env123 'not-found sym val $env cont))
;;    ((eq? sym (caar e))
;;     (set-cdr! (car e) val)
;;     (cont val))
;;    (#t (modify-env sym val (cdr e) cont))))

;; sort of inviting problems
;; (set a 3)
;; a is not in environment , therefore fall over ...

;; (set! a 3)
;;       (a 3)
;;         (3)
;; (cadr : a
;; (caddr : 3)
;;
;; want $e in interpreter , to be causal linked to $env here 
;; 
;; set! 
;; exp = (set! a b)

(define (eval-set! exp env cont)
  (base-eval (third exp)
	     env
	     (lambda (result)
	       (let ((sym (second exp)))
		 (cond
		  ((eq? sym '$e)
		   (enviro-modify sym result env cont))
		  (#t		
		   (enviro-modify sym result env cont)))))))


;; (set a 3)
;; a is not in scope
;; environment extended to include (a . 3) binding
;; think this is better than throwing an error
;; =====================================================================

;; debugging is that arguments are enforced
;; (cons x y) ;; expects 2 arguments only
;;       (x y)  cdr
;;         (y)  cddr
;;          ()  cdddr

;; --------- send user to the error repl ------------------------
;; how does the user recover ?
;; how supply restarts ?
;; how supply errors ?
;; --------------------------------------------------------------
;; (define (eval-cons exp env cont)
;;   ;; enforce 2 arguments only or start error repl
;;   (cond
;;    ((or (null? (cdr exp))
;; 	(null? (cdr (cdr exp)))
;; 	(not (null? (cdr (cdr (cdr exp))))))
;;     (error-repl `(cons-only-takes-two-arguments ,exp) env cont))
;;    (#t
;;     (base-eval (second exp) env
;; 	       (lambda (r1)
;; 		 (base-eval (third exp) env
;; 			    (lambda (r2)
;; 			      (cont (cons r1 r2)))))))))
;;-----------------------------------------------------------


;; passed just condition arg1 arg2
;; (if x y z)
(define (eval-if exp env cont)
  (let ((condition (second exp))
	(consequent (third exp))
	(alternative (fourth exp)))
    (base-eval condition
	       env
	       (lambda (result)
		 (if result
		     (base-eval consequent env cont)
		     (base-eval alternative env cont))))))



;; in the interpreted version liek to do this
;; and see $e as
;; ( (aa . 4) .... environment $e continues ....)
;; (set! $e (cons (cons (quote aa) 4) $e))
;;
;; evaluate variable in macro language
;; really looking at renaming the variable
;;
;; backdoor $e => $env
;; so this is how interpreter can access environment
;; lets keep an eye out for this ability ....
;; see if we can develop it further ...
;;
;; just get first of value of key-value pair
;;

(define (eval-var exp env cont)
  (cond
   ((eq? exp '$e) ; for debugging type $e then get environment shown
    (cont env))
   (#t
    (enviro-lookup exp env cont))))


;;
;; two different machines
;; the macro expander and the functional runtime machine
;;
;; defmacro behaves like define in that it gets installed like define in the macro environment
;;
;; what does it mean to redefine a macro ?
;; old macro under that name gets trashed ?
;; warnings or road blocks to prevent trashing already declared macros by accident 
;;
;; (defmacro setq2 (v1 v2 e)
;;   (list 'progn (list 'setq v1 e) (list 'setq v2 e)))
;;
;; (defmacro setq2 (v1 v2 e)  ...body ...)
;;           cadr   caddr       cdddr 
(define (eval-defmacro exp env cont)
  (let* ((macro-name (cadr exp))
	 (macro-definition `(mlambda ,@(cddr exp)))
	 (macro-closure (make-macro-closure macro-definition env)))
    (enviro-modify-or-extend macro-name
			     macro-closure
			     env
			     cont)))


;; callcc simply does not work ...
;; (callcc (lambda (x) ...))
(define (eval-callcc exp env cont)
  (base-eval `(,exp ,(make-prim cont)) env cont))


;; removed from def-macro form
;; (define (extend-env sym val env)
;;       (cons sym (cons val env)))    
;;   (define (modify-or-extend-env sym val e cont)
;;     (cond
;;      ((null? e)
;;       (set! $env (extend-env sym val $env))
;;       (cont val))    
;;      ((eq? sym (car e))  <--- bug is here should be (caar e)
;;       (set-car! (cdr e) val)
;;       (cont val))
;;      (#t (modify-or-extend-env sym val (cdr (cdr e)) cont))))
;;   ;; 
  




;; define can be desugar-ed in various ways
;; (define a 3)
;; define expression stripped define before
;; just given stripped (a 3)
;; car  : a
;; cadr : 3
;; e iterates over $env global ptr
;; ----- only support (define f ....) at the moment ...
;; (define (f . x) body ...) is a lambda expression
;; (define f (lambda ... body))
;; ------------------------------------------------
;; (define a 3)
;; second : a 
;; third  : 3
(define (eval-define exp env cont)
  ;; eval the thing want to assign to a symbol 
  (base-eval (third exp)
	     env
	     (lambda (result)
	       (let ((sym (second exp)))
		 (enviro-modify-or-extend sym result env cont)))))



;; (quote) missing argument - malformed expression
;; (quote alpha)
(define (eval-quote exp env cont)
  (let ((quoted (second exp)))
    (cont quoted)))



;; pretty printer slightly broken
;;
;; if the thing being printed is a closure - causes infinite loop on environment
;; if it is a macro-closure 
;; 
;; ( 1 . 2 ) printed as ( 1 2 ) which is wrong ...


(define (pretty-tail exp)
  (cond
   ((null? exp) '())
   ((boolean? exp) (display exp))   
   ((number? exp) (display exp))
   ((pair? exp)
    (pretty (car exp))
    (cond
     ((null? (cdr exp)) '())
     ((pair? (cdr exp)) (display " ") (pretty-tail (cdr exp)))
     (#t (display " . ") (pretty-tail (cdr exp)))))
   ((symbol? exp) (display exp))
   ((string? exp) (write exp))
   ((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ((closure? exp) (display "#<closure ")(write (second exp)) (display " >"))
   ((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">"))))

(define (pretty exp)
  (cond
   ((null? exp) (display '()))
   ((boolean? exp) (display exp))   
   ((number? exp) (display exp))
   ((symbol? exp) (display exp))
   ((string? exp) (write exp))
   ((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ((closure? exp) (display "#<closure ")(write (second exp)) (display " >"))   
   ((and (pair? exp) (eq? (car exp) 'quote)) ;; ?
    (display "'") (pretty-tail (cdr exp)))   
   ((pair? exp) (display "(") (pretty-tail exp) (display ")"))
   ((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">"))))



;; how is the debugger supposed to work
;; what about information debugger has for itself
;; how does it know what level it is on ?
;;
;; height?
;; some repl ...
(define (error-repl exp env cont)
  (newline)
  (display "debugger") (display "[??") ;;(display nth)
  (display "] : ")
  (let ((input (read)))
    ;;(display input)
    (pretty input)
    (newline)
    (base-eval input env (lambda (result)
			   (display "out") (display "[??") ;; (display nth)
			   (display "] : ")
			   ;;(display result)
			   (pretty result)
			   (newline)
			   (error-repl result env cont)))))







;; some repl ...
(define (black-repl nth env result cont)
  (let ((input (read)))
    (if (eof-object? input) (cont  (exit))
	(begin
	  (newline)
	  (display "in") (display "[") (display nth) (display "] : ")  	  
	  ;;(display input)
	  (pretty input)
	  (newline)
	  (base-eval input env (lambda (result)
			     (display "out") (display "[") (display nth) (display "] : ")
			     ;;(display result)
			     (pretty result)
			     (newline)
			     (black-repl (+ nth 1) env result cont)))))))


;; very similar to eval-begin ... strips begin ... just implicit sequence
;; do we want to print and count each thing though ...
(define (black-pre-repl cmds env nth result cont)
  (cond
   ((null? cmds)
    (black-repl (+ nth 1) env result cont))
   (#t
    (let ((input (car cmds)))
      (display "in") (display "[") (display nth) (display "] : ")
      ;;(display input)
      (pretty input)
      (newline)
      (base-eval input
		 env
		 (lambda (result)
		   (display "out") (display "[") (display nth) (display "] : ")
		   ;;(display result)
		   (pretty result)
		   (newline)
		   (black-pre-repl (cdr cmds) env (+ nth 1) result cont)))))))





;;
;; ideally like to start black with some commands to do stuff ...
;; implicit begin also
;; also like to make black a macro , so when loaded into scheme
;; we can just write
;; (black .... ) a series of lisp expressions without having to quote everythin
;;
;; really just get one fkn macro to work in scheme
;; just one ...
;;
;; racket -format -- different than mit-scheme version of format ...
(define (black . cmds)
  ;;(format #t "~%blue scheme~%")  
  ;;(format #t "startup cmds = {~a}~%" cmds)
  (display "\nsimple scheme interpreter\n")
  (display "\nstartup cmds\n")
  (display cmds) 
  (black-pre-repl cmds $env 0 #f (lambda (x) x)))

  ;; (error 'we 'finding 'out 'wat 'cmds 'means)
  ;; (black-repl 0 #f (lambda (x) x)))



;; -----------------------------------------------------------------------------------
;; setup the initial environment now everything is in scope for a scheme interpreter
;;
;; environment was initially a flat list
;;  a 1 b 2
;; but when value is a list , appending two lists together values spliced into other
;; using cons pairs it keeps them seperated
;; if cons pair altered , then doesnt spew into other key-values further down environmnet chain
;; or cut the environment chain completely.
;;
;; change environment code implementation , what spill over ramifications , what else needs to change
;; to accomodate this ...
;;
;; symbols in environment
;; define
;; defmacro
;; set!
;; 
;;
;; reading top down source code
;;


;; 
;; (define (prim-eval exp env cont)
;;   (cont (eval exp (interaction-environment))))




(set! $env
    (make-enviro
     `(
           ;;(eval-quote . ,eval-quote)
           (a . 3)
	   (b . 4)
	   (c . 5)
	   (d . (1 2 3))
	   (e . (4 5 6))
	   (f . (7 8 9))

	   ;;(prim-eval . ,(make-prim prim-eval))
	   (macro-closure? . ,(make-prim macro-closure?))

	   (display . ,(make-prim display))
	   (write . ,(make-prim write))
	   
	   (caar . ,(make-prim caar))
	   (cadr . ,(make-prim cadr))
	   (cdar . ,(make-prim cdar))
	   (cddr . ,(make-prim cddr))
	   
	   (car . ,(make-prim car))
	   (cdr . ,(make-prim cdr))

	   ;; <---- cons : arity2 ----> using a slower more checking cons...
	   ;;(cons . ,(make-prim eval-cons))
	   (cons . ,(make-prim cons))
	   
	   (primitive? . ,(make-prim primitive?))
	   
	   (list . ,(make-prim list))
	   (append . ,(make-prim append))
	   
	   (null? . ,(make-prim null?))
	   (pair? . ,(make-prim pair?))
	   (symbol? . ,(make-prim symbol?))	     
	   (eq? . ,(make-prim eq?))
	   
	   (newline . ,(make-prim newline))

	   (make-prim . ,(make-prim make-prim))
	   
	   (first . ,(make-prim first))
	   (second . ,(make-prim second))
	   (third . ,(make-prim third))
	   ;;(fourth . ,(make-prim fourth))
	   
	   (+ . ,(make-prim +))
	   (- . ,(make-prim -))
	   (* . ,(make-prim *))
	   (/ . ,(make-prim /))
	   
	   (> . ,(make-prim >))
	   (< . ,(make-prim <))
	   (= . ,(make-prim =))
	   
	   (gensym . ,(make-prim gensym))
	   
	   (not . ,(make-prim not))
	   ;; and or --- macros ??
	   ;; when unless loop for
	   
	   ;; format
	   ($base-eval . ,base-eval)
	   ($env . ,$env))))



;; -----------------------------------------------------------------------------------
;; once all the symbols are defined 
;; we can install handlers
;;
;; install-handler! should also extend environment so that
;;
;; -----------------------------------------------------------------------------------


(install-handler! 'numbers
		  (lambda (exp env cont) (number? exp))
		  (lambda (exp env cont) (cont exp)))

(install-handler! 'booleans
		  (lambda (exp env cont) (boolean? exp))
		  (lambda (exp env cont) (cont exp)))

(install-handler! 'strings
		  (lambda (exp env cont) (string? exp))
		  (lambda (exp env cont) (cont exp)))

(install-handler! 'internal-procedure
		  (lambda (exp env cont) (procedure? exp))
		  (lambda (exp env cont) (cont exp)))

(install-handler! 'symbols
		  (lambda (exp env cont) (symbol? exp))
		  eval-var) 

(install-handler! 'evil
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'evil)))
		  eval-evil) 

(install-handler! 'load
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'load)))
		  eval-load) 


(install-handler! 'defmacro
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'defmacro)))
		  eval-defmacro) 

(install-handler! 'quasiquote
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'quasiquote)))
		  eval-quasiquote) 

(install-handler! 'quote
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'quote)))
		  eval-quote) 

(install-handler! 'define
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'define)))
		  eval-define) 

(install-handler! 'lambda
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'lambda)))
		  eval-lambda) 


(install-handler! 'if
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'if)))
		  eval-if) 

(install-handler! 'begin
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'begin)))
		  eval-begin)


(install-handler! 'let
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'let)))
		  eval-let)

(install-handler! 'mlambda
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'mlambda)))
		  eval-mlambda) 

(install-handler! 'set!
		  (lambda (exp env cont)
		    (and (pair? exp)
			 (eq? (car exp) 'set!)))
		  eval-set!) 

;; cons is a primitive function , i think a guards check when it is applied would be all thats
;; required ... 
;; 
;; (install-handler! 'cons
;; 		  (lambda (exp env cont)
;; 		    (and (pair? exp)
;; 			 (eq? (car exp) 'cons)))
;; 		  eval-cons) 
;;

