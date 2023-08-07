

;;------------------------------------------------------------------
;; as soon as this file is loaded
;; -----------------------------------------------------------------
(defmacro rl ()
  `(load "macro.scm"))

(defmacro rb ()
  `(begin (rl) (black)))

(defmacro rk ()
  `(begin (rl) (black ;;'(load "../test/factorial.scm")
		      '(load "../test/quasi.scm")
		      ;;(load "../test/cond.scm")		      ;;(load "../test/swap2.scm")
		      
		      )))

(defmacro ri ()
  `(begin (rl) (black '(load "../test/identity.scm"))))
;;-------------------------------------------------------------------




;; bug : redefining routine later on in program , deletes one looking at and causes spurious behaviour
;; layers :: symbol to be locked , unabled to be redefined

(define (cadr x) (car (cdr x)))
(define (caar x) (car (car x)))
(define (cdar x) (cdr (car x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))


(define (first xs) (car xs))
(define (second xs) (car (cdr xs)))
(define (third xs) (car (cdr (cdr xs))))


;;(define *the-primitives* (list))
(define $env (list))


;;---------------------------------------------------------------------
;; environment is list of key . value pairs 
;; true environment $env
;; env : ( (key . value) (key . value) (key . value) ...)
;; (car  (key . value)
;; (caar  key
;; (cdar  value
(define (env-lookup exp env cont)
  (cond
   ((null? env) (error 'env-lookup 'variable 'not-found! exp env))   
   ((eq? exp (caar env)) (cdar env))
   (#t (env-lookup exp (cdr env) cont))))
;; lookup in environment
;; $env is a cheat into environment 

;; used by set!
(define (modify-or-extend-env sym val e cont)
  (cond
   ((null? e)  ; add key . val to front environment chain
    (set! $env (cons (cons sym val) $env))
    (cont val))    
   ((eq? sym (caar e)) ; set val slot in key . val pair
    (set-cdr! (car e) val)
    (cont val))
   (#t (modify-or-extend-env sym val (cdr e) cont))))




(define my-special-counter
  (let ((code 0))
    (lambda ()
      (set! code (+ code 1))
      code)))
   

;; Eval functions
(define (self-identity? expression)
  (or (number? expression)
      (boolean? expression)
      (string? expression)))





(define (base-eval exp cont)
  (cond ((self-identity? exp) (cont exp)) ;; rather than (apply cont exp) just do it directly
	((symbol? exp)		 (eval-var              exp $env cont))
	((eq? (car exp) 'set!)	 (eval-set!             exp cont))
	((eq? (car exp) 'define) (eval-define           exp cont))
	;; ---- try unify all environment access and setting variables 	
	((eq? (car exp) 'quote)  (eval-quote (car (cdr  exp))    cont))
	((eq? (car exp) 'quasiquote)      (eval-quasiquote  exp  cont))	
	((eq? (car exp) 'defmacro) (eval-defmacro       exp      cont))	
	((eq? (car exp) 'if) (eval-if (cdr exp)                  cont))
	((eq? (car exp) 'lambda) (eval-lambda exp                cont))
	((eq? (car exp) 'mlambda) (eval-mlambda exp              cont))	
	((eq? (car exp) 'begin)  (eval-begin (cdr exp)           cont))
	((eq? (car exp) 'let)    (eval-let  exp                  cont))
	((eq? (car exp) 'let*)   (eval-let* exp                  cont))
	((eq? (car exp) 'load)   (eval-load exp                  cont))
	
	;; ;;((eq? (car exp) 'cond)	 (meta-apply 'eval-cond (cdr exp) env cont))
	
	;; ((operator? exp 'let*)	 (meta-apply 'eval-let*
	;; 				     (car (cdr exp)) (cdr (cdr exp)) env cont))
	;; ((operator? exp  'letrec) (meta-apply 'eval-letrec
	;; 				     (car (cdr exp)) (cdr (cdr exp)) env cont))
	;; ((operator? exp 'EM)	 (meta-apply 'eval-EM exp env cont))
	;; ((operator? exp 'exec-at-metalevel)
	;;  (meta-apply 'eval-EM exp env cont))
	;; ((operator? exp 'primitive-EM)
	;;  (meta-apply 'eval-primitive-EM exp env cont))

	;; ((operator? exp 'reset)	 (meta-apply 'eval-reset exp env cont))

	;; ((operator? exp  'exit)	 (meta-apply 'eval-exit exp env cont))
	;; ((operator? exp 'load)	 (meta-apply 'eval-load exp env cont))
	;; ((operator? exp 'and)	 (meta-apply 'eval-and (cdr exp) env cont))
	;; ((operator? exp 'or)	 (meta-apply 'eval-or (cdr exp) env cont))
	;; ((operator? exp 'delay)	 (meta-apply 'eval-delay exp env cont))
	;; ((operator? exp 'cons-stream)
	;;  (meta-apply 'eval-cons-stream exp env cont))
	(else (eval-application exp cont))))



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

(define (eval-load exp cont)
  (let ((current-port #f))
    (define (reader nth)
      (let ((input (read current-port)))
	(if (eof-object? input)
	    (begin
	      (close-port current-port)
	      (cont 'done))
	    (begin
	      (newline)
	      (display "in") (display "[") (display nth) (display "] : ")  	  
	      (display input)
	      (newline)
	      (base-eval input (lambda (result)
				 (display "out") (display "[") (display nth) (display "] : ")
				 (display result)
				 (newline)
				 (reader (+ nth 1))))))))
    (let ((filename-expr (cadr exp)))
      (base-eval filename-expr
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
;;
(define (eval-quasiquote exp cont)
  (define (qq-recur exp)
    (cond
     ((pair? exp)
      (cond
       ((eq? (car exp) 'unquote)
	(base-eval (cadr exp) (lambda (r) r)))
       (#t
	(cons (qq-recur (car exp))
	      (qq-recur (cdr exp)))
	)))
     (#t exp)))
  (cont (qq-recur (cadr exp))))




























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


(define (eval-let exp cont)
  (let* ((let-args (cadr exp)) ;; ((a 1 2 3)(b 2 ...)(c 3 ...))
	 (let-body (cddr exp)) ;; ...body ...
	 (let-args-syms (map car let-args)) ;;  (a b c)
	 (let-args-body (map cdr let-args)) ;; ((1 2 3) (2 ..)( 3 ...))
	 (let-args-body-vals (map (lambda (s) (eval-begin s (lambda (v) v)))
				  let-args-body)) ;; implicit begin on let-args-bodies	 
	 )
    
    ;; helpers
    (define (bind args vals)
      (cond
       ((null? args) '()) ;; done -- maybe we threw inputs away ?
       (#t
	(cons (cons (car args) (car vals))
	      (bind (cdr args) (cdr vals))))))
    ;; temporary extend environemnt
    (define (extend-env partial-env env)
      (append partial-env env))
    ;; 
    (let ((ext-env (extend-env (bind let-args-syms let-args-body-vals) $env))
	  (old-env $env))
      (set! $env ext-env)
      (eval-begin let-body		  
		  (lambda (result)
		    (set! $env old-env)
		    (cont result))))))



;; (begin .... )
;; begin keyword stripped before reach here ...
;; ------------------------------------------------------------------------------------
(define (eval-begin2 exp cont result)  
  (cond
   ((null? exp) (cont result))
   (#t (base-eval (car exp)
		  (lambda (result2)
		    (eval-begin2 (cdr exp) cont result2))))))

(define (eval-begin exp cont)
  (eval-begin2 exp cont #f))


;; ---------- evaluate sequence ---------------
(define (eval-seq2 xs ys cont)
  (cond
   ((null? xs) (cont (reverse ys)))
   (#t (base-eval (car xs)
		  (lambda (result)
		    (eval-seq2 (cdr xs) (cons result ys) cont))))))

(define (eval-seq xs cont)
  (eval-seq2 xs '() cont))


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


(define (eval-primitive-op op vals cont)
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
(define (eco-case123 lam-args vals lam-body cont)
  (define (bind args vals)
    (cond
     ((symbol? args) (cons args vals)) ;; slurp + done
     ((null? args) '()) ;; done -- maybe we threw inputs away ?
     (#t
      (cons
       (cons (car args) (car vals))
       (bind (cdr args) (cdr vals))))))
  
  (define (extend-env partial-env env)
    (append partial-env env))
  
  (let ((ext-env (extend-env (bind lam-args vals) $env))
	(old-env $env))
    (set! $env ext-env)
    ;; (format #t "lambda env : ~a~%" $env)
    ;; (format #t "lambda body : ~a~%" lam-body)    
    (eval-begin lam-body		  
		(lambda (result)
		  (set! $env old-env)
		  (cont result)))))




;; -------------------------------------------------------------------------------
;; function application
;;----------------------------------------------------------------
(define (eval-compound-op op vals cont)
  (let* ((lam (cadr op))
	 (lam-arg (cadr lam))
	 (lam-body (cddr lam)))
    (eco-case123 lam-arg vals lam-body cont)))


(define scheme-procedure? procedure?)
  
(define (primitive? op)
  (and (pair? op)
       (eq? (car op) 'primitive)
       (procedure? (cdr op))))


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

(define (eam-case123 mlam-args unargs mlam-body cont)
  (define (take xs n)
    (cond
     ((null? xs) '())
     ((< n 1) '(<--rest-of-env-->))
     (#t (cons (car xs) (take (cdr xs) (- n 1))))))

  ;; 
  ;; ( (a . b) . ... )
  (define (bind args vals)
    (cond
     ((symbol? args) (cons args vals)) ;; slurp + done
     ((null? args) '()) ;; done -- maybe we threw inputs away ?
     (#t (cons
	  (cons
	   (car args)
	   (cons (car vals)))
	  (bind (cdr args) (cdr vals))))))
  
  (define (extend-env partial-env env)
    (append partial-env env))
  
  (let* ((bindings (bind mlam-args unargs))
	 (ext-env (extend-env bindings $env))
	 (old-env $env))
    (set! $env ext-env)    
    (format #t "macro-body : ~a~%" mlam-body)    
    (format #t "macro env : ~a~%" (take $env 3)) ;; limit env to 3 entries ...
    (format #t "macro-bindings : ~a~%" bindings)    
    ;;(cont mlam-body) ;; --------- pass answer as body of macro unevaluated ...
    (eval-begin mlam-body		  
		(lambda (expanded-expression)
		  (set! $env old-env)
		  (format #t "\nmacro-expansion : ~a~%" expanded-expression)		  
		  (base-eval expanded-expression cont)))))
                  ;; instead of base-eval if we just pass
		  ;;(cont expanded-expression) <-- equivalent to macro-expand-1 


(define (eval-app-macro op unargs cont)
  (format #t "macro mlam : ~a~%" op)
  (let* ((mlam (cadr op))
	 (mlam-arg (cadr mlam))
	 (mlam-body (cddr mlam)))
    (eam-case123 mlam-arg unargs mlam-body cont)))

;; tidy stuff up above
;; equivalent macro-expand-1







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
(define (eval-application exp cont)
  ;;(format #t "ev-app ***~a~%" exp)
  (let ((op (car exp))
	(unargs (cdr exp)))
    (base-eval op (lambda (fun)
		    (cond
		     ((macro-closure? fun)
		      (eval-app-macro fun unargs cont))
		     ((primitive? fun)
		      (eval-seq unargs
				(lambda (vals)
				  (eval-primitive-op fun vals cont))))
		     (#t
		      (eval-seq unargs
				(lambda (vals)
				  (eval-compound-op fun vals cont)))))))))



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
(define (tag x y)
  (cons x y))

;; -----------closures + lambda ------------------------------
(define (closure? x)
  (and (pair? x)
       (eq? (car x) 'closure)))

(define (make-closure lam env)
  (tag 'closure (cons lam env)))

(define (eval-lambda exp cont)
  (cont (make-closure exp $env)))


;; ---------- macro-closure + mlambda ---------------------------------
(define (macro-closure? x)
  (and (pair? x)
       (eq? (car x) 'macro-closure)))

(define (make-macro-closure mlam env) ;;<<<<
  (tag 'macro-closure (cons mlam env)))

(define (eval-mlambda exp cont)
  (cont (make-macro-closure exp $env)))



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
(define (modify-env sym val e cont)
  (cond
   ((null? e) (error 'modify-env123 'not-found sym val $env cont))
   ((eq? sym (caar e))
    (set-cdr! (car e) val)
    (cont val))
   (#t (modify-env sym val (cdr e) cont))))

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
;; 
(define (eval-set! exp cont)
  (base-eval (third exp)
	     (lambda (result)
	       (let ((sym (second exp)))
		 (cond
		  ((eq? sym '$e)
		   (format #t "setting $e ***~a~%" result)
		   (set! $env result)
		   (cont result))
		  (#t		     
		   (modify-env sym result $env cont)))))))



;; passed just condition arg1 arg2
(define (eval-if exp cont)
  (let ((condition (car exp))
	(consequent (cadr exp))
	(alternative (caddr exp)))
    (base-eval condition
	       (lambda (result)
		 (if result
		     (base-eval consequent cont)
		     (base-eval alternative cont))))))



;; in the interpreted version liek to do this
;; and see $e as
;; ( (aa . 4) .... environment $e continues ....)
;; (set! $e (cons (cons (quote aa) 4) $e))
;;
;; evaluate variable in macro language
;; really looking at renaming the variable
;;
;;
(define (eval-var exp env cont)
  (cond
   ((eq? exp '$e) ; for debugging type $e then get environment shown
    (cont $env))
   (#t
    (cont (env-lookup exp env cont)))))



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
(define (eval-defmacro exp cont)
  (let* ((macro-name (cadr exp))
	 (macro-definition `(mlambda ,@(cddr exp)))
	 (macro-closure (make-macro-closure macro-definition $env)))
    (modify-or-extend-env macro-name
			  macro-closure
			  $env
			  cont)))

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
(define (eval-define exp cont)
  ;; eval the thing want to assign to a symbol 
  (base-eval (third exp)
	     (lambda (result)
	       (let ((sym (second exp)))
		 (modify-or-extend-env sym result $env cont)))))




(define (eval-quote exp cont)
  (cont exp))


;; some repl ...
(define (black-repl nth result cont)
  (let ((input (read)))
    (if (eof-object? input) (cont  (exit))
	(begin
	  (newline)
	  (display "in") (display "[") (display nth) (display "] : ")  	  
	  (display input)
	  (newline)
	  (base-eval input (lambda (result)
			     (display "out") (display "[") (display nth) (display "] : ")
			     (display result)
			     (newline)
			     (black-repl (+ nth 1) result cont)))))))


;; very similar to eval-begin ... strips begin ... just implicit sequence
;; do we want to print and count each thing though ...
(define (black-pre-repl cmds nth result cont)
  (cond
   ((null? cmds)
    (black-repl (+ nth 1) result cont))
   (#t
    (let ((input (car cmds)))
      (display "in") (display "[") (display nth) (display "] : ")
      (display input)
      (newline)
      (base-eval input (lambda (result)
			 (display "out") (display "[") (display nth) (display "] : ")
			 (display result)
			 (newline)
			 (black-pre-repl (cdr cmds) (+ nth 1) result cont)))))))



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
(define (black . cmds)
  (format #t "\n black macro:  sanity check v1.0 ~%")  
  (format #t "\n startup cmds = {~a}~%" cmds)
  (black-pre-repl cmds 0 #f (lambda (x) x)))

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

      
(set! $env
      `(     (a . 3)
	     (b . 4)
	     (c . 5)
	     (d . (1 2 3))
	     (e . (4 5 6))
	     (f . (7 8 9))
	     
	     (car . ,(make-prim car))
	     (cdr . ,(make-prim cdr))
	     (cons . ,(make-prim cons))
	     (primitive? . ,(make-prim primitive?))

	     (list . ,(make-prim list))
	     (append . ,(make-prim append))
	     
	     (+ . ,(make-prim +))
	     (- . ,(make-prim -))
	     (* . ,(make-prim *))
	     (/ . ,(make-prim /))

	     (> . ,(make-prim >))
	     (< . ,(make-prim <))
	     (= . ,(make-prim =))

	     (not . ,(make-prim not))
	     ;; and or --- macros ??
	     ;; when unless loop for
	     
	     ;; format
	     ($base-eval . ,base-eval)
	     ($env . ,$env)))




;; -----------------------------------------------------------------------------------







