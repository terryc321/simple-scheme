


;; bug : redefining routine later on in program , deletes one looking at and causes spurious behaviour
;; layers :: symbol to be locked , unabled to be redefined

(define (cadr x) (car (cdr x)))
(define (caar x) (car (car x)))
(define (cdar x) (cdr (car x)))
(define (caddr x) (car (cdr (cdr x))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))

;;(define *the-primitives* (list))
(define $env (list))

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
	((eq? (car exp) 'quote)  (eval-quote (car (cdr  exp))    cont))
	((eq? (car exp) 'define) (eval-define (cdr exp)          cont))
	((eq? (car exp) 'defmacro) (eval-defmacro (cdr exp)      cont))	
	((eq? (car exp) 'if) (eval-if (cdr exp)                  cont))
	((eq? (car exp) 'set!)	 (eval-set! (cdr exp)            cont))
	((eq? (car exp) 'lambda) (eval-lambda exp                cont))
	((eq? (car exp) 'mlambda) (eval-mlambda exp              cont))	
	((eq? (car exp) 'begin)  (eval-begin (cdr exp)           cont))
	;; ;;((eq? (car exp) 'cond)	 (meta-apply 'eval-cond (cdr exp) env cont))
	
	;; ((operator? exp 'let)	 (meta-apply 'eval-let
	;; 				     (car (cdr exp)) (cdr (cdr exp)) env cont))
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


;; cons car cdr ... evaluate arguments
;; (define (eval-application exp env cont)
;;   (cond

;;(define (eval-list 
;; evaluate all items in list
;;
;; because arguments are not usually very long
;; we will cons them up ,
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
     (#t (cons (car args) (cons (car vals)
				(bind (cdr args) (cdr vals)))))))
  (define (extend-env partial-env env)
    (append partial-env env))
  (let ((ext-env (extend-env (bind lam-args vals) $env))
	(old-env $env))
    (set! $env ext-env)
    (eval-begin lam-body		  
		(lambda (result)
		  (set! $env old-env)
		  (cont result)))))


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
					
(define (eam-case123 mlam-args unargs mlam-body cont)
  (define (bind args vals)
    (cond
     ((symbol? args) (cons args vals)) ;; slurp + done
     ((null? args) '()) ;; done -- maybe we threw inputs away ?
     (#t (cons (car args) (cons (car vals)
				(bind (cdr args) (cdr vals)))))))
  (define (extend-env partial-env env)
    (append partial-env env))
  
  (let ((ext-env (extend-env (bind mlam-args unargs) $env))
	(old-env $env))
    (set! $env ext-env)
    (eval-begin mlam-body		  
		(lambda (result)
		  (set! $env old-env)
		  (cont result)))))

(define (eval-app-macro op unargs cont)
  (let* ((mlam (cadr op))
	 (mlam-arg (cadr mlam))
	 (mlam-body (cddr mlam)))
    (eam-case123 mlam-arg unargs mlam-body cont)))



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
  (let ((op (car exp))
	(unargs (cdr exp)))
    (base-eval op (lambda (result)
		    (cond
		     ((macro-closure? result)
		      (eval-app-macro op unargs cont))
		     ((primitive? result)
		      (eval-seq unargs
				(lambda (vals)
				  (eval-primitive-op op vals cont))))
		     (#t
		      (eval-seq unargs
				(lambda (vals)
				  (eval-compound-op op vals cont)))))))))



  
  ;; (eval-seq exp (lambda (op-vals)
  ;; 		  (let ((op (car op-vals))
  ;; 			(vals (cdr op-vals)))
  ;; 		    ;; (format #t "eval-app : operator = ~a\n" op)
  ;; 		    ;; (format #t "eval-app : vals = ~a\n" vals)
  ;; 		    (cond
  ;; 		     ((primitive? op) (eval-primitive-op op vals cont))
  ;; 		     (#t (eval-compound-op op vals cont)))))))




(define (compound? op)
  (and (not (primitive? op))
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

(define (make-macro-closure lam env)
  (tag 'macro-closure (cons lam env)))

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
(define (modify-env sym val e cont)
  (cond
   ((null? e) (error 'modify-env 'not-found sym val $env cont))
   ((eq? sym (car e))
    (set-car! (cdr e) val)
    (cont val))
   (#t (modify-env sym val (cdr (cdr e)) cont))))
     
;;
;;   set!
;; (a 3)
;;  car = a
;;  cadr = 3
(define (eval-set! exp cont)
  (base-eval (cadr exp)
	     (lambda (result)
	       (let ((sym (car exp)))
		 (modify-env sym result $env cont)))))




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



;; environment is a simple flat list
;; true environment $env
;; env : (key value key value key value ...)
;;
(define (env-lookup exp env cont)
  (cond
   ((null? env) (error 'env-lookup 'variable 'not-found! exp env))   
   ((eq? exp (car env)) (car(cdr env)))
   (#t (env-lookup exp (cdr (cdr env)) cont))))




;; evaluate variable in macro language
;; really looking at renaming the variable 
(define (eval-var exp env cont)
  (cond
   ((eq? exp '$env)
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
;; (defmacro f 
;;
(define (eval-defmacro exp cont)
  ;; (define a 3)
  ;; define expression stripped define before
  ;; just given stripped (a 3)
  ;; car  : a
  ;; cadr : 3
  ;; e iterates over $env global ptr
  (define (modify-or-extend-env sym val e cont)
    (define (extend-env sym val env)
      (cons sym (cons val env)))
    (cond
     ((null? e)
      (set! $env (extend-env sym val $env))
      (cont val))    
     ((eq? sym (car e))
      (set-car! (cdr e) val)
      (cont val))
     (#t (modify-or-extend-env sym val (cdr (cdr e)) cont))))
  ;; ----- only support (define f ....) at the moment ...
  ;; (define (f . x) body ...) is a lambda expression
  ;; (define f (lambda ... body))
  ;; ------------------------------------------------
  ;; (define a 3)
  ;;         (a 3)  is exp   , define got stripped
  ;;  (cadr : 3
  ;;  (car  : a
  (base-eval (cadr exp)
	     (lambda (result)
	       (let ((sym (car exp)))
		 (modify-or-extend-env sym result $env cont)))))









;; define can be desugar-ed in various ways
(define (eval-define exp cont)
  ;; (define a 3)
  ;; define expression stripped define before
  ;; just given stripped (a 3)
  ;; car  : a
  ;; cadr : 3
  ;; e iterates over $env global ptr
  (define (modify-or-extend-env sym val e cont)
    (define (extend-env sym val env)
      (cons sym (cons val env)))
    (cond
     ((null? e)
      (set! $env (extend-env sym val $env))
      (cont val))    
     ((eq? sym (car e))
      (set-car! (cdr e) val)
      (cont val))
     (#t (modify-or-extend-env sym val (cdr (cdr e)) cont))))
  ;; ----- only support (define f ....) at the moment ...
  ;; (define (f . x) body ...) is a lambda expression
  ;; (define f (lambda ... body))
  ;; ------------------------------------------------
  ;; (define a 3)
  ;;         (a 3)  is exp   , define got stripped
  ;;  (cadr : 3
  ;;  (car  : a
  (base-eval (cadr exp)
	     (lambda (result)
	       (let ((sym (car exp)))
		 (modify-or-extend-env sym result $env cont)))))


(define (eval-quote exp cont)
  (cont exp))


;; some repl ...
(define (black-repl nth result cont)
  (newline)
  (display "in") (display "[") (display nth) (display "] : ")
  (let ((input (read)))
    (display input)
    (newline)
    (base-eval input (lambda (result)
			   (display "out") (display "[") (display nth) (display "] : ")
			   (display result)
			   (newline)
			   (black-repl (+ nth 1) result cont)))))

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
  (format #t "\n black macro: welcome ~%")  
  (format #t "\n startup cmds = {~a}~%" cmds)
  (black-pre-repl cmds 0 #f (lambda (x) x)))

  ;; (error 'we 'finding 'out 'wat 'cmds 'means)
  ;; (black-repl 0 #f (lambda (x) x)))




;; -----------------------------------------------------------------------------------
;; setup the initial environment now everything is in scope for a scheme interpreter
;; reading top down source code
;;
(set! $env
  (list 'a 3
	 'b 4
	 'c 5
	 'car (make-prim car)
	 'cdr (make-prim cdr)
	 'cons (make-prim cons)
	 'primitive? (make-prim primitive?)
	 
	 '+ (make-prim +)
	 '- (make-prim -)
	 '* (make-prim *)
	 '/ (make-prim /)

	 '> (make-prim >)
	 '< (make-prim <)
	 '= (make-prim =)

	 'not (make-prim not)
	 ;; and or --- macros ??
	 ;; when unless loop for
	 
	 ;; format
	 
	 '$env $env))

;; -----------------------------------------------------------------------------------







