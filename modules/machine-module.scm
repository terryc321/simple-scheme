

;; github terryc321/simple-scheme ...
;;(add-to-load-path "/home/terry/simple-scheme/src/")
;;(add-to-load-path "/home/terry/simple-scheme/macros/")

;; expand loads other macros cond let let* and or 
;;(load "/home/terry/simple-scheme/macros/expand.scm")

;; (define-module (machine)
;;   #:use-module (list-utility)
;;   #:use-module (environment)  
;;   #:use-module (closure)
;;   #:use-module (derived)  ;; now called macro-expander-module ...for own macro expansions 
;; ;;  #:use-module (ice-9 pretty-print) ;; circular lists cause confusion
;;   #:export (run))


;; for emacs input-output
(define (flush)
  (force-output))



;; lisp machine

;; to execute a jump instruction
;; (cont)
;; (save-continue) = push cont onto stack
;; (restore-continue) = pop + assign to cont
;;
;; (save-environment) = push
;; (restore-environment) = pop + assign to environment
;;


;; expr
;; env
;; cont
;; args
;; val
;; stack

;;
;; types represented as proper lists
;; ('environment env-pairs)
;; environment-tag => first
;; env-pairs=> second
;; cdr (cdr => null the empty list '()
;;

;; eval =
;; apply = 
;; if = conditions
;; begin = sequence
;; set!
;; define


;; environment proper lists
;; nice thing here , is if want new language , simply change the names
;; but thats like english to russian
;; now understand two languages instead of one , doing the same thing , quadruple effort

;; generally macros cant get away from
;; cant import OR or AND definitions ...
;;
;; cant use primitive map - looking for primitive func and 
(define initial-environment
  (lambda ()
    (make-env `((a . 3)
		(b . 4)
		(c . 5)
		(+ . ,+)
		(- . ,-)
		(/ . ,/)
		(* . ,*)
		(> . ,>)
		(< . ,<)
		(= . ,=)
		(cons . ,cons)
		(car . ,car)
		(cdr . ,cdr)
		(null? . ,null?)
		(eq? . ,eq?)
		(length . ,length)
		(reverse . ,reverse)
		(append . ,append)
		;; (map . ,map)
		;; (filter . ,filter)
		(list . ,list)
		(symbol? . ,symbol?)
		(number? . ,number?)
		(integer? . ,integer?)
		(real? . ,real?)
		(not . ,not)
		(display . ,display)
		(newline . ,newline)
		(string? . ,string?)
		(write . ,write)
		(<= . ,<=)
		(>= . ,>=)
		(pair? . ,pair?)
		(mod . ,modulo)
		(format . ,format)
		(vector-ref . ,vector-ref)
		(vector-set! . ,vector-set!)
		(vector . ,vector)
		(gensym . ,gensym)
		))))




;; (first = (a 3)
;; (first (first = a
;; (second (first = 3
;; extend environment ... properly so everything has proper lists ...
;; (list 'environment (cons (list key value) old-env-key-values))

;; registers
(define cont 0)
(define expr 0)
(define stack 0)
(define env 0)
(define unargs 0)
(define op 0)
(define args 0)
(define val 0)
(define port 0) ;;file port for reading files

(define halt
  (lambda ()
    (newline)
    (display "HALT : machine halted")
    (newline)
    (flush)))


;;----------------------------------------------------------------------------------------
;; lookup variable in current environment
;; assign value to val
;; jump to continue
;; never return... either HALT or jump to where value needed
(define m-var
  (lambda ()
    (cond
     ((eq? expr '$e)
      (set! val env)
      (cont))
     ((eq? expr '$s)
      (set! val stack)
      (cont))     
     (#t ;; do proper lookup
      (let ((look (lookup-env env expr)))
	(if (pair? look)
	    (begin (set! val (cdr look))
		   (cont))
	    (begin (newline)
		   (display "VARIABLE NOT FOUND IN ENVIRONMENT :") (display expr) (newline)
		   (display "ENVIRONMENT :") (display env) (newline)
		   (halt))))))))


;;--------------------------------------------------------------------------------------

(define lambda?
  (lambda (e)
    (and (pair? e) (eq? (car e) 'lambda))))


;; make a closure
(define m-lambda
  (lambda ()
    ;;(set! val (list 'closure expr env))
    (set! val (make-closure expr env))
    (cont)))


;; save + restore 
(define save
  (lambda (e)
    (set! stack (cons e stack))))

;; ignoring stack is empty ..

;; args register args. 
(define (save-args)
  (set! stack (cons args stack)))
(define (restore-args)
  (set! args (car stack))
  (set! stack (cdr stack)))


;; unargs register unargs. 
(define (save-unargs)
  (set! stack (cons unargs stack)))
(define (restore-unargs)
  (set! unargs (car stack))
  (set! stack (cdr stack)))

;; continuation register cont
(define (save-cont)
  (set! stack (cons cont stack)))
(define (restore-cont)
  (set! cont (car stack))
  (set! stack (cdr stack)))

;; environment register env
(define (save-env)
  (set! stack (cons env stack)))
(define (restore-env)
  (set! env (car stack))
  (set! stack (cdr stack)))


;; expression register expr
(define (save-expr)
  (set! stack (cons expr stack)))
(define (restore-expr)
  (set! expr (car stack))
  (set! stack (cdr stack)))


;; -------------- crappy pretty printer ------------------------------------------
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
   ;;((environment? exp) (display "#<environment ") (display " >"))
   ;;((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ((closure? exp) (display "#<closure ")(display " >"))
   ((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">"))))


(define (pretty exp)
  (cond
   ((null? exp) (display '()))
   ((boolean? exp) (display exp))   
   ((number? exp) (display exp))
   ((symbol? exp) (display exp))
   ((string? exp) (write exp))
   ;;((environment? exp) (display "#<environment ") (display " >"))   
   ;;((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ((closure? exp) (display "#<closure ")(display " >"))   
   ((and (pair? exp) (eq? (car exp) 'quote)) ;; ?
    (display "'") (pretty-tail (cdr exp)))   
   ((pair? exp) (display "(") (pretty-tail exp) (display ")"))
   ((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">")))
  )

(define pretty-print pretty)

;;------------------------------------------------------------------------------------
;; expr (cloz a b c)
;; evaluate operator and all arguments
;; then do function application
;;
;; save any
;; expr is a list (fn arg1 arg2 arg3)
;; want to translate that into
;; (eval'd fn eval'd arg1 eval'd arg2 ...)
;;
;; save unargs
;; unargs = '()
;;
;;
;; restore unargs
;; do the call
;; this way preserve tail call and stack does not grow indefinitely
;;

;; look at what registers got clobbered and save them just in case
;; as it may be deeply nested recursion , likely all of them are going to be needed to be preserved
;; by saving on to the stack
(define m-apply
  (lambda ()
    (save-unargs)
    (save-args)
    (save-cont)
    (save-env)
    
    (set! unargs expr)
    (set! args '())
    (set! cont m-args-2)
    (m-args-1)
    ))

;; if unargs empty , no more args to evaluate
;; take one item from unargs
;; put it into expr
;; run m-eval
;; like to return to m-args-2
(define m-args-1
  (lambda ()
    (cond
     ((null? unargs) ;; no more args to compute
      (set! args (reverse args))
      (m-args-3))
     (#t
      (set! expr (car unargs))
      (set! unargs (cdr unargs))
      (m-eval)))))

;; evaluated an argument - add it to list
;; cont likely already m-args-2 but set it just in case
;;
(define m-args-2
  (lambda ()
    (set! args (cons val args))
    (set! cont m-args-2)
    (restore-env) ;; --- restore original environment after eval arg
    (save-env)
    (m-args-1)
    ))

(define m-args-3
  (lambda ()
    (set! expr args)
        
    ;; clean up stack in reverse order
    (restore-env)
    (restore-cont)
    (restore-args)
    (restore-unargs)

    ;; the call
    (cond
     ((primitive-procedure? (first expr))
      (primitive-apply (first expr) (rest expr)))
     (#t
      (compound-apply)
      ))))



(define (arglist-bind args vals)
  (cond
   ((symbol? args) (cons (cons args vals) '())) ;; slurp + done
   ((null? args) '()) ;; done -- maybe we threw inputs away ?
   (#t
    (cons
     (cons (car args) (car vals))
     (arglist-bind (cdr args) (cdr vals))))))



(define compound-apply
  (lambda ()
    ;; assume clozure?
    ;; (display "compound-apply : applying clozure")(newline)
    ;; (write expr)
    ;; (newline)
    ;; (newline)
    ;; (newline)
    ;; (newline)
    
    (let* ((cloz (car expr))
	   (lam      (closure-lam cloz))
	   (lam-args (closure-args cloz))
	   (lam-body (closure-body cloz))
	   (lam-env  (closure-env  cloz))	   
	   (vals (cdr expr))
	   (bindings (arglist-bind lam-args vals))
	   (new-env (fresh-env lam-env bindings)))

      
      ;; (newline)
      ;; (display "\nnew environment : ")(write new-env)
      ;; (newline)
      ;; (display "\nlambda body : ")(write lam-body)
      ;; (display "\n")

      ;;---------------
      ;; (save-cont)
      ;; (save-env)
      ;; (save-expr)

      ;; first expression will do
      (set! expr lam-body)
      (set! env new-env)
      ;;(set! cont compound-return)
      ;;(m-eval)
      (m-begin-implicit)
      
      )))


;; something to do will tail calls , just jump they dont return ...
(define compound-return ;; --- never get here ????....
  (lambda ()
    ;; restore in reverse order put on stack
    (restore-expr)
    (restore-env)
    (restore-cont)
    ;; it returned ?
    (cont)
    ))


;; use underlying system to do the primitive apply for us
;; goto continue when done
(define primitive-apply
  (lambda (fn vals)
    (set! val (apply fn vals))
    (cont)))

(define primitive-procedure?
  (lambda (p)
    (procedure? p)))



;; sequence
;; (begin ...)
(define begin? (lambda (e) (and (pair? e) (eq? (car e) 'begin))))

(define m-begin
  (lambda ()
    ;; drop leading begin keyword
    (set! expr (cdr expr))
    (cond
     ((null? expr)
      (newline)
      (display "BEGIN - GIVEN EMPTY CLAUSE :") (display expr) (newline)
      (halt))
     (#t
      (m-begin-implicit)))))
     

(define m-begin-implicit
  (lambda ()
    (cond
     ((null? (cdr expr)) ;; last expression -- set expr 1st item of begin,go eval
      (set! expr (car expr))
      (m-eval))
     (#t
      (save-cont)
      (set! cont m-begin-2)      
      (save-expr)
      (save-env) ;;------
      (set! expr (car expr))
      (m-eval)))))
  

(define m-begin-2
  (lambda ()
    (restore-env) ;;---- ??
    (restore-expr)
    (set! expr (cdr expr))
    (cond
     ((null? (cdr expr)) ;; last expression in begin
      (set! expr (car expr))
      (restore-cont)
      (m-eval))
     (#t      
      (save-expr)
      (save-env) ;; ----? 
      (set! expr (car expr))
      (set! cont m-begin-2) ;was already no?
      (m-eval)))))

(define m-begin-3 ;; never get here ??
  (lambda ()
    (restore-cont)
    (cont)    
    ))
    





;; expr = (if a b c)
(define if? (lambda (e) (and (pair? e) (eq? (car e) 'if))))				       
(define m-if
  (lambda ()
    ;; drop leading begin keyword
    (save-cont)
    (save-expr)
    (save-env) ;; ----- restore envi
    (set! expr (second expr))
    (set! cont m-if-2)
    (m-eval)
    ))
(define m-if-2
  (lambda ()
    ;; drop leading begin keyword
    (restore-env)
    (restore-expr)
    (restore-cont)
    (if val
	(begin
	  (set! expr (third expr))
	  (m-eval))
	(begin
	  (set! expr (fourth expr))
	  (m-eval)))))



;; lambda expression - (lambda arglist ...body...) one not been evaluated to closure yet.
;; closures evaluate to themselves
;; primitive procedures from host language are themselves
(define m-eval
  (lambda ()
    (cond
     ((number? expr)  (set! val expr) (cont))
     ((boolean? expr) (set! val expr) (cont))     
     ((string? expr) (set! val expr) (cont))     
     ((closure? expr) (set! val expr) (cont))
     ((primitive-procedure? expr) (set! val expr) (cont))
     ((symbol? expr)  (m-var))
     ((lambda? expr)  (m-lambda))
     ((if? expr) (m-if))
     ((define? expr) (m-define))
     ((set!? expr) (m-set!))
     ((quote? expr) (m-quote))     
     ((begin? expr) (m-begin))
     ;; --- should this be here ?? ---
     ((load? expr) (m-load))
     ;; application
     ((pair? expr) (m-apply))
     (#t (halt))      
    )))

;; (define unpack-environment
;;   (lambda (e) (second e)))

;; ;; change a value or extend environment
;; (define mutate-environment
;;   (lambda (sym v)
;;     (define recur
;;       (lambda (ys)
;; 	(cond
;; 	 ((null? ys) (set! env (extend-env sym v env)))
;; 	 ((eq? sym (first (first ys))) (set-car! ys (list sym v)))
;; 	 (#t (recur (cdr ys))))))
;;     (recur (unpack-environment env))))



;; (define mutate-only-environment
;;   (lambda (sym v)
;;     (define recur
;;       (lambda (ys)
;; 	(cond
;; 	 ((null? ys)
;; 	  (newline)
;; 	  (display "SET! VARIABLE NOT FOUND IN ENVIRONMENT :") (display sym) (newline)
;; 	  (halt))
;; 	 ((eq? sym (first (first ys))) (set-car! ys (list sym v)))
;; 	 (#t (recur (cdr ys))))))
;;     (recur (unpack-environment env))))


(define set!? (lambda (e) (and (pair? e) (eq? (car e) 'set!))))				       
;; expr = (define sym expr)
(define m-set!
  (lambda ()
    ;;(display "EXPR = ") (display expr) (newline)
    
    (save-env) ;; ---- 
    (save-expr)
    (save-cont)
    
    (set! expr (third expr))
    (set! cont m-set!-2)
    ;;(display "toEXPR = ") (display expr) (newline)
    (m-eval)
    ))

;; value in val
;; 
(define m-set!-2
  (lambda ()
    (restore-cont)
    (restore-expr)
    (restore-env) ;; -----
    
    ;;(display "toVAL = ") (pretty val) (newline)
    ;; mutate environment
    ;;(display "fromEXPR = ") (pretty expr) (newline)
    (set! expr (second expr))
    ;;(display "symEXPR = ") (pretty expr) (newline)
    ;; if just extend env , no need to do set-car! or search for binding
    ;;(mutate-only-environment expr val)

    ;; side effects environment env with mutating the binding
    ;; expr <- val
    ;; val still holds result of evaluation
    (let ((has-set (set-env! env expr val)))
      (if has-set
	  #t
	  (begin
	    (newline)
	    (display "SET! VARIABLE NOT FOUND IN ENVIRONMENT :") (display expr) (newline)
	    (display "ENVIRONMENT :") (display env) (newline)
	    (halt)
	    )))
    
    ;;(display "newENV = ") (pretty env) (newline)    
    ;;(set! env (extend-env (list (list expr val)) env))
    (cont)
    ))



(define quote? (lambda (e) (and (pair? e) (eq? (car e) 'quote))))				       
;; expr = (quote x)
(define m-quote
  (lambda ()
    (set! val (second expr))
    (cont)
    ))


(define define? (lambda (e) (and (pair? e) (eq? (car e) 'define))))				       
;; expr = (define sym expr)
(define m-define
  (lambda ()
    ;;(display "EXPR = ") (display expr) (newline)
    (save-env) ;;; --------- save env 
    (save-expr)
    (save-cont)
    ;; making 
    ;;(set! env (extend-env (second expr) #f env))
    
    ;;(mutate-environment (second expr) #f) ;;; ????
    
    (set! expr (third expr))
    (set! cont m-define-2)
    ;;(display "toEXPR = ") (display expr) (newline)
    (m-eval)
    ))

;; value in val
;; 
(define m-define-2
  (lambda ()
    (restore-cont)
    (restore-expr)
    (restore-env) ;; ------- restore env
    
    ;;(display "toVAL = ") (pretty val) (newline)
    ;; mutate environment
    ;;(display "fromEXPR = ") (pretty expr) (newline)
    (set! expr (second expr))
    ;;(display "symEXPR = ") (pretty expr) (newline)
    ;; if just extend env , no need to do set-car! or search for binding
    ;;(mutate-environment expr val)

    ;; define either mutates or creates a new binding visible in environment
    (define-env! env expr val)    
    ;;(display "newENV = ") (pretty env) (newline)    
    ;;(set! env (extend-env (list (list expr val)) env))
    (cont)
    ))





;; print val register
;; jump to continue
(define m-print
  (lambda ()
    (newline)
    (display "out : ")
    (pretty-print val)
    (newline)
    (display "stk : ")
    (display (length stack))
    (newline)
    (flush) ;; 
    (cont)
    ))

;; read a value from somewhere into expr register for ease of use
;; goto cont
(define m-read
  (lambda ()
    (set! expr (read))
    (if (eof-object? expr)
	(exit) ;; abort!
	#f)
    (display "\nin : ")
    (write expr)
    (newline)
    (set! expr (expand-derived expr))
    (newline)    
    (cont)))


;; set next thing to do is m-repl2
;; now goto read
(define m-repl ;;------ SAVE = RESTORE ENV around repl??
  (lambda ()
    (set! cont m-repl2)
    (m-read)
    ))

(define m-repl2
  (lambda ()
    (set! cont m-repl3)
    (save-env)
    (m-eval)
    ))

(define m-repl3
  (lambda ()
    (set! cont m-repl)
    (restore-env)
    (m-print)
    ))


;; ----------- error handling required for file input/output was painful -----------------
(define (catch-all thunk)
  (with-exception-handler
    (lambda (exn)
      (format (current-error-port)
              "Uncaught exception: ~s\n" exn)
      #f)
    thunk
    #:unwind? #t))

;; just loading one file , no chained loading of files , probably for best as cause awful cascade loop
;; load from file
;; (load "some file")
(define load? (lambda (e) (and (pair? e) (eq? (car e) 'load))))				       

;; clobber PORT , EXPR 
;; error handling code is horrible ...
(define m-load ;; entrance
  (lambda ()
    (set! expr (second expr))
    (save-expr)
    (save-cont)
    
    ;; try open file    
    (if (catch-all
	 (lambda () (set! port (open-input-file expr))))
	(begin
	  (pretty-print expr)(pretty " is a valid file port")(flush))
	(begin
	  (pretty-print expr)(pretty " is a NOT valid file port")(flush)))
    
    (m-load-repl2)
    ))


(define m-load-repl2 ;; read  ---------- may need to save env / restore env between reads + evals ...?...
  (lambda ()
    (set! cont m-load-repl3)

    ;; read from our file 
    (set! expr (read port))
    (if (eof-object? expr)
	(m-load-repl5) ;;done
	#f)
    (display "\nin : ")
    (pretty-print expr)
    (newline)
    (set! expr (expand-derived expr))
    (newline)

    (save-env);; ---
    
    (m-eval) ;; eval -> m-load-repl3
    ))


(define m-load-repl3 ;; print
  (lambda ()
    ;; result is in VAL register , so print it
    (set! cont m-load-repl2)
    (restore-env) ;; --- 
    (m-print)
    ))

(define m-load-repl5 ;; exit - done loading file
  (lambda ()
    (restore-cont)
    (restore-expr)
    (if (port? port)
	(begin
	  ;;(pretty "closing file port : ") (pretty-print expr)(newline)(flush)
	  (close-port port)
	  (flush)
	  )
	#f)    
    (cont) ;; done
    ))

;; ------------------------------------------------------------------------------------------------


(define machine-run
  (lambda ()
    (set! env (initial-environment))
    (set! expr 'a)
    (set! stack '())
    (set! val 0)
    ;; m-repl handle continue
    (m-repl)
    ))

(define run machine-run)

