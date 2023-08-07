
(define-module (base-eval)
  #:use-module (list-utility)
  #:use-module (environment)  
  #:use-module (closure)
  #:export (base-eval
	    repl
	    ))



;; (callcc (lambda (return) ... (return 5)))
;; (callcc f)
;; where f is a procedure takes one argument - the continuation
;;
;; inside interpreter f is an s expression tagged as closure
;; (closure (lambda ...) <env-ptr>)
;;
;; so in order get primitive-procedure cont from system lisp into interpreter
;; need to run apply for interpreter "made" compound procedures
;;
(define (eval-callcc exp env cont)
  (let ((fun (second exp)))
    (base-eval fun env (lambda (fn)
			 (eval-compound-op fn (list cont) env cont)))))





;;
;;
;;-------------------------------------------------------------------------------



(define (eval-seq xs env cont)
  (define (eval-seq2 xs ys env cont)
    (cond
     ((null? xs) (cont (reverse ys)))
     (#t (base-eval (car xs)
		    env
		    (lambda (result)
		      (eval-seq2 (cdr xs) (cons result ys) env cont))))))
    (eval-seq2 xs '() env cont))




(define (eval-quote exp env cont)
  (let ((quoted (second exp)))
    (cont quoted)))



(define (eval-var exp env cont)
  (cond
   ((eq? exp '$e) ;; $e backdoor - environment
    (cont env))
   ((eq? exp '$s) ;; $s backdoor - stack ? unused??
    (cont '()))   
   (#t
    (let ((find (lookup-env env exp)))
      (if (pair? find)
	  (cont (cdr find))
	  (list "eval-var variable not found" exp env cont))))))



;; (if CONDITION THEN then-false)
(define (eval-if exp env cont)
  (cond
   ((not (= 4 (length exp)))
    (format #f "eval-if takes 3 args given ~a" (- (length exp) 1)))
   (#t
    (let ((condition (second exp))
	  (consequent (third exp))
	  (alternative (fourth exp)))
      (base-eval condition
		 env
		 (lambda (result)
		   (if result
		       (base-eval consequent env cont)
		       (base-eval alternative env cont))))))))

   

;; test begin sequence is evaluated only once ie (begin (display "1")(display "2")(display "3"))
;; if see 1123 know 2nd item evaluated more than once.. bug..
(define (eval-implicit-begin exp env cont)
  (define (eval-implicit-begin2 exp env result cont)  
    (cond
     ((null? exp) (cont result))
     (#t (base-eval (car exp)
		    env
		    (lambda (result2)
		      (eval-implicit-begin2 (cdr exp) env result2 cont))))))
  (cond
   ((null? exp) "implicit begin needs something to do")
   (#t (base-eval (car exp)
		  env
		  (lambda (result2)
		    (eval-implicit-begin2 (cdr exp) env result2 cont))))))


;; (begin x)
;; must have one thing to do
(define (eval-begin exp env cont)
  (if (null? (cdr exp))
      "begin given no arguments to process"
      (eval-implicit-begin (cdr exp) env cont)))


;; eval the thing want to assign to a symbol
;; (define f x)
(define (eval-define exp env cont)
  (cond
   ((not (= 3 (length exp)))
    (format #f "eval-define takes 2 args given ~a" (length exp)))
   ((not (symbol? (second exp)))
    (format #f "eval-define requires a symbol name ~a" (second exp)))
   (#t
    (base-eval (third exp)
	       env
	       (lambda (result)
		 (let ((sym (second exp)))
		   (define-env! env sym result)
		   (cont result)))))))


(define (eval-set! exp env cont)
  (cond
   ((not (= 3 (length exp)))
    (format #f "eval-set! takes 2 args given ~a" (length exp)))
   ((not (symbol? (second exp)))
    (format #f "eval-set! requires a symbol name ~a" (second exp)))
   (#t
    (base-eval (third exp)
	       env
	       (lambda (result)
		 (let ((sym (second exp)))
		   (set-env! env sym result)
		   (cont result)))))))




;; eval-set!
;; error handling ??
;; debug support ??


;;------------------------------------------------------------------------------------


;; bit premature
;; but here is a simple quasiquote single level version
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



;; ;; this uses higher order functions...
;; (define (eval-let exp env cont)
;;   (let* ((let-args (cadr exp)) ;; ((a 1 2 3)(b 2 ...)(c 3 ...))
;; 	 (let-body (cddr exp)) ;; ...body ...
;; 	 (let-args-syms (map car let-args)) ;;  (a b c)
;; 	 (let-args-body (map cdr let-args)) ;; ((1 2 3) (2 ..)( 3 ...))
;; 	 (let-args-body-vals (map (lambda (s) (eval-implicit-begin s env #f (lambda (v) v)))
;; 				  let-args-body)) ;; implicit begin on let-args-bodies	 
;; 	 )    
;;     (let ((new-env (extend-env (arglist-bind let-args-syms let-args-body-vals) env)))
;;       (eval-implicit-begin let-body
;; 			   new-env
;; 			   #f
;; 			   (lambda (result)
;; 			     (cont result))))))



(define (pretty-tail exp)
  (cond
   ((null? exp) '())
   ((boolean? exp) (display exp))   
   ((number? exp) (display exp))
   ((closure? exp) (display "#<closure ")(write (second exp)) (display " >"))      
   ((pair? exp)
    (pretty (car exp))
    (cond
     ((null? (cdr exp)) '())
     ((pair? (cdr exp)) (display " ") (pretty-tail (cdr exp)))
     (#t (display " . ") (pretty-tail (cdr exp)))))
   ((symbol? exp) (display exp))
   ((string? exp) (write exp))
   ;;((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ;;((closure? exp) (display "#<closure ")(write (second exp)) (display " >"))
   ;;((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">"))))

(define (pretty exp)
  (cond
   ((null? exp) (display '()))
   ((boolean? exp) (display exp))   
   ((number? exp) (display exp))
   ((symbol? exp) (display exp))
   ((string? exp) (write exp))  
   ;;((macro-closure? exp) (display "#<mclosure ")(write (second exp)) (display " >"))
   ((closure? exp) (display "#<closure ")(write (second exp)) (display " >"))   
   ((and (pair? exp) (eq? (car exp) 'quote)) ;; ?
    (display "'") (pretty-tail (cdr exp)))   
   ((pair? exp) (display "(") (pretty-tail exp) (display ")"))
   ;;((procedure? exp) (display exp));; internal procedure   
   (#t (display "#<???") (write exp) (display ">"))))


(define (eval-lambda exp env cont)
  (cont (make-closure exp env)))


(define primitive? procedure?)

(define (eval-primitive-op proc vals env cont)
  (cont (apply proc vals)))

;; ------------- may need these -----------------------
(define (arglist-bind args vals)
  (cond
   ((symbol? args) (cons (cons args vals) '())) ;; slurp + done
   ((null? args) '()) ;; done -- maybe we threw inputs away ?
   (#t
    (cons
     (cons (car args) (car vals))
     (arglist-bind (cdr args) (cdr vals))))))

;; (define (extend-env partial-env env)
;;   ;; (write "\npartial-env:")
;;   ;; (write partial-env)
;;   (make-enviro (append partial-env (enviro-unpack env))))


(define (eval-compound-op cloz vals env cont)
  (let* ((lam (closure-lam cloz))
	 (lam-env (closure-env cloz))
	 (lam-args (closure-args cloz))
	 (lam-body (closure-body cloz)))
    (let* ((bindings (arglist-bind lam-args vals))
	   (new-env (fresh-env lam-env bindings )))
      ;; (format #t "lam-body ~a~%" lam-body)
      (eval-implicit-begin
       lam-body
       new-env       
       cont))))



(define (eval-application exp env cont)
  (let ((op (car exp))
	(unargs (cdr exp)))
    (base-eval op
	       env
	       (lambda (fun)
		 (cond
		  ;; ((macro-closure? fun)
		  ;;  (eval-app-macro fun unargs env cont))
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



(define (base-eval exp env cont)
  (cond
   ((number? exp)   (cont exp)) 
   ((boolean? exp)  (cont exp)) 
   ((string? exp)   (cont exp))   
   ((symbol? exp)   (eval-var exp env cont))
   ((closure? exp)  (cont exp))
   ((and (pair? exp)(eq? (car exp) 'quote))   (eval-quote exp env cont))
   ((and (pair? exp)(eq? (car exp) 'if))      (eval-if exp env cont))
   ((and (pair? exp)(eq? (car exp) 'begin))   (eval-begin exp env cont))
   ((and (pair? exp)(eq? (car exp) 'define))  (eval-define exp env cont))
   ((and (pair? exp)(eq? (car exp) 'set!))    (eval-set! exp env cont))
   ((and (pair? exp)(eq? (car exp) 'lambda))  (eval-lambda exp env cont))
   ((and (pair? exp)(eq? (car exp) 'quasiquote))  (eval-quasiquote exp env cont))
   ((and (pair? exp)(eq? (car exp) 'callcc))   (eval-callcc exp env cont))
   ;; ((and (pair? exp)(eq? (car exp) 'let))  (eval-let exp env cont))   
   ;; ((pair? exp)     (base-eval-pair exp env cont))
   ((pair? exp) (eval-application exp env cont))
   (#t (list "base-eval operator not recognised" exp))))



(define initial-environment '())

;; missing env
(define (repl)
  (newline)
  ;; (display "ready>") (display "[??") ;;(display nth)
  ;; (display "] : ")
  (let ((input (read)))
    (cond
     ((eof-object? input) (exit)) ;; hard exit
     (#t
      ;;(display input)
      (pretty input)
      (newline)
      (base-eval input initial-environment
		 (lambda (result)
		   (display "out") (display "[??") ;; (display nth)
		   (display "] : ")
		   (pretty result)
		   (newline)
		   (repl)))))))





(set! initial-environment (vector
			   `((a . 3)
			     (b . 4)
			     (c . 5)
			     (cons . ,cons)
			     (car . ,car)
			     (cdr . ,cdr)
			     (+ . ,+)
			     (* . ,*)
			     (- . ,-)
			     (< . ,<)
			     (= . ,=)
			     (> . ,>)
			     (mod . ,modulo)
			     (null? . ,null?)
			     (pair? . ,pair?)
			     (eq? . ,eq?)
			     (append . ,append)			     
			     (list . ,list)
			     (display . ,display)
			     (write . ,write)
			     (newline . ,newline)
			     (format . ,format)
			     (length . ,length)
			     )))



