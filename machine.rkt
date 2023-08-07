#lang r5rs
(define (exit) #t)

;; using procedures with returns here ...
;; bit of a cheat sheet
;; 


(define fourth (lambda (x) (car (cdr (cdr (cdr x))))))
(define third (lambda (x) (car (cdr (cdr x)))))
(define second (lambda (x) (car (cdr x))))
(define first (lambda (x) (car x)))
(define rest (lambda (x) (cdr x)))



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

;;
(define make-empty-environment
  (lambda ()
    (list 'environment '())))

;; environment proper lists
(define initial-environment
  (lambda ()
    (list 'environment `((a 3)(b 4)(c 5)(+ ,+)(- ,-)(/ ,/)(* ,*)(> ,>)(< ,<)(= ,=)))))

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

(define halt
  (lambda ()
    (newline)
    (display "HALT : machine halted")
    (newline)))


(define env-key-values (lambda (ev) (second ev)))

(define env-lookup
  (lambda (sym env-given)
    (define (recur ys)
      (cond
       ((null? ys)
	(display "VARIABLE NOT FOUND :") (display sym) (newline)
	(display "QUESTIONABLE ENV :") (write env) (newline)(newline)
	(halt))
       ((eq? sym (first (first ys))) (set! val (second (first ys))) (cont))
       (#t (recur (cdr ys)))))
    (recur (unpack-environment env-given))))


(define (arglist-bind args vals)
  (cond
   ((symbol? args) (cons (list args vals) '())) ;; slurp + done
   ((null? args) '()) ;; done -- maybe we threw inputs away ?
   (#t
    (cons
     (list (car args) (car vals))
     (arglist-bind (cdr args) (cdr vals))))))

(define pack-environment
  (lambda (ev) (list 'environment ev)))

(define (extend-env sym vel ev)
  (pack-environment (cons (list sym vel)
			  (unpack-environment ev))))

(define (arglist-extend-env xs ev)
  (cond
   ((null? xs) ev)
   (#t (let* ((key-value (first xs))
	      (key (first key-value))
	      (vel (second key-value)))
	 (extend-env key vel (arglist-extend-env (cdr xs) ev))))))


;; lookup variable in current environment
;; assign value to val
;; jump to continue
;; never return... either HALT or jump to where value needed
(define m-var
  (lambda ()
    (env-lookup expr env) 
    ))

(define lambda?
  (lambda (e)
    (and (pair? e) (eq? (car e) 'lambda))))

(define (lambda-args lam)
  (second lam))

;; (lambda arglist body....)
(define (lambda-body lam)
  (rest (rest lam)))


;; make a closure
(define m-lambda
  (lambda ()
    (set! val (list 'closure expr env))
    (cont)))

(define closure?
  (lambda (e)
    (and (pair? e) (eq? (car e) 'closure))))

(define (closure-lambda lam)
  (second lam))

(define (closure-environment lam)
  (third lam))


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
     ((null? unargs)
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
    (m-args-1)
    ))

(define m-args-3
  (lambda ()
    (set! expr args)
        
    ;; clean up stack in reverse order
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


(define compound-apply
  (lambda ()
    ;; assume clozure?
    ;; (display "compound-apply : applying clozure")(newline)
    ;; (write expr)
    (let* ((cloz (car expr))
	   (lam (closure-lambda cloz))
	   (lam-args (lambda-args lam))
	   (lam-body (car (lambda-body lam)))
	   (lam-env  (closure-environment cloz))	   
	   (vals (cdr expr))
	   (new-env (arglist-extend-env (arglist-bind lam-args vals) lam-env)))

      ;; (newline)
      ;; (display "\nnew environment : ")(write new-env)
      ;; (newline)
      ;; (display "\nlambda body : ")(write lam-body)
      ;; (display "\n")

      (save-cont)
      (save-env)
      (save-expr)

      ;; first expression will do
      (set! expr lam-body)
      (set! env new-env)
      (set! cont compound-return)
      (m-eval)
      
      )))


;; something to do will tail calls , just jump they dont return ...
(define compound-return
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
;; (define begin? (lambda (e) (and (pair? e) (eq? (car e) 'begin))))				       
;; (define m-begin
;;   (lambda ()
;;     ;; drop leading begin keyword
;;     (set! expr (cdr expr))
;;     (save-cont)
;;     (save-unargs)
;;     (set! cont m-begin-2)
;;     (set! unargs 0)
;;     ))

;; expr = (if a b c)
(define if? (lambda (e) (and (pair? e) (eq? (car e) 'if))))				       
(define m-if
  (lambda ()
    ;; drop leading begin keyword
    (save-cont)
    (save-expr)
    (set! expr (second expr))
    (set! cont m-if-2)
    (m-eval)
    ))
(define m-if-2
  (lambda ()
    ;; drop leading begin keyword
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
     ((symbol? expr)  (m-var))
     ((lambda? expr)  (m-lambda))
     ((closure? expr) (set! val expr) (cont))
     ((primitive-procedure? expr) (set! val expr) (cont))
     ((if? expr) (m-if))
     ((define? expr) (m-define))
     ;;((begin? expr) (m-begin))
     ((pair? expr) (m-apply))
     (#t (halt))      
    )))

(define unpack-environment
  (lambda (e) (second e)))

;; change a value or extend environment
(define mutate-environment
  (lambda (sym v)
    (define recur
      (lambda (ys)
	(cond
	 ((null? ys) (set! env (extend-env sym v env)))
	 ((eq? sym (first (first ys))) (set-car! ys (list sym v)))
	 (#t (recur (cdr ys))))))
    (recur (unpack-environment env))))



(define define? (lambda (e) (and (pair? e) (eq? (car e) 'define))))				       
;; expr = (define sym expr)
(define m-define
  (lambda ()
    (display "EXPR = ") (display expr) (newline)
    (save-expr)
    (save-cont)
    (set! expr (third expr))
    (set! cont m-define-2)
    (display "toEXPR = ") (display expr) (newline)
    (m-eval)
    ))

;; value in val
;; 
(define m-define-2
  (lambda ()
    (restore-cont)
    (restore-expr)
    (display "toVAL = ") (display val) (newline)
    ;; mutate environment
    (display "fromEXPR = ") (display expr) (newline)
    (set! expr (second expr))
    (display "symEXPR = ") (display expr) (newline)
    ;; if just extend env , no need to do set-car! or search for binding
    (mutate-environment expr val)
    (display "newENV = ") (display env) (newline)
    
    ;;(set! env (extend-env (list (list expr val)) env))
    (cont)
    ))





;; print val register
;; jump to continue
(define m-print
  (lambda ()
    (newline)
    (display "out :")
    (write val)
    (newline)
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
    (cont)))


;; set next thing to do is m-repl2
;; now goto read
(define m-repl
  (lambda ()
    (set! cont m-repl2)
    (m-read)
    ))

(define m-repl2
  (lambda ()
    (set! cont m-repl3)
    (m-eval)
    ))

(define m-repl3
  (lambda ()
    (set! cont m-repl)
    (m-print)
    ))


(define run
  (lambda ()
    (set! env (initial-environment))
    (set! expr 'a)
    (set! stack '())
    (set! val 0)
    ;; m-repl handle continue
    (m-repl)
    ))

