
;; bug somewhere ...
;;$10 = ((#t 1) (#f 2))
;; macro-expansion : (quote (#t 1))
;; out[1] : (#t 1)


;; debugging argument passing
;; bug last slurp arguments (cons key val)
;; should have been (cons (key val) '())
;; so remains a proper list


;; do we have enough machinery to write a cond macro
;; using backquote quasiquote and comma
;; only recently added , splice does not work yet

;; trying to see if macro was slurping arguments correctly
;; if give it args
;; expect args to be the entire cond expression ?

;; (defmacro cond args
;;   `(list ',args))

;; (defmacro cond args
;;    (quasiquote (quote (unquote args))))

(defmacro cond0 (arg)
  `',arg)

(cond0 (#t 1) (#f 2))

;; wanted to debug output
;; so i placed expression inside a quasi-quote with unquote
;; surrounded by quote

(defmacro cond1 args
  `',args)

;; bug in environment extended due to append
;; 


(cond1 (#t 1) (#f 2))
;; args ((#t 1)(#f 2))

;; rewrite to
;; (if #t (begin 1) (if #f (begin 2) '()))

;; given body of cond expression
;; e : '((#t 1)(#f 2))

;; given cond expression
;; null? not defined
;; '() is this null ? , yes all share same
;;
;; dont yet have splicing in place yet in quasi quote mechanism ...
;;(define (rw e) ....
(define rw (lambda (e)
	     (if (null? e) ''()
		 (if (eq? (car (car e)) 'else)
		     `(begin ,@(cdr (car e)))
		     `(if ,(car (car e)) (begin ,@(cdr (car e)))
			  ,(rw (cdr e)))))))


;; $1 = #<procedure rw (e)>
;; scheme@(guile-user)> (rw '((#t 1)(#f 2)))
;; $2 = (if #t (begin 1) (if #f (begin 2) ()))
;; scheme@(guile-user)> (rw '((#t 1 2 3 )(#f 2 3 4)))
;; $3 = (if #t (begin 1 2 3) (if #f (begin 2 3 4) ()))
;; scheme@(guile-user)> (rw '((#t 1 2 3)(#f 4 5 6)(#t 7 8 9)(else 10 11 12)))
;; $4 = (if #t (begin 1 2 3) (if #f (begin 4 5 6) (if #t (begin 7 8 9) (if else (begin 10 11 12) ()))))

(defmacro cond3 args  
  (rw args))

;; if want to see expansion of cond
(defmacro cond4 args  
  `',(rw args))


(cond3 (#t 1) (#f 2))
(cond4 (#t 1) (#f 2))


;; now we have cond3 executes ok
;; define cond in terms of cond3
(defmacro cond args
  (cons 'cond3 args))

(cond (#t 1) (#f 2))
(cond (#f 1) (#t 2))

;; (macroexpand '(cond (#t 1)(#f 2)))
;; (macroexpand-1 '(cond (#t 1)(#f 2)))
;; this macro expansion happens inside interpreter not visible

;; now cond working , we can redefine rewriter in terms of cond
(define rw2 (lambda (e)
	      (cond
	       ((null? e) ''())
	       ((eq? (car (car e)) 'else)
		`(begin ,@(cdr (car e))))
	       (#t `(if ,(car (car e))
			(begin ,@(cdr (car e)))
			,(rw2 (cdr e)))))))


;; cond5 does rewrite will get re-evaluated to do actual cond
(defmacro cond5 args
  (rw2 args))

;; cond6 used for testing to see what cond5 expands to 
(defmacro cond6 args
  `',(rw2 args))


(cond6 (#t 1) (#f 2))
(cond6 (#f 1) (#t 2))
(cond6 ((+ 1 2) 1) (#f 2))

;; cond3 based on rw

;; cond5 based on rw2 
(cond5 (#t 1) (#f 2))
(cond5 (#f 1) (#t 2))

(cond4 (#f 1) (#f 2)(else 3))
(cond6 (#f 1) (#f 2)(else 3))

;; $e access to environment
;; function is
;; evaluate a lambda , we tag it with closure tag
;; ( closure (lambda (e) ...) environment)
;; macro-closure
;;
;; car => (primitive . #procedure...)
;; cdr => (primitive . #procedure...)
;; of underlying system ...

;; (set! $e '())
;;  set environment to be empty list
;; (cond6 (#f 1) (#f 2)(else 3))
;;  => system complains



