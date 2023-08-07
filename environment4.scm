

(define-module (environment)
  #:use-module (list-utility)
  #:export (

	    lookup-env
	    extend-env!
	    set-env!
	    define-env!
	    ))


;; does not work if environment is initially empty ,
;; needs atleast one pair in environment to get started


;; lookup either fails with #f
;; find match returns a pair ( key . val )
(define (lookup-env env key)
  (cond
   ((null? env) #f)
   ((eq? (car (car env)) key) (car env))
   (#t (lookup-env (cdr env) key))))

(define (extend-env! env key val)
  (let ((old-car (car env))
	(old-cdr (cdr env)))
    (set-cdr! env (cons old-car old-cdr))
    (set-car! env (cons key val))
    env))

(define (set-env! env key val)
  (let ((lookup-pair (lookup-env env key)))
    (if lookup-pair
	(begin
	  (set-cdr! lookup-pair val)
	  env)
	#f)))

(define (define-env! env key val)
  (let ((lookup-pair (lookup-env env key)))
    (if lookup-pair
	(begin
	  (set-cdr! lookup-pair val)
	  lookup-pair)
	(extend-env! env key val))))

