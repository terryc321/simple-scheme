#lang racket

;;---------------------------------------------------------------------------------------
;; environment
;; environment-tag + list of key <=> value pairs 
;;
;; untag-environment
;; environment?
;;
;; extend environment
;; cons item onto environment
;;
;; (extend env key val)
;;
;; get 1st item
;; (first env)
;;---------------------------------------------------------------------------------------


(define extend-env 0)
(define env-lookup 0)
(define make-env 0)
(define env? 0)

(let ((ekey (gensym)))

  (define (inside-is-env? ev)
    (and (vector? ev)
         (eq? (vector-ref ev 0) 'env)
         (or (pair? (vector-ref ev 1))
             (null? (vector-ref ev 1)))))

  (define (env-guard ev)
    (if (inside-is-env? ev)
        'ok
        (error "env-guard halt" ev)))
  
  (define (make-empty-env)
    (let ((result (vector 'env '())))
      (env-guard result)
      result))

  (define (env-unpack ev)
    (env-guard ev)
    (vector-ref ev 1))
  
  (define (extend key val ev)
    (env-guard ev)
    (let ((result (vector 'env (cons (list key val) (env-unpack ev)))))
      (env-guard result)
      result))

  (define (lookup key ev)
    (define (recur ys)
      (cond
        ((null? ys) (error "not found in env" key))
        ((eq? key (car (car ys)))
         (car (cdr (car ys))))
        (#t (recur (cdr ys)))))
    (env-guard ev)
    (recur (env-unpack ev)))

  ;; external interface
  (set! extend-env extend)
  (set! env-lookup lookup)
  (set! make-env make-empty-env)
  (set! env? inside-is-env?)
  
  );; inside environment-master key

;; make environment
;; extend environment
;; check things added are in there
(define (test)
  (let ((ev #(env ((c 10)(b 4)(a 3)))))
    (list
     (equal? 3 (env-lookup 'a ev))
     (equal? 4 (env-lookup 'b ev))
     (equal? 10 (env-lookup 'c ev))
     (equal? ev
             (extend-env 'c 10
                         (extend-env 'b 4
                                     (extend-env 'a 3 (make-env))))))))

   
 


;; ;; -----------------------------------------------------------------
;; (define (enviro-lookup exp env cont)
;;   (define (env-recur x ys cont)
;;     (cond
;;      ((null? ys) (error (list x 'not-found 'in 'environment env)))
;;      ((eq? exp (first-first ys)) (cont (rest (first ys))))
;;      (#t (env-recur x (rest ys) cont))))
;;   (env-recur exp (enviro-unpack env) cont))


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
;; (define (enviro-modify sym val env cont)
;;   ;; ys represents a proper list of key value pairs
;;   (define (env-recur ys)
;;     (cond
;;      ((null? ys) (error (list 'enviro-modify 'symbol sym 'not-found 'in 'environment env)))
;;      ((eq? sym (first-first ys)) (begin
;; 				   (set-car! ys (cons sym val))
;; 				   (cont val)))
;;      (#t (env-recur (rest ys)))))
;;   (env-recur (enviro-unpack env)))

;; ----------------------------------------------------------------

;; (define (enviro-modify sym val env cont)
;;   (enviro-pair-lookup sym
;; 		      env
;; 		      (lambda (key-value)
;; 			(if (pair? key-value)
;; 			    (begin (set-cdr! key-value val)
;; 				   (cont key-value))
;; 			    (cont #f)))))

;; (define (enviro-extend sym val env cont)
;;   ;;(set! $env (tag '<environment> (cons (cons sym val) (environment-unpack env))))
;;   (set-cdr! env (cons (cons sym val) (enviro-unpack env)))
;;   (cont val))

;; ;; if pair comes back from environment modified , then its been modified
;; ;; if not , then extend environment 
;; (define (enviro-modify-or-extend sym val env cont)
;;   (define (env-recur ys)
;;     (cond
;;      ((null? ys) (enviro-extend sym val env cont)) ;; pass-through to enviro-extend
;;      ((eq? sym (first-first ys)) (begin
;; 				   (set-car! ys (cons sym val))
;; 				   (cont val)))
;;      (#t (env-recur (rest ys)))))
;;   (env-recur (enviro-unpack env)))



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

