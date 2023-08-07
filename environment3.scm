

(define-module (environment)
  #:use-module (list-utility)
  #:export (
	    ;; make-env
	    ;; empty-env?

	    ;; extend-env
	    ;; lookup-env
	    
	    ;; modify-env
	    ;; modify-or-extend-env

	    extend-env!
	    modify-env!
	    ))



;;----------------- all out modified -------------------------
(define (extend-env! env key val cont)
  (let ((old-car (car env))
	(old-cdr (cdr env)))
    (set-cdr! env (cons old-car old-cdr))
    (set-car! env (cons key val))
    (cont val)))



(define (modify-env! env key val cont)
  (let ((exchanged #f))
    (define (recur ys)
      (cond
       ((null? ys) #f)
       ((eq? (car (car ys)) key)  (begin
				    (set-cdr! (car ys) val)
				    (set! exchanged #t)
				    val))
       (#t (recur (cdr ys)))))
    (let ((maybe (recur env)))
      (if exchanged
	  (cont maybe)
	  (extend-env! env key val cont)))))


;;-----------------------------------------------------
;; environment is a mutable list of alist
;; if make new environment and extend from this environment
;; also link onto
;; just make a new head ?
;; nested define will see each other ... if head is not actual alist just
;;-------------------------------------------------------
(define (make-env) '(()))


;; modify-env! -- include define inside current environment such that
;; future accesses to exp will experience delight of newly mutated list
;;
;; find the pair where the definition lies ...
;;


(define (empty-env? xs) (equal? xs '()))

;; slap a new key-val pair onto something ... pass to continue
(define (extend-env env key val cont)
  (cont (cons (cons key val) env)))


;; lookup key in env ... if found match ... pass to continue PAIR key-value
;; otherwise pass #f false to continue
;; may run off end or loop indefinitely if given circular list???


;; tag win if found in environment
(define (lookup-env env key cont)
  (cond
   ((null? env) (cont #f))
   ((eq? (car (car env)) key)      (cont (cons 'win (cdr (car env)))))
   (#t (lookup-env (cdr env) key cont))))

;;-------------------------------------------------------------
;; tag successfull calls with win
;; able to send both errors and wins through same continue
;; saves exponential growth of possible continues
;; builds a new list 
(define (modify-env env key val cont)
  (let ((exchanged #f))
    (define (recur ys)
      (cond
       ((null? ys) #f)
       ((eq? (car (car ys)) key)  (begin
				    (set! exchanged #t)
				    (cons (cons key val) (cdr ys))))
       (#t (cons (car ys) (recur (cdr ys))))))
    (let ((maybe (recur env)))
      (if exchanged
	  (cont (cons 'win maybe)) 
	  (cont #f)))))

;; if modify-env did the job , pass through
;; or if not extend environment and tag it a win
(define (modify-or-extend-env env key val cont)
  (modify-env env key val (lambda (r)
			    (if (and (pair? r)(eq? (car r) 'win))
				(cont r)
				(cont (cons 'win (cons (cons key val) env)))))))




;;-------------------------------------------------------------



