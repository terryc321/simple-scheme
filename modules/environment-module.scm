


;; (define-module (environment)
;;   #:use-module (list-utility)
;;   #:export (
;; 	    make-env
;; 	    lookup-env
;; 	    extend-env!
;; 	    set-env!
;; 	    define-env!
;; 	    fresh-env
;; 	    ))


;; does not work if environment is initially empty ,
;; needs atleast one pair in environment to get started

(define (make-env alist)
  (vector alist))

(define (unpack-env env)
  (vector-ref env 0))

(define (pack-env alist)
  (vector alist))

;; lookup either fails with #f
;; find match returns a pair ( key . val )
(define (lookup-env env key)
  (define (recur ys)
    (cond
     ((null? ys) #f)
     ((eq? (car (car ys)) key) (car ys))
     (#t (recur (cdr ys)))))
  (recur (unpack-env env)))


(define (extend-env! env key val)
  (vector-set! env 0 (cons (cons key val) (unpack-env env))))

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


(define (fresh-env env alist)
  (vector (append alist (unpack-env env))))


