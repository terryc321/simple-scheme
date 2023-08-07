
why guile does not have these things defined out the gate is bizarre

(define first car)
(define second cadr)

;; recursive expansion
;; quasi quote expression - assuming just one level of quasi-quote nesting for now
;;
;; if meet unquote then just that expression
;; (quasiquote (unquote a)) => a
;;
;; (unquote a)
;; (unquote) ;; malformed unquoted expression - ie computer generated or bad macro expansion for example
;; debugger ??
;;
;;
;; exp  is math e 
;;  e^Z  where Z is an integer ...
;; ========================================================================

(define (qq->compound? ex)
  (and (pair? ex)
       (eq? (car ex) '$qq)))

(define (unquoted? ex)  (and (pair? ex) (eq? (car ex) '$qc)))
(define (unquoted-thing ex) (cadr ex))
(define (unquoted-splice? ex) (and (pair? ex) (eq? (car ex) '$qs)))
(define (unquoted-splice-thing ex) (cadr ex))
(define (parent-of-unquoted-splice? ex) (and (pair? ex)
					     (unquoted-splice? (car ex))))				     
(define (parent-get-spliced-thing ex) (cadar ex))
(define (not-pair? ex) (not (pair? ex)))
(define (qq-recur ex)
  (cond
   ((null? ex) (list (quote list) (list (quote quote) (quote ())))) ;; (list 'quote '()) => (quote ())
   ((not-pair? ex) (list (quote list) (list (quote quote) ex))) ;; not unquoted or spliced
   ((unquoted? ex) (list (quote list) (unquoted-thing ex))) ;; (list a) 
   ((unquoted-splice? ex) (unquoted-splice-thing ex)) ;; unquote-splice ,@ xs => xs 
   (#t
    (append (list (quote append))
	    (map qq-recur ex)))))

(define (qq-entry ex)
  (cond
   ((null? ex) (list (quote quote) ex)) ;; `() = () 
   ((not-pair? ex) (list (quote quote) ex)) ;; `a = 'a 
   ;; ((unquoted-splice? ex) "error unquote-splice outside of list") ;; `,@(list 1 2 3)
   (#t (qq-recur ex))))

;; here is start
(define (qq->compound ex)
  (qq-entry (cadr ex)))




;; qq = quasi-quote
;; (qq EXPR) is a quasi-quoted EXPR
;; qc = quasi-comma
;; qs = quasi-splice
;; ----------------------------
;; reason for different notation is to escape underlying system trying to be helpful by doing the
;; quasi quote expansion before can get hands on s expression itself
;; expansion build s expression
;; -------------------------------
;; has to be cons , car , cdr , list , append 

(qq->compound '($qq a))

(qq->compound '($qq (a b c)))

(qq->compound '($qq (a ($qc b) ($qs c))))

(qq->compound '($qq (let ((tmp (gensym))) a ($qc b) ($qs c))))

==================================================================================================

;; ;; same as qq-recur except in handling ?? it;s just (map qq-recur ex) no??
;; (define (qq-recur-elems ex)
;;   (cond
;;    ((null? ex) (list (list (quote quote) (quote ())))) ;; (list 'quote '()) => (quote ())
;;    ((not-pair? ex) (list (quote list) (list (quote quote) ex))) ;; not unquoted or spliced
;;    ((unquoted? ex) (list (quote list) (unquoted-thing ex))) ;; (list a) 
;;    ((unquoted-splice? ex) (unquoted-splice-thing ex)) ;; unquote-splice ,@ xs => xs 
;;    (#t
;;     (cons (qq-recur (car ex)) ;; element of list may itself be quasi-quote???
;; 	  (qq-recur-elems (cdr ex))))))

while macro ....

user> (let ((loop 1))
	(while (< n 10)
	  (set! loop (+ n 1))))

1st try at while macro .........

(letrec ((loop (lambda (n)
		 (if (< n 10)
		     (begin
		       ...body...
		       (loop (+ n 1)))))))
  (loop 1))


inside ...body... i happen to say
(set! loop (+ n 1))

expanded form looks like this ......

(let ((loop 1))
  (letrec ((loop (lambda (n)
		   (if (< n 10)
		       (begin
			 (set! loop (+ n 1))
			 (loop (+ n 1)))))))
    (loop 1)))

all hell has broken loose........

gensym loop name 


(let ((tmp (gensym "loop")))
`(letrec ((,tmp (lambda (n)
		  (if (< n 10)
		      (begin
			,@body
			(,tmp (+ n 1)))))))
   (,tmp 1)))


--------------------------------------------
macro fully expands before eval gets to see

eval only understands a few core primitives and rest are machine code routines? huh ?


===============================================================================================

(let ((cde '(1 2 3))
      (a "A")
      (b (list (quote quote) "B")))
  (append (list (quote list))
	  (list a)
	  (list b)
	  cde))


(append (quote ()) (quote ()) (quote ()))



;; (append ... cde ...)  is a splice
;; (append ... (list (list (quote quote) a)) ... )



;; ==========================================================================

;; originally thought needed eval to build a quasiquote expression but i dont think thats the case
;; simply need to replace parts not evaluating ie that are quasi-quoted to be quoted ?
;; the result is an s-expression built with cons , list , append , quote 
;; 
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

;; ======================================================================================

motivating example


<<<<<<<<<<<<<<<<<<<<< buggy <<<<<<<<< one arm IF - missing if-false-arm !
(let ((tmp (gensym "loop")))
    ($qq (letrec ((($qc tmp) (lambda ()
		      (if ($qc condition)
			  (begin
			    ($qs body)
			    (($qc tmp)))))))
	   (($qc tmp))))))
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


- write a macro expander

(while (< n 10) (format #t "n = ~a ~%" n))
=>
(let ((tmp (gensym "loop")))
    `(letrec ((,tmp (lambda ()
		      (if ,condition
			  (begin
			    ,@body
			    (,tmp))
			  #f))))
       (,tmp)))

where see ,tmp be replaced with gensym'd loop like loop51

=>
(letrec ((loop51 (lambda ()
		   (if (< n 10)
		       (begin
			 (format #t "n = ~a ~%" n)
			 (loop51))
		       #t))))
  (loop51))

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(qq EXPR)

EXPR has something about it that requires altering s expression
comma is fairly simple ?
simply replace that item by what is in the environment at the time

but i can write equivalent function at compile time , that does not need runtime environment in order to be
described ,
so thinking need base-eval available is not correct...?


>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


