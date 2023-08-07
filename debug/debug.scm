
;; emulate a debugger

;; debugger from repl
;; repl

;;(read)
;;(eval)
;;(print)
;; loop



(define env '((a 1)(b 2)(c 3)))

(define escape 0)
(define nth-input 1)

(define (reader cont)
  (cont (read)))

(define (writer exp cont)
  (display exp)
  (cont #t))

;; escape to repl 
(define (eval exp env cont)  
  (cond
   ((eq? exp 'e) (make-debugger 1 exp env escape cont)) ;; letter e : meant to be exit!
   ((and (pair? exp) ;; quit
	 (eq? (car exp) 'unquote)
	 (eq? (car (cdr exp)) 'q))
    #t)
   ;; ((and (pair? exp) (eq? (car exp) 'set!))    
   (#t (cont exp))))


(define (repl)
  (writer (format #f "ready[~a] >" nth-input)
	  (lambda (t0)
	    (reader
	     (lambda (in)
	       (eval in env
		     (lambda (out)
		       (writer in (lambda (t1)
				    (writer " => " (lambda (t2)
						     (writer out (lambda (t3)
								   (newline)
								   (set! nth-input (+ 1 nth-input))
								   (repl))))))))))))))

(set! escape repl)
;; back out of the computation ?  

(define (make-debugger height exp env escape cont)

  (define nth-input 1)
  
  (define (debug-reader cont)
    (cont (read)))

  (define (debug-writer exp cont)
    (display exp)
    (cont #t))

  (define (debug-eval exp env cont)
    (cond
     ((eq? exp 'e) (make-debugger (+ 1 height) exp env debug-repl cont))
     ((and (pair? exp) ;; quit
	   (eq? (car exp) 'unquote)
	   (eq? (car (cdr exp)) 'q))
      (escape))
     (#t (cont exp))))

  (define (debug-repl)
    (debug-writer (format #f "debug{~a}[~a] >" height nth-input)
		  (lambda (t1)
		    (debug-reader
		     (lambda (in)
		       (debug-eval in env
			     (lambda (out)
			       (debug-writer in
					     (lambda (t1)
					       (debug-writer " => "
							     (lambda (t2)
							       (debug-writer out
									     (lambda (t3)
									       (newline)
									       (set! nth-input (+ 1 nth-input))
									       (debug-repl))))))))))))))

  (debug-repl))




		

