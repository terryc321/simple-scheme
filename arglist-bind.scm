
(add-to-load-path "/home/terry/simple-scheme/src/")
(use-modules (environment))

;; structural pattern matching be nice ...

(define (arglist-bind args vals)
  (cond
   ((symbol? args) (cons (cons args vals) '())) ;; slurp + done
   ((null? args) '()) ;; done -- maybe we threw inputs away ?
   (#t
    (cons
     (cons (car args) (car vals))
     (arglist-bind (cdr args) (cdr vals))))))


;; malformed arguments to values 
;;(arglist-bind '() '(1 2 3))

;; slurp
(arglist-bind 'args '(3 4 5))

(arglist-bind '(a) '(1))

(arglist-bind '(a b) '(1 2))

(arglist-bind '(a . b) '(1 2 3 4 5 6 7))

(arglist-bind '(a b c) '(1 2 3))

(arglist-bind '(a b c . d) '(1 2 3 4 5 6 7))


;;(fresh-env lam-env bindings)
;;(make-env)

(define fe (fresh-env (make-env '()) '((a . 1)(b . 2)(c 3 4 5))))
(lookup-env fe 'a)
(lookup-env fe 'b)
(lookup-env fe 'c)

;; know arglist-bind wrks ok , bindings is result of arglist-bind ...
(define ge (fresh-env (make-env '()) (arglist-bind '(a b c . d) '(1 2 3 4 5 6 7))))
(lookup-env ge 'a)
(lookup-env ge 'b)
(lookup-env ge 'c)
(lookup-env ge 'd)

