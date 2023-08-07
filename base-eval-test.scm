
(define-module (base-eval-test)
  #:use-module (srfi srfi-64)
  #:use-module (list-utility)
  #:use-module (closure)
  #:use-module (environment)
  #:use-module (base-eval))

(define identity (lambda (x) x))

;;--------- testing framework -----------------
(test-begin "base-eval-test")

;; numbers
(test-equal (base-eval 123 (make-env) identity) 123)

;; boolean
(test-equal (base-eval #t (make-env) identity) #t)
(test-equal (base-eval #f (make-env) identity) #f)

;; strings
(test-equal (base-eval "harry" (make-env) identity) "harry")

;; quoted expressions are themselves
(test-equal (base-eval ''a (make-env) identity) 'a)
(test-equal (base-eval '(quote x) (make-env) identity) 'x)

;; condition legs
(test-equal (base-eval '(if #t 1 2) (make-env) identity) 1)
(test-equal (base-eval '(if #f 1 2) (make-env) identity) 2)

;; if takes 3 args only
;; (test-error #t (base-eval '(if #t 1 2 3) (make-env) identity))
;; (test-error #t (base-eval '(if #f 1) (make-env) identity))
;; (test-error #t (base-eval '(if #t) (make-env) identity))
;; (test-error #t (base-eval '(if) (make-env) identity))


;; environment lookup ok
(test-equal (base-eval 'a '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 1)
(test-equal (base-eval 'b '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 3)
(test-equal (base-eval 'c '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 4)
(test-equal (base-eval 'd '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 5)

;; environment lookup throws error if not found
;; (test-error #t (base-eval 'f '((c . 4)(d . 5)(b . 3)(a . 1)) identity))

;; malformed begin
;; (test-error #t (base-eval '(begin) '((c . 4)(d . 5)(b . 3)(a . 1)) identity))

;; sequence check ok
(test-equal (base-eval '(begin 1) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 1)
(test-equal (base-eval '(begin 1 2) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 2)
(test-equal (base-eval '(begin 1 2 3) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 3)
(test-equal (base-eval '(begin 1 2 3 4) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 4)
(test-equal (base-eval '(begin 1 2 3 4 5) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 5)

;; no way to interact with the environment from within an eval-define ...
;; define takes 2 args

;;(test-equal (base-eval '(define a 3) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 3)
;;(test-equal (base-eval '(define b 10) '((c . 4)(d . 5)(b . 3)(a . 1)) identity) 10)
(let ((env  '((c . 4)(d . 5)(b . 3)(a . 1))))
  (test-equal (base-eval '(define b 10) env identity) 10)
  (test-equal env '((c . 4)(d . 5)(b . 10)(a . 1)))
  (test-equal (base-eval '(define c 8) env identity) 8)
  (test-equal env '((c . 8)(d . 5)(b . 10)(a . 1)))
  (test-equal (base-eval '(define a 6) env identity) 6)
  (test-equal env '((c . 8)(d . 5)(b . 10)(a . 6)))
  (test-equal (base-eval '(define b 2) env identity) 2)
  (test-equal env '((c . 8)(d . 5)(b . 2)(a . 6)))
  (test-equal (base-eval '(define f 7) env identity) 7)
  (test-equal env '((f . 7)(c . 8)(d . 5)(b . 2)(a . 6))))


  








;;--------- testing framework -----------------
(test-end "base-eval-test")



