

;; alist module just gets some entry from proper list first + second key - value pairs
(load "modules/alist-module.scm")

;; be ideal if we had some form of assertion
;;
;; so we can do a basic test if first of a list gives first item of list
;; assuming scheme read procedure not corrupted...
(load "modules/assertion-module.scm")

;; list utility - simply first second third fourth etc..
(load "modules/list-utility-module.scm")

;; lets now load the macro substitution 
(load "modules/substitution-module.scm")

;; routines to keep track of macros installed , used by expand-derived to expand derived forms to
;; simpler in terms set! define if begin lambda ...
(load "modules/macro-expander-module.scm")

(load "macros/and-macro.scm")
(load "macros/begin-simplify.scm")
(load "macros/cond-macro.scm")
(load "macros/define-simplify.scm")
(load "macros/if-simplify.scm")
(load "macros/lambda-begin-simplify.scm")
(load "macros/let-macro.scm")
(load "macros/let-procedure-macro.scm")
(load "macros/let-star-macro.scm")
(load "macros/letrec-macro.scm")
(load "macros/or-macro.scm")
(load "macros/while-macro.scm")

"******* macros loaded ************ "



;; closure representation lambda + environment at time of creation of lambda
(load "modules/closure-module.scm")

;; representation of environment
(load "modules/environment-module.scm")

;; sicp like scheme register machine
(load "modules/machine-module.scm")


"************* machine loaded *************** "


