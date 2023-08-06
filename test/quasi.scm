
; is quasi quoting a runtime thing ?
; or is a macro ?
; ;;
; (quasiquote (unquote x))
;
; expands to x in macro world
; then evaluated at runtime becomes lookup-value of x

;; qq to be quasi quote

(defmacro qq (x)  
  (list 1 2 3 4 (quote x)))

(qq 5)


;; infinite lists
;; lazy evaluation choosing not to evaluate everything all the time
;;



