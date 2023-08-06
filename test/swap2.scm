
;; debug macro building routines
;; depends on
;; list let set! defmacro
;;
(defmacro swap (r s)
	   (list 'let (list (list 'tmp s))
		 (list 'set! s r)
		 (list 'set! r 'tmp)))

