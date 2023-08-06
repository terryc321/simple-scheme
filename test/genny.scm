
;; mit-scheme......... laughable simple and incorrect gensym generator......
;; a.k.a gensym....

(define (new-uninterned-symbol str)
  (string->uninterned-symbol str))

(load-option 'format)

;; simple gensym generator
(define genny (let ((n 0))
		(lambda ()
		  (set! n (+ n 1))
		  (string->symbol (format #f "tmp~a" n)))))



