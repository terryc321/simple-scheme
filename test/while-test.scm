

;; requires macros/while.scm 

(let ((n 1))
  (while (< n 10)
    (format #t "n = ~a" n)
    (newline)))



