

;; try get emacs lisp to open a pipe to a guile process running my script
;; pass an s-expression to guile
;; get response back from guile in a string or process, somehow
;; get emacs to detect when press a key - to initiate some action ?
;;

;; (let ((process-connection-type nil))
;;   (start-process "my-process" "foo" "guile" "-c" "(begin (load \"machine.scm\")(run))"))

(let ((process-connection-type nil))
  (start-process "my-process" "foo" "guile"))


;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Process-Information.html
(process-list)
(list-processes) ;; gui version process-list q=closes d=deletes process
(delete-process "my-process")
(process-command (get-process "my-process"))
(process-id (get-process "my-process"))
(process-status "my-process") ;; run  vs  nil

(process-send-string "my-process" "(load \"machine.scm\")\n")

(process-send-string "my-process" "(run)\n")

(process-send-string "my-process" "a\n")

(process-send-string "my-process" "a\n")

(process-send-string "my-process" "(load \"../test/lambda.scm\")\n")

(process-send-string "my-process" "a\n\n")

(process-send-eof "my-process")


(start-process "my-process" "foo2" "ls" "-l" "/bin")

(erase-buffer) ;; clear everything?

(with-current-buffer "foo"
  (erase-buffer))
