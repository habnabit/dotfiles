:;exec emacs -Q --script "$0" "$@"

(require 'server)
(let* ((target (expand-file-name (or (elt argv 0) default-directory)))
       (_ (message "Opening %S..." target))
       (cmd `(let ((-toplevel (magit-toplevel ,target)))
	       (if -toplevel
		   (progn
		     (funcall-interactively 'magit-status -toplevel)
		     (select-frame-set-input-focus (selected-frame)))
		 (format "%S is not in a git repo" ,target))))
       (result (server-eval-at "server" cmd)))
  (if result (message "%s" result)))
