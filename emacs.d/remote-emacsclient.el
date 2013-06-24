(require 'tramp)
(require 'tramp-sh)

(setq server-use-tcp t)
(server-start)

(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun update-tramp-emacs-server-port-forward (method-name)
  "Update the specified TRAMP's method to forward the Emacs server port to the local host.
This lets emacsclient on the remote host open files in the local
Emacs server."
  (let* ((method (assoc method-name tramp-methods))
         (ssh-args (cadr (assoc 'tramp-login-args method))))
    (put-alist 'tramp-login-args
      (list (put-alist "-R" (let ((port (process-contact server-process :service)))
        ;; put-alist makes a dotted pair for the key/value, but tramp-methods
        ;; needs a normal list, so put the value inside a list so that the
        ;; second part of the dotted pair (ie the cdr) is a list, which converts
        ;; it from a dotted pair into a normal list.
                              (list (format "%s:127.0.0.1:%s" port port)))
                       ssh-args))
      method)))

(defun tramp-make-tramp-file-name-from-vec (vec file)
  (tramp-make-tramp-file-name
    (tramp-file-name-method vec)
    (tramp-file-name-user vec)
    (tramp-file-name-host vec)
    file))

(defun tramp-get-remote-emacsclient-socket (vec)
  (with-tramp-connection-property vec "emacsclient-socket"
    (let ((socket (tramp-make-tramp-file-name-from-vec
                   vec
                   (or
                    (tramp-get-method-parameter
                     (tramp-file-name-method vec) 'tramp-emacsclient-socket)
                    "~/.emacs.d/remote-server"))))
      (if (file-writable-p socket)
	  socket
	(tramp-error vec 'file-error "file %s not accessible" socket)))))

(defadvice tramp-open-connection-setup-interactive-shell
  (after copy-server-file-by-tramp (proc vec) activate)
  (let ((socket (tramp-get-remote-emacsclient-socket vec))
        (server (process-contact server-process :local)))
    (with-temp-file socket
      (insert
       (format "127.0.0.1 %d\n" (elt server (- (length server) 1)))
       (format "-auth %s\n" (process-get server-process :auth-key))
       (server-quote-arg (tramp-make-tramp-file-name-from-vec vec ""))
       "\n"))))

(provide 'remote-emacsclient)
