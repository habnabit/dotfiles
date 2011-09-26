(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/magit")
(load "~/.emacs.d/compy-specific/init.el")

(require 'magit)
(require 'twittering-mode)
(require 'css-mode)
(require 'moccur-edit)
(require 'javascript-mode)
(require 'haml-mode)
(eval-after-load 'color-theme
  '(progn
     (color-theme-initialize)
     (require 'color-theme-twilight)
     (color-theme-twilight)))
(require 'color-theme)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(show-paren-mode 1)
(column-number-mode 1)
(global-hl-line-mode 1)
(ido-mode 1)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-to-list 'auto-mode-alist '("\\.module\\'" . nxhtml-mumamo-mode))

(add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4")
(add-to-list 'load-path "~/.emacs.d/ocaml")
(require 'caml-types)
(setq auto-mode-alist
      (cons '("\\.ml[iyl]?$" .  tuareg-mode) auto-mode-alist))

(autoload 'tuareg-mode "tuareg" (interactive)
  "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")
(require 'tuareg)
(define-key tuareg-mode-map (kbd "C-c C-t") 'caml-types-show-type)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook
          #'(lambda ()
              (setq c-basic-offset 4)))

(defvar user-temporary-file-directory "~/.emacs.d/backups/")
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#") ;; emacs autosave files
              (seq bol "." (not (any "."))) ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions
              '(".pyc"
                ".elc")))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pycheckers.py" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pycheckers-init)))

(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-3") 'server-edit)
(global-set-key (kbd "C-c g") 'magit-status)

(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the
message in the minibuffer"
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (let ((err (car (second elem))))
            (message "%s" (fly-pyflake-determine-message err)))))))

(defun fly-pyflake-determine-message (err)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
        ((null (flymake-ler-file err))
         ;; normal message do your thing
         (flymake-ler-text err))
        (t ;; could not compile err
         (format "compile error, problem on line %s" (flymake-ler-line err)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the mini-buffer rather than having to mouse over it"
  (show-fly-err-at-point))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
  (set (make-local-variable 'post-command-hook)
       (cons 'show-fly-err-at-point post-command-hook)))

