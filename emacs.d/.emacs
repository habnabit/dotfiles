(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/flymake")
(add-to-list 'load-path "~/.emacs.d/circe/lisp")
(add-to-list 'load-path "~/.emacs.d/notmuch/emacs")
(add-to-list 'load-path "~/.emacs.d/git-gutter")
(add-to-list 'load-path "~/.emacs.d/popwin-el")
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/web-mode")
(setq web-mode-engines-alist ())
(load "~/.emacs.d/compy-specific/init.el")
(defun fix-path ()
  (interactive)
  (setenv "PATH" (mapconcat 'identity exec-path ":")))
(fix-path)

(when (and (>= emacs-major-version 24)
           (>= emacs-minor-version 2))
  (eval-after-load "mumamo"
    '(setq mumamo-per-buffer-local-vars
           (delq 'buffer-file-name mumamo-per-buffer-local-vars))))

(load "~/.emacs.d/nxhtml/autostart.el")
(require 'flymake)
(require 'magit)
(require 'twittering-mode)
(require 'css-mode)
(require 'moccur-edit)
(require 'javascript-mode)
(require 'haml-mode)
(require 'gnus-art)
(require 'notmuch)
(require 'git-gutter)
(require 'popwin)
(require 'rainbow-delimiters)
(require 'markdown-mode)
(require 'jinja)
(require 'web-mode)
(require 'cython-mode)
(require 'yaml-mode)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(show-paren-mode 1)
(column-number-mode 1)
(add-to-list 'completion-ignored-extensions ".annot")
(add-to-list 'completion-ignored-extensions ".orig")
(ido-mode 1)
(global-git-gutter-mode t)
(global-rainbow-delimiters-mode t)
(popwin-mode 1)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . mako-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mumamo))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'web-mode-engines-alist '(("velocity" . "\\.tmpl\\'")))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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
(setq flymake-run-in-place nil)
(setq twittering-reverse-mode t)

(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#") ;; emacs autosave files
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

(when (load "flymake" t)
  (defun flymake-pycheckers-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pycheckers.py" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pycheckers-init))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.tac\\(\\.example\\)?\\'" flymake-pycheckers-init)))
(add-hook 'find-file-hook 'flymake-mode)

(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-N") 'git-gutter:next-hunk)
(global-set-key (kbd "M-P") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c k") 'git-gutter:revert-hunk)
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


(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/color-theme-solarized")
  (load-theme 'solarized-dark t)
  (global-hl-line-mode 1))
