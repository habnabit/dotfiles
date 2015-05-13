(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/twittering-mode")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/circe/lisp")
(add-to-list 'load-path "~/.emacs.d/notmuch/emacs")
(add-to-list 'load-path "~/.emacs.d/git-gutter")
(add-to-list 'load-path "~/.emacs.d/popwin-el")
(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/web-mode")
(add-to-list 'load-path "~/.emacs.d/pymacs")
(add-to-list 'load-path "~/.emacs.d/tuareg")
(add-to-list 'load-path "~/.emacs.d/ocaml")
(add-to-list 'load-path "~/.emacs.d/autocomplete")
(add-to-list 'load-path "~/.emacs.d/dash")
(add-to-list 'load-path "~/.emacs.d/color-identifiers-mode")
(add-to-list 'load-path "~/.emacs.d/flycheck")
(add-to-list 'load-path "~/.emacs.d/solarized-emacs")
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/rust-mode")
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
(require 'parsley-mode)
(require 'caml-types)
(require 'tuareg)
(require 'auto-complete)
(require 'flycheck)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(autoload 'rust-mode "rust-mode" nil t)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(show-paren-mode 1)
(column-number-mode 1)
(add-to-list 'completion-ignored-extensions ".annot")
(add-to-list 'completion-ignored-extensions ".orig")
(ido-mode 1)
(global-git-gutter-mode t)
(popwin-mode 1)
(global-auto-complete-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.parsley\\'" . parsley-mumamo))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.ml[iyl]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(setq web-mode-engines-alist '(("django" . "\\.jinja2\\'")
                               ("velocity" . "\\.tmpl\\'")))

(autoload 'tuareg-mode "tuareg" (interactive) "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

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

(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "M-N") 'git-gutter:next-hunk)
(global-set-key (kbd "M-P") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-c k") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-3") 'server-edit)
(global-set-key (kbd "C-c g") 'magit-status)
(define-key tuareg-mode-map (kbd "C-c C-t") 'caml-types-show-type)

(defun isort nil
  "Sort python imports"
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "isort -"
   nil t nil t))


(flycheck-define-checker web-html-tidy
  "A HTML syntax and style checker using Tidy.

See URL `https://github.com/w3c/tidy-html5'."
  :command ("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q" source)
  :error-patterns
  ((error line-start
          "line " line
          " column " column
          " - Error: " (message) line-end)
   (warning line-start
            "line " line
            " column " column
            " - Warning: " (message) line-end))
  :modes web-mode)


(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/solarized-emacs")
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t)
  (global-hl-line-mode 1))
