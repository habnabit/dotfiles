(require 'cl)
(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(defun hab/load-package-requirements-list ()
  (mapcar
   (lambda (r)
     (when (null (string-match "^\\([^ ]+\\)\\(?: +\\([^ ]+\\)\\)?$" r))
       (error "Bad requirement %S" r))
     (cons (intern (match-string 1 r)) (match-string 2 r)))
   (remove-if
    (lambda (s) (= (length s) 0))
    (mapcar
     (lambda (x) (replace-regexp-in-string "\\s-*\\(?:#.*\\)?$" "" x))
     (split-string
      (with-temp-buffer
        (insert-file-contents "~/.emacs.d/packages.txt")
        (buffer-string))
      "\n")))))

(defun hab/package-up-to-date-p (package)
  (apply 'package-installed-p package))

(defun hab/update-packages ()
  (interactive)
  (let ((packages (remove-if #'hab/package-up-to-date-p
                             (hab/load-package-requirements-list))))
    (unless (null packages)
      (message "Refreshing package database...")
      (package-refresh-contents)
      (message "Installing %d packages..." (length packages))
      (mapc
       (lambda (package)
         (unless (hab/package-up-to-date-p package)
           (package-install (car package))))
       packages))))

(hab/update-packages)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/pymacs")
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
(require 'css-mode)
(require 'haml-mode)
(require 'gnus-art)
(require 'notmuch)
(require 'git-gutter)
(require 'popwin)
(require 'rainbow-delimiters)
(require 'markdown-mode)
(require 'jinja2-mode)
(require 'web-mode)
(require 'yaml-mode)
(require 'parsley-mode)
(require 'caml-types)
(require 'tuareg)
(require 'auto-complete)
(require 'flycheck)
(require 'flycheck-jsx)
(require 'flycheck-rust)

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
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-to-list 'auto-mode-alist '("\\.parsley\\'" . parsley-mumamo))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tac\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
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
(flycheck-add-mode 'javascript-jshint 'web-mode)

(autoload 'tuareg-mode "tuareg" (interactive) "Major mode for editing Caml code." t)
(autoload 'camldebug "camldebug" (interactive) "Debug caml mode")

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook
          #'(lambda ()
              (setq c-basic-offset 4)))
(add-hook 'web-mode-hook
          #'(lambda ()
              (when (or (equal web-mode-content-type "jsx")
                        (equal web-mode-content-type "javascript"))
                (setq web-mode-code-indent-offset 2))))

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
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t)
  (global-hl-line-mode 1))
