(require 'mumamo)
(require 'mumamo-chunks)

(defmacro parsley:build-string-regexp (quotechar repeat)
  `(rx ,quotechar
       (,repeat
        (or
         (and "\\" (or "\"" "'" "\\" "n" "r" "t" "b" "f"
                       (and "x" (= 2 (in "a-fA-F0-9")))))
         (not (any ,quotechar))))
       ,quotechar))

(defvar parsley-font-lock-keywords
  (list
   '("#.*$" . font-lock-comment-face)
   (cons (parsley:build-string-regexp "'" one-or-more) 'font-lock-string-face)
   (cons (parsley:build-string-regexp "\"" zero-or-more) 'font-lock-string-face)
   '("\\^[[:space:]]*([^)]*)" . font-lock-doc-face)
   '("\\(^\\|:\\)[a-zA-Z_][a-zA-Z0-9_]*" . font-lock-variable-name-face)
   '("~" . font-lock-negation-char-face)
   )
  "Parsley font lock keywords.")

(define-derived-mode parsley-mode prog-mode "Parsley"
  "Parsley mode."
  (set (make-local-variable 'font-lock-defaults)
       '(parsley-font-lock-keywords)))

(defun parsley:predicate-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "\?(" ")" 'borders 'python-mode))

(defun parsley:action-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "!(" ")" 'borders 'python-mode))

(defun parsley:rule-value-chunk (pos max)
  (mumamo-quick-chunk-forward pos max "->" '(")\\|$" nil) 'borders 'python-mode))

(define-mumamo-multi-major-mode parsley-mumamo
  "Parsley mumamo mode."
  ("Parsley" parsley-mode
   (parsley:predicate-chunk
    parsley:action-chunk
    parsley:rule-value-chunk)))


(provide 'parsley-mode)
