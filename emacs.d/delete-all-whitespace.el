:;exec emacs --batch "$@" -Q -l "$0"

(dolist (buffer (buffer-list))
  (unless (or buffer-read-only (string-match-p "^ *\\*" (buffer-name buffer)))
    (message "stripping %s..." buffer)
    (with-current-buffer buffer
      (delete-trailing-whitespace)
      (when (buffer-modified-p)
        (message "  stripped %s" buffer)
        (save-buffer)))))
