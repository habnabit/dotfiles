
;; -*- emacs-lisp -*-
;;
;;
;; name: color-theme-twilight
;; date: Wed Oct 06 2010 00:09:29 GMT-0700 (PDT)
;;
;;
;; To use this theme save this code into a
;; file named color-theme-twilight.el and place it
;; in a directory in your load-path
;;
;;    (require 'color-theme-twilight)
;;    (color-theme-twilight)
;;


(require 'color-theme)

(defun color-theme-twilight ()
  "Generated with http://color-theme-select.heroku.com/ on Wed Oct 06 2010 00:09:29 GMT-0700 (PDT)
Color theme by Marcus Crafter, based off the TextMate Twilight theme, created 2008-04-18"
  (interactive)
  (color-theme-install
    '(color-theme-twilight
      (
       (background-color . "black")
       (background-mode . "dark")
       (border-color . "black")
       (cursor-color . "#A7A7A7")
       (foreground-color . "#F8F8F8")
       (mouse-color . "sienna1")
      )
      (
      )
      (default  ((t (:background "black" :foreground "white"))))
      (blue  ((t (:foreground "blue"))))
      (bold  ((t (:bold t))))
      (bold-italic  ((t (:bold t))))
      (buffers-tab  ((t (:background "black" :foreground "white"))))
      (font-lock-builtin-face  ((t (:foreground "white"))))
      (font-lock-comment-face  ((t (:foreground "#5F5A60"))))
      (font-lock-constant-face  ((t (:foreground "#CF6A4C"))))
      (font-lock-doc-string-face  ((t (:foreground "DarkOrange"))))
      (font-lock-function-name-face  ((t (:foreground "#e16521"))))
      (font-lock-keyword-face  ((t (:foreground "#dbb800"))))
      (font-lock-preprocessor-face  ((t (:foreground "Aquamarine"))))
      (font-lock-reference-face  ((t (:foreground "SlateBlue"))))
      (font-lock-regexp-grouping-backslash  ((t (:foreground "#E9C062"))))
      (font-lock-regexp-grouping-construct  ((t (:foreground "red"))))
      (font-lock-string-face  ((t (:foreground "#4d6537"))))
      (font-lock-type-face  ((t (:foreground "#783300"))))
      (font-lock-variable-name-face  ((t (:foreground "#7587A6"))))
      (font-lock-warning-face  ((t (:bold t :foreground "Pink" :weight bold))))
      (py-builtins-face  ((t (:foreground "#935e29"))))
      (py-pseudo-keyword-face  ((t (:foreground "#ae5251"))))
      (py-decorators-face  ((t (:foreground "#935e29"))))
      (py-XXX-tag-face  ((t (:foreground "gold"))))
      (gui-element  ((t (:background "#303030" :foreground "black"))))
      (region  ((t (:background "#444444"))))
      (minibuffer-prompt  ((t (:foreground "#ff6600"))))
      (flymake-warnline  ((t (:foreground "black" :background "LightSteelBlue"))))
      (flymake-errline  ((t (:foreground "black" :background "LightSalmon"))))
      (mode-line  ((t (:background "#303030" :foreground "white"))))
      (highlight  ((t (:background "#222222"))))
      (highline-face  ((t (:background "SeaGreen"))))
      (text-cursor  ((t (:background "yellow" :foreground "black"))))
      (tuareg-font-lock-governing-face  ((t (:foreground "#dbb800"))))
      (zmacs-region  ((t (:background "snow" :foreground "ble"))))
      (widget-field  ((t (:background "#333"))))
      (mumamo-border-face-in  ((t (:foreground "Aquamarine"))))
      (mumamo-border-face-out  ((t (:foreground "Aquamarine"))))
     )
  )
)

(provide 'color-theme-twilight)