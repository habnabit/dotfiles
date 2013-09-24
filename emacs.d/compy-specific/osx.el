(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(alpha 90 85))
(global-set-key (kbd "s-`") (lambda () (interactive) (other-frame 1)))
(global-set-key (kbd "s-~") (lambda () (interactive) (other-frame -1)))
(global-set-key (kbd "<kp-delete>") 'delete-char)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 79)
 '(mumamo-background-colors nil)
 '(solarized-broken-srgb nil)
 '(solarized-diff-mode (quote high))
 '(solarized-italic nil)
 '(solarized-underline nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "apple" :family "Menlo")))))
