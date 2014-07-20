;;; basic-conf --- My basic configurations

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'basic-conf)

;;; Code:

;; to display time
(display-time)

;; Centering code stolen from somewhere and restolen from
;; http://www.chrislott.org/geek/emacs/dotemacs.html
;; centers the screen around a line...
(global-set-key [(control l)]  'centerer)

;; Change Ctrl+L behavior, so cool!
(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom."
  (interactive)
  (cond ((eq last-command 'centerer2)  ; 3 times pressed = bottom
         (recenter -1))
        ((eq last-command 'centerer1)  ; 2 times pressed = top
         (recenter 0)
         (setq this-command 'centerer2))
        (t                             ; 1 time pressed = middle
         (recenter)
         (setq this-command 'centerer1))))

;; my java-mode hook
(defun my-java-mode-hook ()
  "Personalized java mode."
    (c-set-offset 'inline-open 0))

(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Make shell-prompt read-only
(defvar comint-prompt-read-only t)

(provide 'basic-conf)

;;; basic-conf.el ends here
