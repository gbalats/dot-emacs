;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'basic-conf)

;;; Code:

(provide 'basic-conf)

;; to display time
(display-time)

;; to change the default tab size
(setq default-tab-width 4)

;; to stop making backup files
(setq make-backup-files nil)

;; disable auto-save
(setq auto-save-default nil)

;; always end a file with a newline
(setq require-final-newline 'query)

;; Centering code stolen from somewhere and restolen from 
;; http://www.chrislott.org/geek/emacs/dotemacs.html
;; centers the screen around a line...
(global-set-key [(control l)]  'centerer)

;; Change Ctrl+L behavior, so cool!
(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom"
  (interactive)
  (cond ((eq last-command 'centerer2)  ; 3 times pressed = bottom
		 (recenter -1))
		((eq last-command 'centerer1)  ; 2 times pressed = top
		 (recenter 0)
		 (setq this-command 'centerer2))
		(t                             ; 1 time pressed = middle
		 (recenter)
		 (setq this-command 'centerer1))))

;; c-editing tabs (change indent-tabs-mode to t, to insert tabs instead)
(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4
              tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
              indent-tabs-mode nil)

;; swap C-a with M-m
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; set line numbering by default
(global-linum-mode 1)

;; set other modes by default
(delete-selection-mode 1)
(column-number-mode 1)

;; my java-mode hook
(defun my-java-mode-hook ()
    (c-set-offset 'inline-open 0))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Make shell-prompt read-only
(setq comint-prompt-read-only t)
