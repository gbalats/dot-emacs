;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'key-bindings)

;;; Code:

(provide 'key-bindings)

;; set the keybinding so that f3 will start the shell
(global-set-key [f3] 'shell)

;; set the keybinding so that you can use f4 for goto line
(global-set-key [f4] 'goto-line)

;; set the keybinding so that you can use f5 to toggle easy keys (e.g. arrows, Home, etc)
(global-set-key [f5] 'no-easy-keys-minor-mode)

;;; Rebind `C-x C-b' for `buffer-menu'
(global-set-key "\C-x\C-b" 'ibuffer)

;;; Since `M-<SPC>' is used by gnome, use `C-c <SPC>' instead
(global-set-key (kbd "C-c <SPC>") 'just-one-space)

;; Copy
(global-set-key (kbd "C-c w") (quote copy-word))
(global-set-key (kbd "C-c l") (quote copy-line))
(global-set-key (kbd "C-c p") (quote copy-paragraph))

;; Re-compile shortcut
(global-set-key (kbd "C-c c") 'recompile)

;; Magit-status shortcut
(global-set-key (kbd "C-c m") 'magit-status)

;; Find files recursively shortcut
(global-set-key (kbd "C-c f") 'find-name-dired)
