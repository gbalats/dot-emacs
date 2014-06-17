;;; key-bindings --- My personal keybindings
;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'key-bindings)

;;; Code:

;; set the keybinding so that you can use f4 for goto line
(global-set-key [f4] 'goto-line)

;;; Since `M-<SPC>' is used by gnome, use `C-c <SPC>' instead
(global-set-key (kbd "C-c <SPC>") 'just-one-space)

(provide 'key-bindings)

;;; key-bindings ends here
