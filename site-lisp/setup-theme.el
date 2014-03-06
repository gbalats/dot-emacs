;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'setup-theme)

;;; Code:

(require 'color-theme)
(require 'zenburn-theme)

(defun on-frame-open (frame)
  (unless (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame))

(defun on-after-init ()
  (let ((frame (selected-frame)))
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame))))

(add-hook 'after-make-frame-functions 'on-frame-open)
(add-hook 'window-setup-hook 'on-after-init)
(menu-bar-mode 0)

(provide 'setup-theme)

;; setup-theme.el ends here
