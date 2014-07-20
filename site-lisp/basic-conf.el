;;; basic-conf --- My basic configurations

;;; Commentary:

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'basic-conf)

;;; Code:

;; to display time
(display-time)

;; my java-mode hook
(defun my-java-mode-hook ()
  "Personalized java mode."
    (c-set-offset 'inline-open 0))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(provide 'basic-conf)

;;; basic-conf.el ends here
