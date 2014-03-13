;;; Code:

(require 'compile)
(require 'cl)

(defun project-root-find (command-string)
  (let ((tools '(("make" . "Makefile")
                 ("ant"  . "build.xml")
                 ("mvn"  . "pom.xml"))))
    (loop for (key . value) in tools
          when (string-match (concat "^" key "\\b") command-string)
          return (locate-dominating-file default-directory value))))

(defun project-root-start-hook ()
  ;; this is what actually causes the compilation directory to change
  (set (make-local-variable 'compilation-process-setup-function)
       'project-root-setup)
  ;; correct compilation message (1st line)
  (let ((project-root (project-root-find compile-command)))
    (if project-root (setq default-directory project-root))))

(defun project-root-setup ()
  (let ((project-root (project-root-find compile-command)))
    (if project-root
        (progn
          (cd project-root)    ;sets default-directory
          (with-current-buffer (compilation-find-buffer)
            (save-excursion
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert
                 "** project-root has changed the default directory to:\n"
                 "** " default-directory "\n"))))))))

(add-hook 'compilation-mode-hook 'project-root-start-hook)
(provide 'project-root)

;;; project-root.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
