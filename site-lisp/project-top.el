;;; project-top --- Find project's top directory when compiling

;;; Commentary:

;; This package automatically detects the right top directory when
;; compiling a project by searching for an appropriate building file
;; (e.g., `Makefile', `build.xml', etc) upwards.

;;; Code:

(require 'compile)
(require 'cl)


(defcustom project-top-build-tools
  '(("make" . "Makefile")
    ("ant"  . "build.xml")
    ("mvn"  . "pom.xml"))
  "Build configuration files per tool."
  :type '(alist :value-type (group string))
  :group 'project-top)


(defun project-top-find (command-string)
  "Return the projects's top directory given a COMMAND-STRING for compilation."
  (loop for (key . value) in project-top-build-tools
        when (string-match (concat "^" key "\\b") command-string)
        return (locate-dominating-file default-directory value)))

(defun project-top-start-hook ()
  "Change the default directory when compiling."
  ;; this is what actually causes the compilation directory to change
  (set (make-local-variable 'compilation-process-setup-function)
       'project-top-setup)
  ;; correct compilation message (1st line)
  (let ((project-top-directory (project-top-find compile-command)))
    (if project-top-directory (setq default-directory project-top-directory))))

(defun project-top-setup ()
  "Adjust the compilation buffer by displaying the top directory."
  (let ((top-directory (project-top-find compile-command)))
    (when (and top-directory (not (equal top-directory default-directory)))
      (cd top-directory)    ; sets default-directory
      (with-current-buffer (compilation-find-buffer)
        (save-excursion
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert
             "** project-top has changed the default directory to:\n"
             "** " default-directory "\n")))))))

(add-hook 'compilation-mode-hook 'project-top-start-hook)
(provide 'project-top)

;;; project-top.el ends here
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
