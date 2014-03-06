;; Handle Java .class files

(provide 'java-decomp)

(add-to-list 'file-name-handler-alist '("\\.class$" . java-decomp-handler))

(add-hook 'archive-extract-hooks
          (lambda ()
            (cond ((string-match "\\.class$" (buffer-file-name))
                   (java-decomp-transform)))))

(defun java-decomp-transform ()
  "Decompiles a Java class-file inside a Jar, using javap."
  (let*
      ((components (split-string (buffer-file-name) ":"))
       (jar-file-name (car components))
       (class-file-name (cadr components))
       (java-class-name (replace-regexp-in-string
                         "/" "." (file-name-sans-extension class-file-name))))
    (with-current-buffer (buffer-name)
      (erase-buffer)
      (call-process "javap" nil t nil "-verbose"
                    "-classpath" jar-file-name java-class-name)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (javap-mode)
      (rename-buffer (concat java-class-name
                             " ("
                             (file-name-nondirectory jar-file-name)
                             ")")))))

(defun java-decomp-handler (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let ((file (car args)))
      (with-current-buffer (create-file-buffer file)
        (call-process "javap" nil (current-buffer) nil "-verbose"
                      "-classpath" (file-name-directory file)
                      (file-name-sans-extension (file-name-nondirectory file)))
        (setq buffer-file-name file)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (javap-mode)
        (current-buffer))))
   ((java-decomp--alternate-handler op args))))

(defun java-decomp--alternate-handler (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'java-decomp-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))
