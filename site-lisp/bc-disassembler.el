;;; bc-disassembler --- Java bytecode and LLVM bitcode disassembler

;;; Commentary:

;; This package allows automatic disassembly of Java bytecode and LLVM bitcode.
;;
;; Disassembly can happen in three cases:
;; (a) when opening a Java .class file
;; (b) when opening an LLVM .bc file
;; (c) when disassembling a .class file inside a jar
;;
;; When `javap-mode' is available, it is automatically selected for the
;; current Java bytecode-containing buffer.
;;
;; When `llvm-mode' is available, it is automatically selected for the
;; current LLVM bitcode-containing buffer.
;;
;; In any case, `javap' and `llvm-dis' must be installed in the
;; system, since these are the tools that actually perform the
;; disassembly.

;;; Code:


;; Add handlers for automatically disassembling .class and .bc files
(add-to-list 'file-name-handler-alist '("\\.class$" . bc-disassembler-java))
(add-to-list 'file-name-handler-alist '("\\.bc$"    . bc-disassembler-llvm))

;; Add hook for automatically disassembling .class files inside jars
(add-hook 'archive-extract-hooks
          (lambda ()
            (cond ((string-match "\\.class$" (buffer-file-name))
                   (bc-disassembler-autoextract)))))


;;---------------------------------------------------------------
;; Auto-extracting and disassembly of Java bytecode inside jars
;;---------------------------------------------------------------

(defun bc-disassembler-autoextract ()
  "Disassembles a Java class-file inside a jar archive, using javap."
  (let*
      ((components (split-string (buffer-file-name) ":"))
       (jar-file   (car components))
       (class-file (cadr components))
       (class-name (replace-regexp-in-string
                         "/" "." (file-name-sans-extension class-file))))
    (rename-buffer (concat class-name
                           " (" (file-name-nondirectory jar-file) ")"))
    (erase-buffer)
    (generate-bytecode-buffer class-file jar-file t)
    (message "Disassembled %s" class-file)))


;;------------------------------
;; Java and LLVM handlers
;;------------------------------

(defun generate-bytecode-buffer (class-file &optional classpath inside-jar-p)
  "Disassembles a Java CLASS-FILE inside a buffer, using `javap'.

The CLASSPATH argument can be used to determine where to search
for class-files.  The newly generated buffer is returned."
  (let* ((dirname     (file-name-directory class-file))
         (filename    (file-name-nondirectory class-file))
         (classname   (file-name-sans-extension filename))
         (buffer      (if inside-jar-p (current-buffer)
                        (generate-new-buffer filename))))

    ;; fully qualify class name if inside jar
    (when inside-jar-p
      (setq classname (replace-regexp-in-string
                       "/" "." (file-name-sans-extension class-file))))

    ;; create buffer to hold disassembled Java bytecode
    (with-current-buffer buffer
      ;; Disassemble .class file
      (call-process "javap" nil t nil "-private" "-verbose"
                    "-classpath" (if classpath classpath dirname) classname)

      ;; Set buffer's filename
      (setq buffer-file-name class-file)

      ;; Set read-only mode for this buffer
      (setq buffer-read-only t)

      ;; Mark the buffer as unmodified
      (set-buffer-modified-p nil)

      ;; Jump to the beginning of the buffer
      (goto-char (point-min))

      ;; Switch to `javap-mode'
      (when (fboundp 'javap-mode)
        (javap-mode))

      (current-buffer))))

(defun bc-disassembler-java (op &rest args)
  "Handle .class files by putting the output of javap in the buffer."
  (cond
   ((eq op 'get-file-buffer)
     (let* ((class-file (car args))
            (buffer-name (file-name-nondirectory class-file)))
       (generate-bytecode-buffer class-file)))
   ((java-decomp--alternate-handler op args)))) ;;; TODO: change name


(defun bc-disassembler-llvm (op &rest args)
  "Handle .bc files by putting the output of llvm-dis in the buffer."
  (cond
   ((eq op 'get-file-buffer)
    (let* ((file (car args))
           (filename (file-name-nondirectory file))
           (basename (file-name-sans-extension filename))
           (tempfile (make-temp-file basename nil ".ll")))
      (with-current-buffer (generate-new-buffer filename)
        (call-process                   ; Disassemble bitcode into `tempfile'
         "llvm-dis" nil t nil
         "-o" tempfile file)            ; Command arguments
        (insert-file-contents tempfile) ; Read contents of `tempfile'
        (delete-file tempfile)          ; Delete `tempfile'
        (setq buffer-file-name file)
        (setq buffer-read-only t)       ; Set readonly mode
        (set-buffer-modified-p nil)
        (goto-char (point-min))         ; Move cursor to beginning
        (when (fboundp 'llvm-mode)      ; View in llvm-mode
          (llvm-mode))
        (current-buffer))
      ;; (minibuffer-message "%s was automatically disassembled..." file)
      ))
   ((llvm-decomp--alternate-handler op args)))) ;;; TODO: change name


(defun java-decomp--alternate-handler (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'bc-disassembler-java
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun llvm-decomp--alternate-handler (operation args)
  "Run the real handler without the javap handler installed."
  (let ((inhibit-file-name-handlers
         (cons 'bc-disassembler-llvm
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(provide 'bc-disassembler)

;;; bc-disassembler ends here
