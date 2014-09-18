;;; cedet-setup.el --- my cedet setup

;;; Commentary:

;; This is based on the new version of CEDET.

;;; Code:

(defvar cedet-root-directory
  (concat user-emacs-directory (file-name-as-directory"site-lisp/cedet/"))
  "The root directory where CEDET is installed.")

;; initialize CEDET
(load-file (concat cedet-root-directory "cedet-devel-load.el"))

;; Add subdirectory to load path
(add-to-list 'load-path (concat cedet-root-directory "contrib"))
;; (load-file (concat cedet-root-directory "contrib/cedet-contrib-load.el"))

;; Require the following packages
(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(require 'semantic/senator)

(load-file (concat cedet-root-directory "contrib/semantic-tag-folding.el"))
;; TODO: why isn't this working?
;; (require 'semantic-tag-folding)

;; Configure semantic submodes
(dolist (submode '(global-semantic-idle-local-symbol-highlight-mode
                   global-semantic-idle-completions-mode
                   global-semantic-idle-summary-mode
                   global-semantic-decoration-mode
                   global-semantic-show-unmatched-syntax-mode))
  (add-to-list 'semantic-default-submodes submode t))

;; Additional includes
(semantic-add-system-include "/opt/llvm/default/include" 'c++-mode)
(semantic-add-system-include "/usr/include" 'c++-mode)

;; Enable EDE (Project Management) features
(global-ede-mode t)
(ede-enable-generic-projects)

;; Enable tag folding for all semantic-enabled buffers
;; (global-semantic-tag-folding-mode 1)
;;; The above is not working. Use the following workaround:
(add-hook 'semantic-decoration-mode-hook
          (lambda () (semantic-tag-folding-mode t)))

;; Enable Semantic
(semantic-mode 1)

;; generic CEDET mode customization
(defun gbalats/cedet-hook ()
  "Load my personal CEDET configurations."
  ;; (add-to-list 'ac-sources 'ac-source-gtags)
  ;; (add-to-list 'ac-sources 'ac-source-semantic)
  ;; (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;; (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "C-c v") 'semantic-decoration-include-visit)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c d") 'semantic-ia-show-doc)
  ;; (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c C-c c") 'semantic-ia-describe-class)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c +") 'semantic-tag-folding-show-block)
  (local-set-key (kbd "C-c -") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c C-c +") 'semantic-tag-folding-show-all)
  (local-set-key (kbd "C-c C-c -") 'semantic-tag-folding-fold-all))

(add-hook 'c-mode-common-hook 'gbalats/cedet-hook)

;; Load contrib library
;; (require 'eassist)
(load-file (concat cedet-root-directory "contrib/eassist.el"))

;; Enable SRecode
(global-srecode-minor-mode 1)

;; C/C++ CEDET mode customization
(defun gbalats/c-mode-cedet-hook ()
  "Provide additional Keybindings for C/C++ modes."
  (local-set-key (kbd "C-c t") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-c l") 'eassist-list-methods)
  (local-set-key (kbd "C-c C-r") 'semantic-symref))

(add-hook 'c-mode-common-hook 'gbalats/c-mode-cedet-hook)

(when (cedet-ectag-version-check t)
  (semantic-load-enable-primary-ectags-support))

(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; load projects
(load-file (concat user-emacs-directory "site-lisp/etc/cedet-projects.el"))

(provide 'cedet-setup)
;;; cedet-setup.el ends here
