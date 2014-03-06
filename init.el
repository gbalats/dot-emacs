(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-report-syntactic-errors t)
 '(c-backspace-function (quote delete-backward-char))
 '(inhibit-startup-screen t)
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/")
                            ("melpa" . "http://melpa.milkbox.net/packages/")
                            ("marmalade" . "http://marmalade-repo.org/packages/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:background "brightred"))))
 '(flymake-warnline ((t (:background "brightblue")))))

;; load files
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/lb-datalog-mode/")

;; install prelude packages (and other packages) on emacs 24
(when (>= emacs-major-version 24)
  (require 'prelude-packages)
  (require 'cedet))

(require 'basic-conf)
(require 'key-bindings)
(require 'win-switch)
(require 'no-easy-keys)
(require 'tbemail)
(require 'flymake)
(require 'flymake-cursor)
(require 'java-decomp)
(require 'project-root)
(require 'no-selection-copy)
(require 'lb-datalog-mode)
(require 'setup-theme)


;; Uncomment this line to give emacs access to clipboard contents
;; (require 'xclip)

;; enable / disable easy keys by default
(no-easy-keys-minor-mode 0)

;; window-switching parameters
(win-switch-setup-keys-ijkl "\C-xo")
(setq win-switch-idle-time 1.0)
(setq win-switch-other-window-first nil)

;; AucTex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Tramp
(setq tramp-default-method "rsync")
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir")
(setq password-cache-expiry 3600)

;; FlyMake
(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

;; Setting some C / C++ defaults
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (flymake-mode t)
                      (global-set-key (kbd "C-c C-v") 'my-flymake-show-next-error))))

;; PHP mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Groovy
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq whitespace-global-modes
      '(c-mode c++-mode lb-datalog-mode java-mode emacs-lisp-mode
               shell-script-mode sh-mode))

;; Thesaurus
(when (>= emacs-major-version 24)
  (require 'synonyms)
  (setq synonyms-file        "~/.emacs.d/thesaurus/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/thesaurus/mthesaur.txt.cache"))
