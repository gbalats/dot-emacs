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
(add-to-list 'load-path "~/.emacs.d/use-package/")


(require 'basic-conf)
(require 'key-bindings)
(require 'flymake)
(require 'flymake-cursor)
(require 'java-decomp)
(require 'project-root)
(require 'use-package)


;; Various packages
(use-package prelude-packages)
(use-package setup-theme)


;;---------------------
;; Builtin packages
;;---------------------

;; Dired
(use-package find-dired
  :bind ("C-c f" . find-name-dired))

;; Rebind `C-x C-b' for `buffer-menu'
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Electric pairs
(use-package electric
  :commands electric-pair-mode
  :idle (electric-pair-mode t))

;; Tramp
(use-package tramp
  :config
  (setq tramp-default-method "rsync")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir")
  (setq password-cache-expiry 3600))

;; Whitespace mode
(use-package whitespace
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t)
  (setq whitespace-global-modes
        '(c-mode c++-mode lb-datalog-mode java-mode emacs-lisp-mode
                 shell-script-mode sh-mode)))

(use-package cedet)


;;-----------------------
;; Extra packages (user)
;;-----------------------

(use-package tbemail)

;; window-switching
(use-package win-switch
  :commands win-switch-mode
  :idle (win-switch-mode)
  :config
  (win-switch-setup-keys-ijkl "\C-xo")
  (setq win-switch-idle-time 1.0)
  (setq win-switch-other-window-first nil))

;; communicating with clipboard
(use-package xclip
  :disabled t)

;; enable / disable easy keys (e.g., arrows)
(use-package no-easy-keys
  :bind ("<f5>" . no-easy-keys-minor-mode)
  :init (no-easy-keys-minor-mode 0))

;; Copying things without selecting them
(use-package no-selection-copy
  :bind (("C-c w" . copy-word)
         ("C-c l" . copy-line)
         ("C-c p" . copy-paragraph)))


;;-----------------------
;; Extra packages (dist)
;;-----------------------

;; Thesaurus
(use-package synonyms
  :config
  (setq synonyms-file        "~/.emacs.d/thesaurus/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/thesaurus/mthesaur.txt.cache"))

;; AucTex
(use-package tex-site
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t))

;; FlyMake
(defun my:flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line))

;; Setting some C / C++ defaults
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (flymake-mode t)
                      (global-set-key (kbd "C-c C-v") 'my-flymake-show-next-error))))


;;-----------------------
;; Major Modes
;;-----------------------

;; PHP mode
(use-package php-mode
  :mode "\\.php$")

;; Groovy mode
(use-package groovy-mode
  :mode "\\.groovy$"
  :interpreter "groovy")

;; LB-Datalog mode
(use-package lb-datalog-mode
  :mode "\\.logic$"
  :load-path "lb-datalog-mode/")
