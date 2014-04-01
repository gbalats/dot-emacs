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


(require 'basic-conf)                  ; basic configuration
(require 'java-decomp)                 ; auto-decompress Java bytecode
(require 'key-bindings)                ; global keybindings
(require 'use-package)


;;---------------------
;; Builtin packages
;;---------------------

;; Re-compile shortcut
(use-package compile
  :defer t
  :bind ("C-c c" . recompile)
  :config
  (use-package project-root)
  (setq compilation-scroll-output 'first-error))

;; Dired
(use-package find-dired
  :defer t
  :bind ("C-c f" . find-name-dired))

;; Rebind `C-x C-b' for `buffer-menu'
(use-package ibuffer
  :defer t
  :bind ("C-x C-b" . ibuffer))

;; Open shell with <f3>
(use-package shell
  :defer t
  :bind ("<f3>" . shell))

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

(use-package tbemail)


;;-----------------------
;; Extra packages (dist)
;;-----------------------

(use-package package
  :config
  (use-package prelude-packages))

;; Magit
(use-package magit
  :ensure t
  :commands magit-status
  :bind ("C-c m" . magit-status))

;; Google this
(use-package google-this
  :ensure t
  :init (google-this-mode t))

;; Thesaurus
(use-package synonyms
  :ensure t
  :config
  (setq synonyms-file        "~/.emacs.d/thesaurus/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/thesaurus/mthesaur.txt.cache"))

;; AucTex
(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
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

(use-package zenburn-theme
  :ensure t)

;; Transparent zenburn theme
(use-package color-theme
  :ensure t
  :config
  (use-package zenburn-theme)

  (defun on-frame-open (frame)
    (unless (display-graphic-p frame))
    (set-face-background 'default "unspecified-bg" frame))

  (defun on-after-init ()
    (let ((frame (selected-frame)))
      (unless (display-graphic-p frame)
        (set-face-background 'default "unspecified-bg" frame))))

  (add-hook 'after-make-frame-functions 'on-frame-open)
  (add-hook 'window-setup-hook 'on-after-init)
  (menu-bar-mode 0))

;; FlyMake
(use-package flymake
  :ensure t
  :idle (flymake-start-syntax-check)
  :init
  (defun my:flymake-show-next-error()
    (interactive)
    (flymake-goto-next-error)
    (flymake-display-err-menu-for-current-line))
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (flymake-mode 1)
                        (local-set-key (kbd "C-c C-v")
                                       'my:flymake-show-next-error)))))

(use-package flymake-cursor
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))

;;-----------------------
;; Major Modes
;;-----------------------

;; PHP mode
(use-package php-mode
  :mode "\\.php\\'")

;; Groovy mode
(use-package groovy-mode
  :mode "\\.groovy\\'"
  :interpreter "groovy")

;; LB-Datalog mode
(use-package lb-datalog-mode
  :mode "\\.logic\\'"
  :load-path "lb-datalog-mode/")
