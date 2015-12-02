;;; init --- Emacs Initialization

;;; Commentary:

;; This is used instead of the `.emacs' file for initialization.

;;; Code:


;;-------------------------
;; Custom Variables
;;-------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(require-final-newline (quote query)) ; require newline at EOF
 ;; set some C coding style variables
 '(c-default-style (quote ((java-mode . "java")
                           (awk-mode . "awk")
                           (other . "linux"))))
 '(c-report-syntactic-errors t)
 '(c-backspace-function (quote delete-backward-char))
 '(c-basic-offset 4)
 ;; magit options
 '(magit-push-always-verify nil)
 '(magit-auto-revert-mode t)
 ;; set some haskell coding style variables
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 ;; configure tab behavior
 '(tab-width 4)
 '(tab-stop-list (number-sequence 4 80 4))
 '(indent-tabs-mode nil)
 ;; flycheck options
 '(flycheck-flake8-maximum-line-length 100)
 ;; other options
 '(auto-save-default nil)               ; no auto-save
 '(make-backup-files nil)               ; no backup files
 '(global-linum-mode t)                 ; line numbering by default
 '(inhibit-startup-screen t)            ; no startup screen
 '(delete-selection-mode t)             ; delete active region
 '(column-number-mode t)                ; show column number
 '(show-paren-mode t)                   ; show matching parentheses
 ;; configure org-mode file applications
 '(org-file-apps (quote ((auto-mode . emacs)
                         ("\\.mm\\'" . default)
                         ("\\.x?html?\\'" . default)
                         ("\\.pdf\\'" . "evince %s"))))
 ;; configure package repositories
 '(package-archives
   (quote (("gnu" . "http://elpa.gnu.org/packages/")
           ("melpa" . "http://melpa.milkbox.net/packages/")
           ("marmalade" . "http://marmalade-repo.org/packages/")))))


;; uncomment the following line to load CEDET (dev version)
;; (load-file (concat user-emacs-directory "site-lisp/etc/cedet-setup.el"))

;; specify some additional load paths
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


(require 'use-package)


;;-------------------------
;; Global keybindings
;;-------------------------


;; set the keybinding so that you can use `f4' for goto line
(global-set-key [f4] 'goto-line)

;;; Since `M-<SPC>' is used by gnome, use `C-c <SPC>' instead
(global-set-key (kbd "C-c C-<SPC>") 'just-one-space)

;; swap `C-a' with `M-m'
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "M-m") 'move-beginning-of-line)

;; centers the screen around a line with `C-l'
(global-set-key [(control l)]  'centerer)

;; Centering code stolen from somewhere and restolen from
;; http://www.chrislott.org/geek/emacs/dotemacs.html
(defun centerer ()
  "Repositions current line: once middle, twice top, thrice bottom."
  (interactive)
  (cond ((eq last-command 'centerer2)  ; 3 times pressed = bottom
         (recenter -1))
        ((eq last-command 'centerer1)  ; 2 times pressed = top
         (recenter 0)
         (setq this-command 'centerer2))
        (t                             ; 1 time pressed = middle
         (recenter)
         (setq this-command 'centerer1))))



;;-------------------------
;; Configure Status Bar
;;-------------------------

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)



;;---------------------
;; Builtin packages
;;---------------------


;; Re-compile shortcut
(use-package compile
  :bind ("C-c c" . recompile)
  :config
  (use-package project-top)
  (setq compilation-scroll-output 'first-error
        compilation-always-kill t))     ; kill old compile process

;; Dired
(use-package find-dired
  :bind ("C-c f" . find-name-dired))

;; Rebind `C-x C-b' for `buffer-menu'
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; Open shell with <f3>
(use-package shell
  :bind ("<f3>" . shell)
  :config
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook
            'ansi-color-for-comint-mode-on) ; add color to shell
  (setq comint-prompt-read-only t))         ; make shell-prompt read-only

;; Move windows with shift-arrow keys
(use-package windmove
  :config (windmove-default-keybindings 'shift)
  ;; Make windmove work in org-mode
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right))

;; Tramp
(use-package tramp
  :defer t
  :config
  (setq tramp-default-method "rsync")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave-dir")
  (setq password-cache-expiry 3600))

;; Whitespace mode
(use-package whitespace
  :commands whitespace-mode
  :bind ("C-c W" . whitespace-cleanup)
  :init
  (setq whitespace-style '(face empty tabs lines-tail trailing))
  (global-whitespace-mode t)
  (setq whitespace-global-modes
        '(c-mode c++-mode lb-datalog-mode java-mode emacs-lisp-mode
                 shell-script-mode sh-mode python-mode))
  ;; Python hook
  (add-hook 'python-mode-hook
            #'(lambda ()
                (set (make-local-variable 'whitespace-line-column) 90)))
  ;; Java hook
  (add-hook 'java-mode-hook
            #'(lambda ()
                (set (make-local-variable 'whitespace-line-column) 100))))


;;-------------------------
;; Extra packages (server)
;;-------------------------


(use-package package
  :config
  (use-package prelude-packages))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'whitespace-mode)
  (diminish 'whitespace-newline-mode)
  (diminish 'global-whitespace-mode))

;; automatic disassembly
(use-package autodisass-java-bytecode   ; auto-disassemble Java bytecode
  :ensure t)
(use-package autodisass-llvm-bitcode    ; auto-disassemble LLVM bitcode
  :ensure t)

;; Quick move minor mode
(use-package ace-jump-mode
  :ensure t
  :bind ("C-c <SPC>" . ace-jump-mode))

(use-package windresize
  :ensure t
  :bind ("C-c r" . windresize))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands smartparens-global-mode
  :config
  (require 'smartparens-config)
  :init
  (smartparens-global-mode 1))

;; Flx-Ido
(use-package flx-ido
  :disabled t
  :ensure t
  :init
  (ido-everywhere 1)
  (ido-mode 1)
  (flx-ido-mode 1)
  :config
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

;; Key chords
(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :config
  (key-chord-define-global "jf" 'ace-jump-mode)
  (key-chord-define-global "jg" 'god-mode)
  (key-chord-define-global "jl" 'other-window))

;; GNU Global Tags
(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :diminish ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1)))))

;; Magit
(use-package magit
  :ensure t
  :init (add-hook 'magit-status-mode-hook
                  (lambda () (linum-mode -1)))
  :bind ("C-c m" . magit-status))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind ("C-c s" . mc/edit-lines)
        ("C-c S" . mc/mark-more-like-this-extended))

;; enable / disable easy keys (e.g., arrows)
(use-package guru-mode
  :ensure t
  :diminish guru-mode
  :init (add-hook 'prog-mode-hook 'guru-mode)
  :config (setq guru-warn-only t))

;; Google this
(use-package google-this
  :ensure t
  :diminish google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap))

;; Thesaurus
(use-package synonyms
  :ensure t
  :commands synonyms
  :config
  (setq synonyms-file        "~/.emacs.d/thesaurus/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/thesaurus/mthesaur.txt.cache"))

;; communicating with clipboard
(use-package xclip
  :ensure t
  :disabled t)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode))

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

;; ;; FlyMake
;; (use-package flymake
;;   :ensure t
;;   :disabled t
;;   :init
;;   (add-hook 'c-mode-common-hook
;;             #'(lambda ()
;;                 (local-set-key (kbd "M-n") 'flymake-goto-next-error)
;;                 (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))
;;   ;; For some reason, flymake fails if started immediately
;;   :config
;;   (setq flymake-log-level 3)
;;   :idle (add-hook 'find-file-hook 'flymake-find-file-hook))

;; (use-package flymake-cursor
;;   :ensure t)

(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook
            (function (lambda ()
                        (setq flycheck-emacs-lisp-load-path load-path)))))

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

(use-package zenburn-theme
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-c e" . er/expand-region))

(use-package hippie-exp
  :ensure t
  :bind ("M-/" . hippie-expand))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (require 'helm-config)
  (helm-mode 1))

;; easy-kill (similar to expand-region)
(use-package easy-kill
  :ensure t
  :bind ("M-w" . easy-kill))

(use-package god-mode
  :ensure t
  :bind ("C-c g" . god-local-mode)
  :defines saved-god-mode-line-faces
  :config
  (defun god-mode-line-on ()
    "Alter the mode-line display. Intended to be used when in `god-mode'."
    (setq-local saved-god-mode-line-faces
                (cons (face-attribute 'mode-line :background)
                      (face-attribute 'mode-line :foreground)))
    (set-face-foreground 'mode-line "white")
    (set-face-background 'mode-line "red")
    (message "Entering god mode..."))
  (defun god-mode-line-off ()
    "Revert back to ordinary mode-line display."
    (when saved-god-mode-line-faces
      (set-face-background 'mode-line (car saved-god-mode-line-faces))
      (set-face-foreground 'mode-line (cdr saved-god-mode-line-faces)))
    (message "Exiting god mode..."))
  (add-hook 'god-mode-enabled-hook 'god-mode-line-on)
  (add-hook 'god-mode-disabled-hook 'god-mode-line-off))


;; Auto-complete
(use-package auto-complete
  :ensure t
  :disabled t
  :config
  ;; add to dictionary directories
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  ;; default configuration for auto-complete
  (require 'auto-complete-config)
  (ac-config-default)
  ;; include C headers
  (defun my:ac-c-header-init ()
    (require 'auto-complete-c-headers)
    (add-to-list 'ac-sources 'ac-source-c-headers)
    (add-to-list 'achead:include-directories '"/usr/include"))
  ;; call this function from c/c++ hooks
  (add-hook 'c++-mode-hook 'my:ac-c-header-init)
  (add-hook 'c-mode-hook 'my:ac-c-header-init))


;; YASnippet Programming Templates
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))



;;-------------------------
;; Major Modes (server)
;;-------------------------

;; C/C++ and Java modes
(use-package cc-mode
  :init
  ;; Java hook
  (defun my-java-mode-hook ()
    "Personalized java mode."
    (c-set-offset 'inline-open 0))
  ;; C++ hook
  (defun my-c++-mode-hook ()
    "Personalized c++ mode."
    (setq c-basic-offset 4)
    (c-set-offset 'inline-open 0)
    (c-set-offset 'brace-list-open '*)
    (c-set-offset 'block-open 0)
    (c-set-offset 'inline-open 0)
    (c-set-offset 'case-label '*)
    (c-set-offset 'access-label '/))
  ;; Add hooks
  (add-hook 'c++-mode-hook 'my-c++-mode-hook)
  (add-hook 'java-mode-hook 'my-java-mode-hook))

;; PHP mode
(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

;; Groovy mode
(use-package groovy-mode
  :ensure t
  :mode "\\.groovy\\'"
  :interpreter "groovy"
  :config
  (progn
    (use-package inf-groovy)))



;;-------------------------
;; Major Modes (local)
;;-------------------------


;; LB-Datalog mode
(use-package lb-datalog-mode
  :mode "\\.logic\\'")

;; LLVM mode
(use-package llvm-mode
  :mode "\\.ll\\'"
  :load-path "llvm/")



;;-------------------------
;; Custom Faces
;;-------------------------


;; TODO: remove this in the future as it is replicating the faces
;; configured by zenburn, which for some reason they are not set.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:foreground "#BC8383" :underline t :weight bold))))
 '(flymake-warnline ((t (:foreground "#DFAF8F" :underline t :weight bold)))))

;;; init.el ends here
