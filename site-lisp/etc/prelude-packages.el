;;; prelude-packages --- My prelude packages

;;; Commentary:

;; Packages to be installed automatically.

;;; Code:

(require 'cl)
(require 'package)

(defconst prelude-packages
  '(auctex clojure-mode coffee-mode groovy-mode haskell-mode
           markdown-mode javap-mode deft gist paredit projectile
           rainbow-mode solarized-theme color-theme zenburn-theme
           auto-complete auto-complete-c-headers ac-dabbrev
           python volatile-highlights yaml-mode yari)
  "A list of packages to ensure are installed at launch.")

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package prelude-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'prelude-packages)

;;; prelude-packages.el ends here

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
