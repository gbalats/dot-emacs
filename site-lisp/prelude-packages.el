(require 'cl)
(require 'package)

(defconst prelude-packages
  '(auctex clojure-mode coffee-mode deft expand-region
           gist groovy-mode haskell-mode magit javap-mode
           markdown-mode paredit projectile python color-theme
           rainbow-mode solarized-theme zenburn-theme
           volatile-highlights yaml-mode yari auto-complete
           ac-dabbrev synonyms)
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
