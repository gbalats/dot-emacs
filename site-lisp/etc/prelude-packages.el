;;; prelude-packages --- My prelude packages

;;; Commentary:

;; Packages to be installed automatically.

;;; Code:

(require 'cl)
(require 'package)

(defconst prelude-packages
  '(deft gist markdown-mode rainbow-mode volatile-highlights yaml-mode yari)
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
