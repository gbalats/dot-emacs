;; cedet-projects --- this file contains project metadata for CEDET

;;; Commentary:

;; The entries should be of the following form:
;;
;; (when (and (file-exists-p "~/repo/llvm-datalog")
;;            (fboundp 'ede-cpp-root-project))
;;   (ede-cpp-root-project "fact-generator"
;;                         :name "LLVM-Datalog Fact Generator Project"
;;                         :file "~/repo/llvm-datalog/fact-generator/Makefile"
;;                         :include-path '("/src" "/include")
;;                         :system-include-path '("/opt/llvm/default/include")))

;;; Code:
