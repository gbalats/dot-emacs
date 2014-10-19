#----------------------------------------
# LLVM Emacs Extensions
#----------------------------------------

llvm.dir := site-lisp/llvm
llvm.url := http://llvm.org/svn/llvm-project/llvm/trunk/utils/emacs/

# Create llvm directory
$(llvm.dir):
	$(WGET) -P $@ -r --no-parent -nd -A.el -e robots=off $(llvm.url)

# Download emacs-llvm sources before compiling
$(elisp.out): | $(llvm.dir)

.PHONY: llvm
llvm: $(llvm.dir)
