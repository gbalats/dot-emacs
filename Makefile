EMACS     := emacs --batch
INSTALL   := install
WGET      := wget -q

emacs.dir := $(HOME)/.emacs.d
elisp.dir := $(emacs.dir)/site-lisp
elisp.src := init.el

# Add various elisp Modules
elisp.src += $(wildcard  site-lisp/etc/*.el)
elisp.src += $(addprefix site-lisp/llvm/,emacs.el llvm-mode.el tablegen-mode.el)
elisp.src += $(addprefix site-lisp/use-package/, use-package.el bind-key.el)
elisp.out := $(addprefix $(emacs.dir)/, $(elisp.src))
elisp.lib := $(addprefix -L $(elisp.dir)/,etc use-package lb-datalog-mode)

# Compiled files
elisp.out += $(addsuffix c,$(filter-out $(emacs.dir)/init.el,$(elisp.out)))


# Placeholder
all:


#----------------------------------------
# Miscellaneous optional utilities
#----------------------------------------

include cedet.mk				# install CEDET extensions
include llvm.mk					# install LLVM emacs extensions
include thesaurus.mk			# build thesaurs dictionary

lb-datalog.dir := site-lisp/lb-datalog-mode
lb-datalog.tar := $(wildcard $(lb-datalog.dir)/dist/lb-datalog-mode-*.tar)

$(HOME)/.cask:
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

$(lb-datalog.dir): | $(HOME)/.cask
	$(MAKE) --directory=$@ dist

$(lb-datalog.tar): $(lb-datalog.dir)
	$(EMACS) --eval "(progn (package-initialize)(package-install-file \"$@\" ))"

.PHONY: install-lb-datalog-mode $(lb-datalog.dir)
install-lb-datalog-mode: $(lb-datalog.tar)


#----------------------------------------
# Installing Elisp files
#----------------------------------------

.PHONY: all
all: $(elisp.out)

$(emacs.dir)/%.el: %.el
	$(info ... [elisp] installing $* ...)
	$(INSTALL) -m 444 -D $< $@

$(filter %.elc,$(elisp.out)): %.elc: %.el
	$(EMACS) $(elisp.lib) --eval '(progn (package-initialize)(batch-byte-compile))' $<

.PHONY: clean
clean:
	rm -f $(elisp.out)

.PHONY: clean.elc
clean.elc:
	rm -f $(filter %.elc,$(elisp.out))


#----------------------------------------
# Dependencies
#----------------------------------------

use-package.dir := $(elisp.dir)/use-package

$(use-package.dir)/use-package.elc: $(use-package.dir)/bind-key.el
