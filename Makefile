EMACS     := emacs --batch
INSTALL   := install
MKDIR     := mkdir -p
WGET      := wget -q

emacs.dir := $(HOME)/.emacs.d
elisp.dir := $(emacs.dir)/site-lisp
elisp.src := init.el

# Add various elisp Modules
loadextra := 0

elisp.src += $(wildcard  site-lisp/etc/project-top.el)
elisp.out := $(addprefix $(emacs.dir)/, $(elisp.src))

# Compiled files
elisp.out += $(addsuffix c,$(filter-out $(emacs.dir)/init.el,$(elisp.out)))


# Placeholder
all:


ifeq ($(loadextra), 1)

elisp.src += $(wildcard  site-lisp/etc/*.el)
elisp.src += $(addprefix site-lisp/llvm/,emacs.el llvm-mode.el tablegen-mode.el)
elisp.lib := $(addprefix -L $(elisp.dir)/,etc lb-datalog-mode)

include cedet.mk				# install CEDET extensions
include llvm.mk					# install LLVM emacs extensions
include thesaurus.mk			# build thesaurs dictionary

endif

#----------------------------------------
# Miscellaneous optional utilities
#----------------------------------------

$(HOME)/.cask:
	curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

install-lb-datalog-mode: | $(HOME)/.cask
	$(MAKE) --directory=site-lisp/lb-datalog-mode dist install

.PHONY: install-lb-datalog-mode


#----------------------------------------
# Installing Elisp files
#----------------------------------------

.PHONY: all
all: $(elisp.out)

$(emacs.dir)/%.el: %.el
	$(info ... [elisp] installing $* ...)
	@$(MKDIR) $(@D)
	$(INSTALL) -m 444 $< $@

$(filter %.elc,$(elisp.out)): %.elc: %.el
	$(EMACS) $(elisp.lib) --eval '(progn (package-initialize)(batch-byte-compile))' $<

.PHONY: clean
clean:
	rm -f $(elisp.out)

.PHONY: clean.elc
clean.elc:
	rm -f $(filter %.elc,$(elisp.out))
