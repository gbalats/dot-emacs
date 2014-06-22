EMACS     := emacs --batch

emacs.dir := $(HOME)/.emacs.d
elisp.src := init.el
elisp.src += $(wildcard site-lisp/*.el)
elisp.src += $(wildcard lb-datalog-mode/*.el)
elisp.src += $(wildcard autodisass-java-bytecode/*.el)
elisp.src += $(addprefix use-package/, use-package.el bind-key.el)
elisp.src += $(addprefix llvm/, emacs.el llvm-mode.el tablegen-mode.el)
elisp.out := $(addprefix $(emacs.dir)/, $(elisp.src))
emacs.lib := $(addprefix -L $(emacs.dir)/,site-lisp use-package)

# Compiled files
elisp.nocomp := $(addprefix $(emacs.dir)/, init.el site-lisp/setup-theme.el)
elisp.out    += $(addsuffix c,$(filter-out $(elisp.nocomp),$(elisp.out)))

include ../common.mk


# Placeholder
all:


#----------------------------------------
# LLVM Emacs Extensions
#----------------------------------------

elisp.llvm_url := http://llvm.org/svn/llvm-project/llvm/trunk/utils/emacs/

# Create llvm directory
llvm:
	wget -P llvm -r --no-parent -nd -A.el -e robots=off $(elisp.llvm_url)

# Download emacs-llvm sources before compiling
$(elisp.out): | llvm


#----------------------------------------
# Installing Elisp files
#----------------------------------------

.PHONY: all
all: $(elisp.out)

$(emacs.dir)/%.el: %.el
	@echo "... [elisp] installing $* ..."
	$(INSTALL) -m 444 -D $< $@

$(filter %.elc,$(elisp.out)): %.elc: %.el
	$(EMACS) $(emacs.lib) -f batch-byte-compile $<

.PHONY: clean.elc
clean.elc:
	rm -f $(filter %.elc,$(elisp.out))


#----------------------------------------
# Thesaurus Specific
#----------------------------------------

thesaurus     := $(emacs.dir)/thesaurus/mthesaur.txt
thesaurus.url := ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip

$(thesaurus):
	@echo "... [elisp] downloading thesaurus ..."
	@wget $(thesaurus.url) -O mthesaur.zip
	@unzip mthesaur.zip -d $(@D)
	@rm mthesaur.zip

.PHONY: thesaurus
thesaurus: $(thesaurus)


#----------------------------------------
# Dependencies
#----------------------------------------

$(emacs.dir)/use-package/use-package.elc: \
   $(emacs.dir)/use-package/bind-key.el
