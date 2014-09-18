EMACS     := emacs --batch
INSTALL   := install
WGET      := wget -q

emacs.dir := $(HOME)/.emacs.d
elisp.dir := $(emacs.dir)/site-lisp
elisp.src := init.el

# Add various elisp Modules
elisp.src += $(wildcard  site-lisp/etc/*.el)
elisp.src += $(wildcard  site-lisp/autodisass-java-bytecode/*.el)
elisp.src += $(wildcard  site-lisp/autodisass-llvm-bitcode/*.el)
elisp.src += $(wildcard  site-lisp/lb-datalog-mode/*.el)
elisp.src += $(addprefix site-lisp/llvm/,emacs.el llvm-mode.el tablegen-mode.el)
elisp.src += $(addprefix site-lisp/use-package/, use-package.el bind-key.el)
elisp.out := $(addprefix $(emacs.dir)/, $(elisp.src))
elisp.lib := $(addprefix -L $(elisp.dir)/,etc use-package)

# Compiled files
elisp.out += $(addsuffix c,$(filter-out $(emacs.dir)/init.el,$(elisp.out)))


# Placeholder
all:


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


#----------------------------------------
# Installing Elisp files
#----------------------------------------

.PHONY: all
all: $(elisp.out)

$(emacs.dir)/%.el: %.el
	$(info ... [elisp] installing $* ...)
	$(INSTALL) -m 444 -D $< $@

$(filter %.elc,$(elisp.out)): %.elc: %.el
	$(EMACS) $(elisp.lib) -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(elisp.out)

.PHONY: clean.elc
clean.elc:
	rm -f $(filter %.elc,$(elisp.out))


#----------------------------------------
# Thesaurus Specific
#----------------------------------------

thesaurus     := $(emacs.dir)/thesaurus/mthesaur.txt
thesaurus.url := ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
thesaurus.zip := /tmp/mthesaur.zip

.INTERMEDIATE: $(thesaurus.zip)
$(thesaurus.zip):
	$(info ... [elisp] downloading thesaurus ...)
	$(WGET) $(thesaurus.url) -O /tmp/mthesaur.zip

export UNZIP := -qq
$(thesaurus): $(thesaurus.zip)
	unzip -u $< -d $(@D)

.PHONY: thesaurus
thesaurus: $(thesaurus)


#----------------------------------------
# CEDET Setup
#----------------------------------------

cedet.dir := site-lisp/cedet

.PHONY: $(cedet.dir)
$(cedet.dir):
	$(MAKE) --directory=$@

$(elisp.dir)/cedet:
	ln -s $(abspath $(cedet.dir)) $@

cedet: $(cedet.dir) | $(elisp.dir)/cedet


#----------------------------------------
# Dependencies
#----------------------------------------

use-package.dir := $(elisp.dir)/use-package

$(use-package.dir)/use-package.elc: $(use-package.dir)/bind-key.el
