EMACS     := emacs

emacs.dir := $(HOME)/.emacs.d
elisp     := init.el
elisp     += $(wildcard site-lisp/*.el)
elisp     += $(wildcard lb-datalog-mode/*.el)

include ../common.mk

# Installing Elisp files

.PHONY: all
all: $(addprefix $(emacs.dir)/, $(elisp))

$(emacs.dir)/%.el: %.el
	@echo "... [elisp] installing $* ..."
	$(INSTALL) -m 444 -D $< $@

# Thesaurus Specific

thesaurus     := $(emacs.dir)/thesaurus/mthesaur.txt
thesaurus.url := ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip

$(thesaurus):
	@echo "... [elisp] downloading thesaurus ..."
	@wget $(thesaurus.url) -O mthesaur.zip
	@unzip mthesaur.zip -d $(@D)
	@rm mthesaur.zip

.PHONY: thesaurus
thesaurus: $(thesaurus)
