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
