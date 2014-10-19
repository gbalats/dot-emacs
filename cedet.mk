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
