.PHONY: defaults install uninstall all opt micmatch-pcre micmatch-str \
        common install-str install-pcre uninstall-str uninstall-pcre \
        backup clean archive

ifndef PREFIX
  BINDIR = $(shell dirname `which ocaml`)
  PREFIX = $(shell dirname $(BINDIR))
else
  BINDIR = $(PREFIX)/bin
endif
export PREFIX
export BINDIR


PP = camlp4orf
export PP


default: micmatch-pcre
install: install-pcre
uninstall: uninstall-pcre

## GODIVA/GODI targets
all: common
	cd micmatch_pcre && $(MAKE) all-bc
opt: common
	cd micmatch_pcre && $(MAKE) all-nc

## end of GODIVA targets

micmatch-pcre: common pcre
micmatch-str: common str

common:
	cd micmatch_common && $(MAKE)

str: common
	cd micmatch_str && $(MAKE)
pcre: common
	cd micmatch_pcre && $(MAKE)

install-str:
	cd micmatch_str && $(MAKE) install
install-pcre:
	cd micmatch_pcre && $(MAKE) install

uninstall-str:
	cd micmatch_str && $(MAKE) uninstall
uninstall-pcre:
	cd micmatch_pcre && $(MAKE) uninstall


backup:
	scp -r . $$BACKUP_DIR/micmatch/

clean::
	cd doc && $(MAKE) clean
	cd micmatch_common && $(MAKE) clean
	cd micmatch_str && $(MAKE) clean
	cd micmatch_pcre && $(MAKE) clean



VERSION = $(shell ./VERSION)

# Only for developers; requires camlmix, hevea, pdflatex 
# and maybe other things.
archive:
	@echo "Making archive for version $(VERSION)"
	cd micmatch_str && $(MAKE) version
	cd micmatch_pcre && $(MAKE) version
	cd doc && $(MAKE)
	rm -rf /tmp/micmatch /tmp/micmatch-$(VERSION) && \
	 	cp -r . /tmp/micmatch && \
		cd /tmp/micmatch && \
			$(MAKE) clean && \
			rm -f *~ micmatch*.tar* && \
		cd /tmp && cp -r micmatch micmatch-$(VERSION) && \
		tar czf micmatch.tar.gz micmatch && \
		tar cjf micmatch.tar.bz2 micmatch && \
		tar czf micmatch-$(VERSION).tar.gz micmatch-$(VERSION) && \
		tar cjf micmatch-$(VERSION).tar.bz2 micmatch-$(VERSION)
	mv /tmp/micmatch.tar.gz /tmp/micmatch.tar.bz2 .
	mv /tmp/micmatch-$(VERSION).tar.gz /tmp/micmatch-$(VERSION).tar.bz2 .
	cp micmatch.tar.gz micmatch.tar.bz2 $$WWW/
	cp micmatch-$(VERSION).tar.gz micmatch-$(VERSION).tar.bz2 $$WWW/
	cp LICENSE $$WWW/micmatch-license.txt
	cp VERSION $$WWW/micmatch-version
	cp Changes $$WWW/micmatch-changes.txt
	cp doc/micmatch-manual.pdf $$WWW
	cp doc/micmatch-manual.html $$WWW/micmatch-manual-nocounter.html
	cp -r doc/mmodoc/ $$WWW
	touch -c $$WWW/micmatch.html.mlx
