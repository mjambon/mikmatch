.PHONY: defaults install uninstall all opt micmatch-pcre micmatch-str \
        common install-str install-pcre uninstall-str uninstall-pcre \
        backup clean archive pcre str

ifndef PREFIX
  BINDIR = $(shell dirname `which ocaml`)
  PREFIX = $(shell dirname $(BINDIR))
else
  BINDIR = $(PREFIX)/bin
endif
export PREFIX
export BINDIR


default: micmatch-pcre
install: install-pcre
uninstall: uninstall-pcre

## GODIVA/GODI targets
all: common
	cd pcre && $(MAKE) all-bc
opt: common
	cd pcre && $(MAKE) all-nc

## end of GODIVA targets

micmatch-pcre: common pcre
micmatch-str: common str

common:
	cd common && $(MAKE)

str: common
	cd str && $(MAKE)
pcre: common
	cd pcre && $(MAKE)

install-str:
	cd str && $(MAKE) install
install-pcre:
	cd pcre && $(MAKE) install

uninstall-str:
	cd str && $(MAKE) uninstall
uninstall-pcre:
	cd pcre && $(MAKE) uninstall


backup:
	scp -r . $$BACKUP_DIR/micmatch/

clean::
	cd doc && $(MAKE) clean
	cd common && $(MAKE) clean
	cd str && $(MAKE) clean
	cd pcre && $(MAKE) clean



VERSION = $(shell ./VERSION)

# Only for developers; requires camlmix, hevea, pdflatex 
# and maybe other things.
archive:
	@echo "Making archive for version $(VERSION)"
	cd str && $(MAKE) version
	cd pcre && $(MAKE) version
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
