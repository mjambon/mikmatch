.PHONY: default install uninstall reinstall \
        all opt mikmatch-pcre mikmatch-str \
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


default: mikmatch-pcre
install: install-pcre
uninstall: uninstall-pcre
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

## GODIVA/GODI targets
all: common
	cd pcre && $(MAKE) all-bc
opt: common
	cd pcre && $(MAKE) all-nc

## end of GODIVA targets

mikmatch-pcre: common pcre
mikmatch-str: common str

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
	scp -r . $$BACKUP_DIR/mikmatch/

clean::
	cd doc && $(MAKE) clean
	cd common && $(MAKE) clean
	cd str && $(MAKE) clean
	cd pcre && $(MAKE) clean



VERSION = $(shell ./VERSION)
export VERSION

install: OCAMLFIND_INSTFLAGS = -patch-version $(VERSION)
export OCAMLFIND_INSTFLAGS

# Only for developers; requires camlmix, hevea, pdflatex 
# and maybe other things.
archive:
	@echo "Making archive for version $(VERSION)"
	cd str && $(MAKE) version
	cd pcre && $(MAKE) version
	cd doc && $(MAKE)
	rm -rf /tmp/mikmatch /tmp/mikmatch-$(VERSION) && \
	 	cp -r . /tmp/mikmatch && \
		cd /tmp/mikmatch && \
			$(MAKE) clean && \
			rm -rf *~ mikmatch*.tar* `find . -name .svn` && \
		cd /tmp && cp -r mikmatch mikmatch-$(VERSION) && \
		tar czf mikmatch.tar.gz mikmatch && \
		tar cjf mikmatch.tar.bz2 mikmatch && \
		tar czf mikmatch-$(VERSION).tar.gz mikmatch-$(VERSION) && \
		tar cjf mikmatch-$(VERSION).tar.bz2 mikmatch-$(VERSION)
	mv /tmp/mikmatch.tar.gz /tmp/mikmatch.tar.bz2 .
	mv /tmp/mikmatch-$(VERSION).tar.gz /tmp/mikmatch-$(VERSION).tar.bz2 .
	cp mikmatch.tar.gz mikmatch.tar.bz2 $$WWW/
	cp mikmatch-$(VERSION).tar.gz mikmatch-$(VERSION).tar.bz2 $$WWW/
	cp LICENSE $$WWW/mikmatch-license.txt
	cp VERSION $$WWW/mikmatch-version
	cp Changes $$WWW/mikmatch-changes.txt
	$(MAKE) install-www-doc

install-www-doc:
	cp doc/mikmatch-manual.pdf $$WWW
	cp doc/mikmatch-manual.html $$WWW/mikmatch-manual-nocounter.html
	cp doc/mikmatch-ocamldoc/* $$WWW/mikmatch-ocamldoc
	touch -c $$WWW/mikmatch.html.mlx
