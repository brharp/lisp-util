# Do not edit.
LISPC   = clisp -q -c
PREFIX   = /usr/local
SYSCONFDIR = /usr/local/etc
PKGDATADIR = /usr/local/share
SHELL	        = /bin/sh
PACKAGE_NAME    = lisp-util
PACKAGE_VERSION = 1.0
DISTDIR         = $(PACKAGE_NAME)-$(PACKAGE_VERSION)
OBJECTS         = xml.fas http.fas zimbra.fas posix.fas cgi.fas ical.fas \
                  xml-template.fas db.fas db2.fas io.fas
SOURCES         = db2.lisp

default: compile

makefiles Makefiles:
	(echo "# Do not edit."; $(SHELL) makedefs) >makedefs.tmp
	set +e; if cmp makedefs.tmp makedefs.out; then rm makedefs.tmp; \
	else mv makedefs.tmp makedefs.out; fi >/dev/null 2>/dev/null
	rm -f Makefile; (cat makedefs.out Makefile.in) >Makefile

compile: config.lisp $(OBJECTS)

%.fas: %.lisp
	$(LISPC) $<

install: compile
	mkdir -p $(DESTDIR)$(PREFIX)/lib/lisp
	install -m 644 $(OBJECTS) $(DESTDIR)$(PREFIX)/lib/lisp
	install -m 644 $(SOURCES) $(DESTDIR)$(PREFIX)/lib/lisp

clean:
	rm -f config.lisp
	rm -f $(OBJECTS)

dist:
	mkdir -p $(DISTDIR)
	cp *.in $(DISTDIR)
	cp *.lisp $(DISTDIR)
	tar -czf $(DISTDIR).tgz $(DISTDIR)
	rm -rf $(DISTDIR)

edit = sed \
	-e 's|@prefix[@]|$(PREFIX)|g' \
	-e 's|@sysconfdir[@]|$(SYSCONFDIR)|g' \
	-e 's|@pkgdatadir[@]|$(PKGDATADIR)|g'

config.lisp:
	$(edit) $${srcdir}$@.in > $@

config.lisp: config.lisp.in


