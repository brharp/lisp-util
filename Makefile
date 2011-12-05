#CLISP=clisp -q
LISPC=ecl -q -compile
OBJECTS=xml.fas http.fas zimbra.fas posix.fas
INSTALLDIR?=/usr/local
PACKAGE_NAME=zmlisp
PACKAGE_VERSION=1.0
DISTDIR=$(PACKAGE_NAME)-$(PACKAGE_VERSION)

compile: $(OBJECTS)

%.fas: %.lisp
	$(LISPC) $<

install: compile
	install -m 644 $(OBJECTS) $(INSTALLDIR)/lib/lisp

clean:
	rm -f $(OBJECTS)

dist:
	mkdir -p $(DISTDIR)
	cp *.lisp $(DISTDIR)
	cp Makefile $(DISTDIR)
	tar -czf $(DISTDIR).tgz $(DISTDIR)

