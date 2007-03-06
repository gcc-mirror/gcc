SUBDIRS = api

EXTRA_DIST = README.jaxp $(man_MANS)

## GCJ LOCAL: we don't want to install all of Classpath's info files.
## info_TEXINFOS = cp-hacking.texinfo cp-vmintegration.texinfo cp-tools.texinfo

%.dvi : %.texinfo
	texi2dvi $<

%.ps : %.dvi
	dvips -o $@ $<

docs: cp-hacking.ps cp-vmintegration.ps cp-tools.ps

man_MANS = $(TOOLS_MANFILES)
TOOLS_MANFILES = \
	gappletviewer.1 \
	gjar.1 \
	gjarsigner.1 \
	gjavah.1 \
	gkeytool.1 \
	gnative2ascii.1 \
	gorbd.1 \
	grmid.1 \
	grmiregistry.1 \
	gserialver.1 \
	gtnameserv.1

BASEVER = $(top_srcdir)/../../gcc/BASE-VER
DEVPHASE = $(top_srcdir)/../../gcc/DEV-PHASE

POD2MAN = pod2man --center="GNU" --release="gcc-$(shell cat $(BASEVER))"
TEXI2POD = perl $(top_srcdir)/../../contrib/texi2pod.pl
STAMP = echo timestamp >

# gcc-vers.texi is generated from the version files.
gcc-vers.texi: $(BASEVER) $(DEVPHASE)
	(echo "@set version-GCC $(shell cat $(BASEVER))"; \
	 if [ "$(shell cat $(DEVPHASE))" = "experimental" ]; \
	 then echo "@set DEVELOPMENT"; \
	 else echo "@clear DEVELOPMENT"; \
	 fi) > $@T
	mv -f $@T $@

%.1: %.pod
	$(STAMP) $@
	-($(POD2MAN) --section=1 $< > $(@).T$$$$ && \
		mv -f $(@).T$$$$ $@) || \
		(rm -f $(@).T$$$$ && exit 1)

.INTERMEDIATE: gappletviewer.pod gjarsigner.pod gjar.pod gjavah.pod \
	gkeytool.pod gnative2ascii.pod gorbd.pod grmid.pod grmiregistry.pod \
	gserialver.pod gtnameserv.pod

gappletviewer.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gappletviewer < $< > $@

gjarsigner.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjarsigner < $< > $@

gjar.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjar < $< > $@

gjavah.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjavah < $< > $@

# hack around the cross references and the enumeration
gkeytool.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gkeytool < $< \
	  | sed -e 's/^For more details.*/See I<Common Options> for more details./' \
	    -e 's/1\.<\([^>]*\)>/- \1/' \
		> $@

gnative2ascii.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gnative2ascii < $< > $@

gorbd.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gorbd < $< > $@

grmid.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D grmid < $< > $@

grmiregistry.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D grmiregistry < $< > $@

gserialver.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gserialver < $< > $@

gtnameserv.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gtnameserv < $< > $@
