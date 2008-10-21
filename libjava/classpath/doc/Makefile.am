SUBDIRS = api

EXTRA_DIST = README.jaxp texi2pod.pl $(man_MANS)

## GCJ LOCAL: we don't want to install all of Classpath's info files.
## info_TEXINFOS = cp-hacking.texinfo cp-vmintegration.texinfo cp-tools.texinfo
TEXINFO_TEX = ../../gcc/doc/include/texinfo.tex
info_TEXINFOS = cp-tools.texinfo

.texinfo.dvi:
	texi2dvi $<

.dvi.ps:
	dvips -o $@ $<

docs: cp-hacking.ps cp-vmintegration.ps cp-tools.ps $(TOOLS_MANFILES)

man_MANS = $(TOOLS_MANFILES)
TOOLS_MANFILES = \
	gappletviewer.1 \
	gjar.1 \
	gjarsigner.1 \
	gjavah.1 \
	gcjh.1 \
	gkeytool.1 \
	gnative2ascii.1 \
	gorbd.1 \
	grmid.1 \
	grmiregistry.1 \
	gserialver.1 \
	gtnameserv.1 \
	gjdoc.1

POD2MAN = pod2man --center="GNU" --release="$(VERSION)"
TEXI2POD = perl $(srcdir)/texi2pod.pl
STAMP = echo timestamp >

.pod.1:
	$(STAMP) $@
	-($(POD2MAN) --section=1 $< > $(@).T$$$$ && \
		mv -f $(@).T$$$$ $@) || \
		(rm -f $(@).T$$$$ && exit 1)

.INTERMEDIATE: gappletviewer.pod gjarsigner.pod gjar.pod gjavah.pod \
	gkeytool.pod gnative2ascii.pod gorbd.pod grmid.pod grmiregistry.pod \
	gserialver.pod gtnameserv.pod gcjh.pod gjdoc.pod

gappletviewer.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gappletviewer < $< > $@

gjarsigner.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjarsigner < $< > $@

gjar.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjar < $< > $@

gcjh.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gcjh < $< > $@

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

gjdoc.pod: $(srcdir)/cp-tools.texinfo
	-$(TEXI2POD) -D gjdoc < $< > $@

# GCJ LOCAL CHANGE
#CLEANFILES = $(TOOLS_MANFILES)

# GCJ LOCAL CHANGE
# The following commands allow us to release tarballs with the man pages
# and info documentation prebuilt.  This feature is enabled via 
# --enable-generated-files-in-srcdir in the configure script.

if GENINSRC
STAMP_GENINSRC = stamp-geninsrc
else
STAMP_GENINSRC =
endif

all-local: $(STAMP_GENINSRC) 

stamp-geninsrc: $(TOOLS_MANFILES) cp-tools.info
	-cp -p gappletviewer.1 $(srcdir)/gappletviewer.1
	-cp -p gjar.1 $(srcdir)/gjar.1
	-cp -p gjarsigner.1 $(srcdir)/gjarsigner.1
	-cp -p gjavah.1 $(srcdir)/gjavah.1
	-cp -p gjdoc.1 $(srcdir)/gjdoc.1
	-cp -p gkeytool.1 $(srcdir)/gkeytool.1
	-cp -p gnative2ascii.1 $(srcdir)/gnative2ascii.1
	-cp -p gorbd.1 $(srcdir)/gorbd.1
	-cp -p grmid.1 $(srcdir)/grmid.1
	-cp -p grmiregistry.1 $(srcdir)/grmiregistry.1
	-cp -p gserialver.1 $(srcdir)/gserialver.1
	-cp -p gtnameserv.1 $(srcdir)/gtnameserv.1
	-cp -p cp-tools.info $(srcdir)/cp-tools.info
	touch $@

CLEANFILES = stamp-geninsrc cp-tools.info
MAINTAINERCLEANFILES = \
		$(srcdir)/gappletviewer.1 \
		$(srcdir)/gjar.1 \
		$(srcdir)/gjarsigner.1 \
		$(srcdir)/gjavah.1 \
		$(srcdir)/gjdoc.1 \
		$(srcdir)/gkeytool.1 \
		$(srcdir)/gnative2ascii.1 \
		$(srcdir)/gorbd.1 \
		$(srcdir)/grmid.1 \
		$(srcdir)/grmiregistry.1 \
		$(srcdir)/gserialver.1 \
		$(srcdir)/gtnameserv.1 \
		$(srcdir)/cp-tools.info
