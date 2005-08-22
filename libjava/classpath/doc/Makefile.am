SUBDIRS = api

EXTRA_DIST = README.jaxp

## GCJ LOCAL: we don't want to install Classpath's info files.
## info_TEXINFOS = hacking.texinfo vmintegration.texinfo

%.dvi : %.texinfo
	texi2dvi $<

%.ps : %.dvi
	dvips -o $@ $<

docs: hacking.ps vmintegration.ps
