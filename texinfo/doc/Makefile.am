## Makefile.am for texinfo/emacs.
## $Id: Makefile.am,v 1.1.1.2 1998/03/24 18:20:38 law Exp $
## Run automake in .. to produce Makefile.in from this.

info_TEXINFOS = info-stnd.texi info.texi texinfo.txi

# Use the programs built in our distribution.
MAKEINFO = ../makeinfo/makeinfo
INSTALL_INFO = ../util/install-info

# Include our texinfo.tex, not Automake's.
EXTRA_DIST = macro.texi userdoc.texi epsf.tex texinfo.tex

# We try to discover this via configure just to give a better help message.
TEXMF = @TEXMF@

install-data-local:
	@echo "WARNING: You must install texinfo.tex and epsf.tex manually,"
	@echo "WARNING: perhaps in $(TEXMF)/tex/texinfo/"
	@echo "WARNING: and $(TEXMF)/tex/generic/dvips/ respectively."
	@echo "WARNING: See doc/README for some considerations."

# Do not create info files for distribution.
dist-info:

# Do not try to build the info files in $(srcdir),
# since we don't distribute them.
.texi.info:
	$(MAKEINFO) -I$(srcdir) `echo $< | sed 's,.*/,,'`
texinfo: $(srcdir)/texinfo.txi
	$(MAKEINFO) -I$(srcdir) texinfo.txi

# Similarly, Do not try to install the info files from $(srcdir).
install-info-am: $(INFO_DEPS)
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(infodir)
	@for file in $(INFO_DEPS); do \
	  d=.; \
	  for ifile in `cd $$d && echo $$file $$file-[0-9] $$file-[0-9][0-9]`; do \
	    if test -f $$d/$$ifile; then \
	      echo " $(INSTALL_DATA) $$d/$$ifile $(infodir)/$$ifile"; \
	      $(INSTALL_DATA) $$d/$$ifile $(infodir)/$$ifile; \
	    else : ; fi; \
	  done; \
	done
	@$(POST_INSTALL)
	@if $(SHELL) -c '$(INSTALL_INFO) --version | sed 1q | fgrep -s -v -i debian' >/dev/null 2>&1; then \
	  for file in $(INFO_DEPS); do \
	    echo " $(INSTALL_INFO) --info-dir=$(infodir) $(infodir)/$$file";\
	    $(INSTALL_INFO) --info-dir=$(infodir) $(infodir)/$$file || :;\
	  done; \
	else : ; fi


# Remove the info files at make distclean.
distclean-aminfo:
	rm -f texinfo texinfo-* info*.info*
