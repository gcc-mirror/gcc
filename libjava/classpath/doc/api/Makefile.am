if CREATE_API_DOCS
noinst_DATA = html
endif

sourcepath = $(top_builddir):$(top_srcdir):$(top_srcdir)/vm/reference:$(top_srcdir)/external/w3c_dom:$(top_srcdir)/external/sax

classpathbox = "<span class='logo'><a href='http://www.gnu.org/software/classpath' target='_top'>GNU Classpath</a> ($(VERSION))"

if CREATE_API_DOCS
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/api
	@list='$(htmllist)'; for p in $$list; do \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  if test -f "$$p"; then \
	    echo " $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/api/$$f"; \
	    $(INSTALL_DATA) $$p $(DESTDIR)$(pkgdatadir)/api/$$f; \
	  elif test -d "$$p"; then \
	    $(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/api/$$f; \
          fi; \
        done

uninstall-local:
	@list='$(htmllist)'; for p in $$list; do \
	  f="`echo $$p | sed -e 's|^.*/||'`"; \
	  if test -f "$$p"; then \
	    echo " rm -f $(DESTDIR)$(pkgdatadir)/api/$$f"; \
	    rm -f $(DESTDIR)$(pkgdatadir)/api/$$f; \
          fi; \
        done
endif

html: create_html

clean-local:
	-rm -rf html create_html gjdoc_rawcomment.cache

create_html:
	-$(MKDIR) html > /dev/null 2>&1
if CREATE_API_DOCS
	$(GJDOC) \
	-use \
	-sourcepath "$(sourcepath)" \
	-encoding UTF-8 \
	-breakiterator \
	-licensetext \
	-linksource \
	-splitindex \
	-validhtml \
	-d html \
	-doctitle "GNU Classpath $(VERSION)" \
	-windowtitle "GNU Classpath $(VERSION) Documentation" \
	-header $(classpathbox) -footer $(classpathbox) \
	-subpackages java:javax:org
	touch create_html
endif
