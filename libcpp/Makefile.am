localedir = $(datadir)/locale
ACLOCAL_AMFLAGS = -I ../config
AM_CPPFLAGS = -I$(srcdir)/../include -I$(srcdir) -I$(srcdir)/../intl \
	-I$(srcdir)/include -DLOCALEDIR="\"$(localedir)\""

noinst_LIBRARIES = libcpp.a
libcpp_a_SOURCES = \
	charset.c directives.c errors.c expr.c files.c \
	identifiers.c init.c lex.c line-map.c macro.c mkdeps.c \
	pch.c symtab.c traditional.c

noinst_HEADERS = \
	include/cpplib.h include/line-map.h include/mkdeps.h \
	include/symtab.h internal.h system.h ucnid.h

XGETTEXT = @XGETTEXT@
GMSGFMT = @GMSGFMT@
MSGMERGE = msgmerge
CATALOGS = @CATALOGS@

if ENABLE_NLS
build-po: $(CATALOGS)
update-po: $(CATALOGS:.gmo=.pox)
else
build-po:
update-po:
endif

# N.B. We do not attempt to copy these into $(srcdir).
.po.gmo:
	-test -d po || mkdir po
	$(GMSGFMT) --statistics -o $@ $<

# The new .po has to be gone over by hand, so we deposit it into
# build/po with a different extension.
# If build/po/$(PACKAGE).pot exists, use it (it was just created),
# else use the one in srcdir.
.po.pox:
	-test -d po || mkdir po
	$(MSGMERGE) $< `if test -f po/$(PACKAGE).pot; \
	                then echo po/$(PACKAGE).pot; \
	                else echo $(srcdir)/po/$(PACKAGE).pot; fi` -o $@

# This rule has to look for .gmo modules in both srcdir and
# the cwd, and has to check that we actually have a catalog
# for each language, in case they weren't built or included
# with the distribution.
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(datadir)
	cats="$(CATALOGS)"; for cat in $$cats; do \
	  lang=`basename $$cat | sed 's/\.gmo$$//'`; \
	  if [ -f $$cat ]; then :; \
	  elif [ -f $(srcdir)/$$cat ]; then cat=$(srcdir)/$$cat; \
	  else continue; \
	  fi; \
	  dir=$(localedir)/$$lang/LC_MESSAGES; \
	  echo $(mkinstalldirs) $(DESTDIR)$$dir; \
	  $(mkinstalldirs) $(DESTDIR)$$dir || exit 1; \
	  echo $(INSTALL_DATA) $$cat $(DESTDIR)$$dir/$(PACKAGE).mo; \
	  $(INSTALL_DATA) $$cat $(DESTDIR)$$dir/$(PACKAGE).mo; \
	done

# Rule for regenerating the message template (gcc.pot).
# Instead of forcing everyone to edit POTFILES.in, which proved impractical,
# this rule has no dependencies and always regenerates libcpp.pot.  This is
# relatively harmless since the .po files do not directly depend on it.
.PHONY: po/$(PACKAGE).pot
po/$(PACKAGE).pot: $(libcpp_a_SOURCES) $(noinst_HEADERS)
	-test -d $(srcdir)/po || mkdir $(srcdir)/po
	$(XGETTEXT) --default-domain=$(PACKAGE) \
	  --keyword=cpp_error,3 --keyword=cpp_errno,3 \
	  --keyword=cpp_error_with_line,5 \
	  --copyright-holder="Free Software Foundation, Inc." \
	  --msgid-bugs-address="http://gcc.gnu.org/bugs.html" \
	  --language=c -o po/$(PACKAGE).pot $^
