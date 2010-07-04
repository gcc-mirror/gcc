## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = no-dependencies foreign no-dist

ACLOCAL_AMFLAGS = -I ../.. -I ../../config

INCLUDES = $(GCINCS)

if INSTALL_LTDL
include_HEADERS = ltdl.h
lib_LTLIBRARIES = libltdl.la
else
noinst_HEADERS = ltdl.h
endif

if CONVENIENCE_LTDL
noinst_LTLIBRARIES = libltdlc.la
endif

## Make sure these will be cleaned even when they're not built by
## default.
CLEANFILES = libltdl.la libltdlc.la

libltdl_la_SOURCES = ltdl.c
libltdl_la_LDFLAGS = -no-undefined -version-info 4:1:1
libltdl_la_LIBADD = $(LIBADD_DL)

libltdlc_la_SOURCES = ltdl.c
libltdlc_la_LIBADD = $(LIBADD_DL)

## Because we do not have automatic dependency tracking:
ltdl.lo: ltdl.h config.h

$(libltdl_la_OBJECTS) $(libltdlc_la_OBJECTS): libtool
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

## These are installed as a subdirectory of pkgdatadir so that
## libtoolize --ltdl can find them later:
ltdldatadir = $(datadir)/libtool/libltdl
ltdldatafiles = COPYING.LIB README acinclude.m4 aclocal.m4 \
		Makefile.am Makefile.in configure.ac configure \
		config-h.in config.guess config.sub \
		install-sh missing ltmain.sh \
		ltdl.c ltdl.h

## GCJ LOCAL: fix install buglet.  See PR 27818
if INSTALL_LTDL
## To avoid spurious reconfiguration when the user installs these files
## with libtoolize, we have to preserve their timestamps carefully:
install-data-local:
	-rm -rf $(DESTDIR)$(ltdldatadir)
	$(mkinstalldirs) $(DESTDIR)$(ltdldatadir)
	( cd $(srcdir) && $(AMTAR) chf - $(ltdldatafiles); ) \
	  | ( umask 0 && cd $(DESTDIR)$(ltdldatadir) && $(AMTAR) xf -; )
endif
