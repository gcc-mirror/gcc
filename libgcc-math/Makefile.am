## Makefile for the toplevel directory of the libgcc-math library.
##
## Copyright (C) 2005
## Free Software Foundation, Inc.
##

AUTOMAKE_OPTIONS = 1.9.5 foreign
ACLOCAL_AMFLAGS = -I .. -I ../config
MAINT_CHARSET = latin1

SUBDIRS = @arch_subdirs@

# May be used by various substitution variables.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

if LIBGCCM_USE_SYMVER
version_arg = -Wl,--version-script=gccm.map
version_dep = gccm.map
.PHONY: gccm.map
gccm.map:
	rm -f gccm.map
	for map in @arch_maps@; do \
	  cat $(srcdir)/$$map >> gccm.map; \
	done
else
version_arg =
version_dep =
endif


if BUILD_LIBGCC_MATH
toolexeclib_LTLIBRARIES = libgcc-math.la

libgcc_math_la_SOURCES =
libgcc_math_la_LIBADD = @arch_libraries@
libgcc_math_la_DEPENDENCIES = $(libgcc_math_la_LIBADD) $(version_dep)
libgcc_math_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(version_arg) -lm
endif


# XXX hack alert
# From libffi/Makefile.am

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
	"JC1FLAGS=$(JC1FLAGS)" \
	"LDFLAGS=$(LDFLAGS)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
	"MAKE=$(MAKE)" \
	"MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
	"SHELL=$(SHELL)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"prefix=$(prefix)" \
	"includedir=$(includedir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

MAKEOVERRIDES=

## ################################################################

