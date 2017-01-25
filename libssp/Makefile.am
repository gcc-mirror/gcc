## Makefile for the toplevel directory of the libssp library.
##
## Copyright (C) 2005-2017 Free Software Foundation, Inc.
##

AUTOMAKE_OPTIONS = 1.9.5 foreign
ACLOCAL_AMFLAGS = -I .. -I ../config
MAINT_CHARSET = latin1

# May be used by various substitution variables.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

if LIBSSP_USE_SYMVER
if LIBSSP_USE_SYMVER_GNU
version_arg = -Wl,--version-script=$(srcdir)/ssp.map
version_dep = $(srcdir)/ssp.map
endif
if LIBSSP_USE_SYMVER_SUN
version_arg = -Wl,-M,ssp.map-sun
version_dep = ssp.map-sun
ssp.map-sun : $(srcdir)/ssp.map \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libssp_la_OBJECTS) $(libssp_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  $(srcdir)/ssp.map \
	  $(libssp_la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libssp_la_LIBADD) | \
	    sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
else
version_arg =
version_dep =
endif

AM_CFLAGS = -Wall

toolexeclib_LTLIBRARIES = libssp.la libssp_nonshared.la

target_noncanonical = @target_noncanonical@
libsubincludedir = $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/include
nobase_libsubinclude_HEADERS = ssp/ssp.h ssp/string.h ssp/stdio.h ssp/unistd.h

libssp_la_SOURCES = \
	ssp.c gets-chk.c memcpy-chk.c memmove-chk.c mempcpy-chk.c \
	memset-chk.c snprintf-chk.c sprintf-chk.c stpcpy-chk.c \
	strcat-chk.c strcpy-chk.c strncat-chk.c strncpy-chk.c \
	vsnprintf-chk.c vsprintf-chk.c
libssp_la_LIBADD = 
libssp_la_DEPENDENCIES = $(version_dep) $(libssp_la_LIBADD)
libssp_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
		    $(version_arg) $(lt_host_flags)

libssp_nonshared_la_SOURCES = \
	ssp-local.c
libssp_nonshared_la_CFLAGS = -prefer-pic
libssp_nonshared_la_LIBADD = 
libssp_nonshared_la_DEPENDENCIES = $(libssp_nonshared_la_LIBADD)
libssp_nonshared_la_LDFLAGS = -static


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

