# Makefile for libga68.
#   Copyright (C) 2025 Jose E. Marchesi.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
#
# if this file is changed then you need to run
#
# autoreconf2.69

AUTOMAKE_OPTIONS = 1.8 foreign
ACLOCAL_AMFLAGS = -I .. -I ../config
# Multilib support.
MAKEOVERRIDES=

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)
TOP_GCCDIR := $(shell cd $(top_srcdir) && cd .. && pwd)

GCC_DIR = $(TOP_GCCDIR)/gcc
A68_SRC = $(GCC_DIR)/algol68

toolexeclibdir=@toolexeclibdir@
toolexecdir=@toolexecdir@
A68_FOR_TARGET=@A68_FOR_TARGET@

extra_darwin_ldflags_libga68=@extra_darwin_ldflags_libga68@

if ENABLE_DARWIN_AT_RPATH
extra_darwin_ldflags_libga68 += -Wc,-nodefaultrpaths
extra_darwin_ldflags_libga68 += -Wl,-rpath,@loader_path
endif

A68_BUILDDIR := $(shell pwd)

if LIBGA68_USE_SYMVER
if LIBGA68_USE_SYMVER_GNU
version_arg = -Wl,--version-script=$(srcdir)/ga68.map
version_dep = $(srcdir)/ga68.map
endif
if LIBGA68_USE_SYMVER_SUN
version_arg = -Wl,-M,ga68.map-sun
version_dep = ga68.map-sun
ga68.map-sun : $(srcdir)/ga68.map \
		$(top_srcdir)/../contrib/make_sunver.pl \
		$(libga68_la_OBJECTS) $(libga68_la_LIBADD)
	perl $(top_srcdir)/../contrib/make_sunver.pl \
	  $(srcdir)/ga68.map \
	  $(libga68_la_OBJECTS) $(libga68_la_LIBADD) \
	> $@ || (rm -f $@ ; exit 1)
endif
else
version_arg =
version_dep =
endif

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
        "GCC_DIR=$(GCC_DIR)" \
        "A68_SRC=$(A68_SRC)" \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"A68_FOR_TARGET=$(A68_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
        "CFLAGS_LONGDOUBLE=$(CFLAGS_LONGDOUBLE)" \
	"EXPECT=$(EXPECT)" \
	"INSTALL=$(INSTALL)" \
	"INSTALL_DATA=$(INSTALL_DATA)" \
	"INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
	"INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
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
	"includedir=$(includedir)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" \
	"gxx_include_dir=$(gxx_include_dir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"LD=$(LD)" \
	"RANLIB=$(RANLIB)" \
	"NM=$(NM)" \
	"NM_FOR_BUILD=$(NM_FOR_BUILD)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"DESTDIR=$(DESTDIR)" \
	"WERROR=$(WERROR)" \
        "TARGET_LIB_PATH=$(TARGET_LIB_PATH)" \
        "TARGET_LIB_PATH_libgm2=$(TARGET_LIB_PATH_libgm2)" \
	"LIBTOOL=$(A68_BUILDDIR)/libtool" \
	"DARWIN_AT_RPATH=$(DARWIN_AT_RPATH)"

# Subdir rules rely on $(FLAGS_TO_PASS)
FLAGS_TO_PASS = $(AM_MAKEFLAGS)

gcc_objdir = $(MULTIBUILDTOP)../../$(host_subdir)/gcc

toolexeclib_DATA = libga68.spec
toolexeclib_LTLIBRARIES = libga68.la

libga68_la_SOURCES = libga68.c \
                     ga68-unistr.c \
                     ga68-posix.c \
                     ga68-alloc.c \
                     ga68-error.c \
                     ga68-standenv.c \
                     ga68.h
libga68_la_LIBTOOLFLAGS =
libga68_la_CFLAGS = $(LIBGA68_GCFLAGS) $(LIBGA68_BOEHM_GC_INCLUDES)
libga68_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
		     $(version_arg) $(lt_host_flags) $(extra_darwin_ldflags_libga68)
libga68_la_DEPENDENCIES = libga68.spec $(version_dep) transput.lo
libga68_la_LIBADD = $(LIBGA68_BOEHM_GC_LIBS) transput.lo

# Rules to build the Algol 68 code in the library.

LTA68COMPILE = $(LIBTOOL) --tag=A68 $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(A68) $(AM_A68FLAGS)

.a68.o:
	$(A68) -o $@ $(A68FLAGS) -c $<

.a68.lo:
	$(LTA68COMPILE) $(A68FLAGS) $(MULTIFLAGS) -c -o $@ $<

transput.a68 : transput.a68.in
	$(AWK) -f $(srcdir)/sppp.awk $< > $@

BUILT_SOURCES = transput.a68

# target overrides
-include $(tmake_file)

include $(top_srcdir)/../multilib.am
