## Makefile for the toplevel directory of the GNU C++ Standard library.
##
## Copyright (C) 1997,1998, 1999, 2000 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING.  If not, write to the Free
## Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
## USA.

AUTOMAKE_OPTIONS = 1.3 cygnus
MAINT_CHARSET = latin1

# This helps subvert libstdcxx_interface, as calculated by devo/config.if
# Needed so that g++ can find the correct include subdir automatically.
INTERFACE = v3
gxx_include_dir=$(includedir)/g++

SUBDIRS = math libio src

check:  $(top_builddir)/mkcheck
	$(top_builddir)/mkcheck 0 $(top_builddir) $(top_srcdir)

check-install: $(top_builddir)/mkcheck
	$(top_builddir)/mkcheck 1 $(top_builddir) $(top_srcdir) $(prefix)

#all-local:  stamp-rebuild

# This has already been sanity-checked by configure to a certain extent
#stamp-rebuild:  $(top_builddir)/config.status
#	if test "$(GCC_OBJDIR)" != "no"; then \
#	(cd "$(GCC_OBJDIR)/gcc" && \
#	 if test -f libgcc2.ready ; then \
#	   rm -f libgcc2.ready libgcc.a libgcc2.a ; \
#	   $(MAKE) libgcc2.a TARGET_LIBGCC2_CFLAGS='@NAMESPACES@' ; \
#	   $(MAKE) libgcc.a ; \
#	 else \
#	   echo '** There does not appear to be a previously-built' ; \
#	   echo '** library here.  Please see' ; \
#	   echo '** <http://sourceware.cygnus.com/libstdc++/install.html>' ; \
#	   echo '** In particular, the "Building and Installing' ; \
#	   echo '** the library (without GCC sources)" section.' ; \
#	   exit 1 ; \
#	 fi \
#	) \
#	fi
#	touch stamp-rebuild

# Multilib support.
MAKEOVERRIDES=

# Multilib variables.
MULTISRCTOP =
MULTIBUILDTOP =
MULTIDIRS =
MULTISUBDIR =
MULTIDO = true
MULTICLEAN = true

# Multilib Makefile bits.
.PHONY: all-multi mostlyclean-multi clean-multi distclean-multi \
	maintainer-clean-multi

all-am: all-multi
install-am: install-multi
mostlyclean-am: mostlyclean-multi
clean-am: clean-multi
distclean-am: distclean-multi
maintainer-clean-am: maintainer-clean-multi

all-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
install-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=install multi-do
mostlyclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=mostlyclean multi-clean
clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=clean multi-clean
distclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=distclean multi-clean
maintainer-clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=maintainer-clean multi-clean

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
	"AR_FLAGS=$(AR_FLAGS)" \
	"CC_FOR_BUILD=$(CC_FOR_BUILD)" \
	"CC_FOR_TARGET=$(CC_FOR_TARGET)" \
	"CFLAGS=$(CFLAGS)" \
	"CXXFLAGS=$(CXXFLAGS)" \
	"CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
	"CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
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
	"EXPECT=$(EXPECT)" \
	"RUNTEST=$(RUNTEST)" \
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"includedir=$(includedir)" \
	"prefix=$(prefix)" \
	"tooldir=$(tooldir)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"NM=$(NM)" \
	"NM_FOR_BUILD=$(NM_FOR_BUILD)" \
	"NM_FOR_TARGET=$(NM_FOR_TARGET)" \
	"DESTDIR=$(DESTDIR)"







