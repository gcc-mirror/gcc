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

SUBDIRS = libio libmath libsupc++ src


# Compile flags that should be constant throughout the build, both for
# SUBDIRS and for libstdc++-v3 in general.
# Optimization flags that are probably a good idea for thrill-seekers. Just
# uncomment the lines below and make, everything else is ready to go... 
# OPTIMIZE_CXXFLAGS = -O3 -fstrict-aliasing -fvtable-gc 
OPTIMIZE_CXXFLAGS = 

# Warning flags to use.
WARN_CXXFLAGS = \
	-Wall -Wno-format -W -Wwrite-strings -Winline \
	$(WERROR) @WFMT_FLAGS@

# These bits are all figured out from configure. Look in acinclude.m4
# or configure.in to see how they are set.
# NB: DEBUGFLAGS have to be at the end so that -O2 can be overridden.
CONFIG_CXXFLAGS = \
	@EXTRA_CXX_FLAGS@ \
	@SECTION_FLAGS@ @CSHADOWFLAGS@ @DEBUG_FLAGS@ 


# Root level of the include sources.
GLIBCPP_INCLUDE_DIR = $(top_srcdir)/include

# Can either use include/c or include/c_std to grab "C" headers. This
# variable is set to the include directory currently in use.
C_INCLUDE_DIR = $(GLIBCPP_INCLUDE_DIR)/@c_include_dir@


# Passed down for cross compilers, canadian crosses.
TOPLEVEL_INCLUDES = -I$(includedir)

LIBMATH_INCLUDES = -I$(top_srcdir)/libmath

if GLIBCPP_NEED_LIBIO
LIBIO_INCLUDES = \
	-I$(top_builddir)/libio -I$(top_srcdir)/libio 
else
LIBIO_INCLUDES = -I$(top_srcdir)/libio
endif

LIBSUPCXX_INCLUDES = -I$(top_srcdir)/libsupc++

# XXX May need to add this in later to enabled shadow header includes
#	-I$(top_builddir)/cshadow
CSHADOW_INCLUDES = \
	-I$(GLIBCPP_INCLUDE_DIR)/std -I$(C_INCLUDE_DIR)


# Use $(blddir) instead of $(top_builddir) for arguments to "mkcheck"
# because then the paths will be full pathnames, not relative
# pathnames. (-Wl,--rpath seems to like this better.)
check:  $(top_builddir)/mkcheck
	chmod +x  $(top_builddir)/mkcheck
	$(top_builddir)/mkcheck 0 `pwd` $(top_srcdir)

check-install: $(top_builddir)/mkcheck
	chmod +x  $(top_builddir)/mkcheck
	$(top_builddir)/mkcheck 1 `pwd` $(top_srcdir) $(prefix)

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

# All the machinations with string instantiations messes up the
# automake-generated TAGS rule. Make a simple one here.
TAGS: tags-recursive $(LISP)

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
	"DESTDIR=$(DESTDIR)" \
	"WERROR=$(WERROR)" \
	"OPTIMIZE_CXXFLAGS=$(OPTIMIZE_CXXFLAGS)" \
	"WARN_CXXFLAGS=$(WARN_CXXFLAGS)" \
	"CONFIG_CXXFLAGS=$(CONFIG_CXXFLAGS)" \
	"GLIBCPP_INCLUDE_DIR=$(GLIBCPP_INCLUDE_DIR)" \
	"C_INCLUDE_DIR=$(C_INCLUDE_DIR)" \
	"TOPLEVEL_INCLUDES=$(TOPLEVEL_INCLUDES)" \
	"LIBMATH_INCLUDES=$(LIBMATH_INCLUDES)" \
	"LIBIO_INCLUDES=$(LIBIO_INCLUDES)" \
	"CSHADOW_INCLUDES=$(CSHADOW_INCLUDES)" \
	"LIBSUPCXX_INCLUDES=$(LIBSUPCXX_INCLUDES)"
