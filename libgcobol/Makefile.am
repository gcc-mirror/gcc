# Copyright (C) 2025 Free Software Foundation, Inc.
# Contributed by the Symas Corporation, 2025

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Written de novo for libgcobol.

AUTOMAKE_OPTIONS = 1.8 foreign
ACLOCAL_AMFLAGS = -I .. -I ../config

# May be used by various substitution variables.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

# Skip the whole process if we are not building libgcobol.
if BUILD_LIBGCOBOL
toolexeclib_LTLIBRARIES  = libgcobol.la
toolexeclib_DATA = libgcobol.spec

##
## 2.2.12 Automatic Dependency Tracking
## Automake generates code for automatic dependency tracking by default
##
libgcobol_la_SOURCES =				\
	charmaps.cc				\
	constants.cc				\
	gfileio.cc				\
	gmath.cc				\
	intrinsic.cc				\
	io.cc					\
	libgcobol.cc				\
	valconv.cc

WARN_CFLAGS = -W -Wall -Wwrite-strings

AM_CPPFLAGS = -I. -I$(srcdir) $(LIBQUADINCLUDE)
AM_CFLAGS = $(XCFLAGS)
AM_CXXFLAGS = $(XCFLAGS)
AM_CXXFLAGS += $(WARN_CFLAGS)
AM_CXXFLAGS += -DIN_TARGET_LIBS
AM_CXXFLAGS += -fno-strict-aliasing

if ENABLE_DARWIN_AT_RPATH
# Handle embedded rpaths for Darwin.
extra_ldflags_libgcobol += -Wc,-nodefaultrpaths
extra_ldflags_libgcobol += -Wl,-rpath,@loader_path
endif

# We want to link with the c++ runtime.
libgcobol_la_LINK = $(CXXLINK) $(libgcobol_la_LDFLAGS)
version_arg = -version-info $(LIBGCOBOL_VERSION)
libgcobol_la_LDFLAGS = $(LTLDFLAGS) $(LIBQUADLIB) $(LTLIBICONV) \
	$(extra_ldflags_libgcobol) $(LIBS) $(version_arg)
libgcobol_la_DEPENDENCIES = libgcobol.spec $(LIBQUADLIB_DEP)

endif BUILD_LIBGCOBOL
