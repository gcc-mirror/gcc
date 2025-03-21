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

if BUILD_LIBGCOBOL
toolexeclib_LTLIBRARIES  = libgcobol.la
endif

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

libgcobol_la_LINK = $(LIBTOOL) --mode=link --tag=CXX $(CXX)	\
		-o libgcobol$(libsuffix).la			\
		-Wc,-shared-libgcc				\
		-version-info $(LIBGCOBOL_VERSION)		\
		-lstdc++					\
		$(LTLDFLAGS) $(LTLIBICONV)

WARN_CFLAGS = -W -Wall -Wwrite-strings

AM_CXXFLAGS = $(CXXFLAGS_FOR_TARGET)

# not defined: DEFS, MAX_ERRORS, LTLDFLAGS
ALL_CXXFLAGS = -I. -I$(srcdir) $(AM_CPPFLAGS) $(DEFS)		\
	$(XCFLAGS) $(AM_CXXFLAGS) $(WARN_CFLAGS) $(MAX_ERRORS)	\
	-DIN_GCC -DIN_TARGET_LIBS -fno-strict-aliasing

%.lo: %.cc
	$(LIBTOOL) --mode=compile --tag=CXX $(CXX) -c	\
		-o $@ $(INCLUDES) $(ALL_CXXFLAGS) $<
