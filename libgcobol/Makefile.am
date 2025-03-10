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

toolexeclib_LTLIBRARIES  = libgcobol.la

libgcobol.la:   $(libgcobol_la_OBJECTS)			\
		$(libgcobol_la_DEPENDENCIES)		\
		$(EXTRA_libgcobol_la_DEPENDENCIES)
	$(AM_V_GEN)$(libgcobol_la_LINK)		\
		-rpath $(libdir)/../lib64	\
		$(libgcobol_la_OBJECTS)		\
		$(libgcobol_la_LIBADD) $(LIBS)


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

#
# configure varables
#

# Automatic
AM_CFLAGS = @CFLAGS@
configure_input = @configure_input@
AM_CPPFLAGS = @CPPFLAGS@
AM_CXXFLAGS = @CXXFLAGS@
DEFS = @DEFS@
ECHO_C = @ECHO_C@
ECHO_N = @ECHO_N@
ECHO_T = @ECHO_T@
ERLCFLAGS = @ERLCFLAGS@
FCFLAGS = @FCFLAGS@
FFLAGS = @FFLAGS@
AM_LDFLAGS = @LDFLAGS@
LIBS = @LIBS@
OBJCFLAGS = @OBJCFLAGS@
OBJCXXFLAGS = @OBJCXXFLAGS@
GOFLAGS = @GOFLAGS@
builddir = @builddir@
abs_builddir = @abs_builddir@
top_builddir = @top_builddir@
top_build_prefix = @top_build_prefix@
abs_top_builddir = @abs_top_builddir@
## srcdir see: overrides
abs_srcdir = @abs_srcdir@
top_srcdir = @top_srcdir@
abs_top_srcdir = @abs_top_srcdir@

# Installation
bindir = @bindir@
datadir = @datadir@
datarootdir = @datarootdir@
docdir = @docdir@
dvidir = @dvidir@
exec_prefix = @exec_prefix@
htmldir = @htmldir@
includedir = @includedir@
infodir = @infodir@
libdir = @libdir@
libexecdir = @libexecdir@
localedir = @localedir@
localstatedir = @localstatedir@
mandir = @mandir@
oldincludedir = @oldincludedir@
pdfdir = @pdfdir@
prefix = @prefix@
psdir = @psdir@
sbindir = @sbindir@
sharedstatedir = @sharedstatedir@
sysconfdir = @sysconfdir@

# Overrides and custom 

CC = @CC@
CXX = @CXX@
AR = @AR@
AS = @AS@
RANLIB = @RANLIB@
LIBGCOBOL_VERSION = @LIBGCOBOL_VERSION@
VERSION_SUFFIX = @VERSION_SUFFIX@
LIBTOOL = @LIBTOOL@ $(LIBTOOLFLAGS)

libgcobol_la_LFLAGS = -lstdc++
libgcobol_la_LINK = $(LIBTOOL) --mode=link --tag=CXX $(CXX)	\
		-o libgcobol$(libsuffix).la			\
		-Wc,-shared-libgcc				\
		-version-info $(LIBGCOBOL_VERSION)		\
		-lstdc++					\
		$(LTLDFLAGS)

# The 'all' rule must be the first one so that it is executed if
# nothing is specified on the command-line.
all: $(LIBGCOBOL_LA)

.PHONY: install install-html install-pdf install-info

###include $(top_srcdir)/../multilib.am
install: libgcobol$(libsuffix).la
	$(LIBTOOL) --mode=install $(INSTALL) $^ $(DESTDIR)$(libdir)/../lib64

WARN_CFLAGS = -W -Wall -Wwrite-strings

# not defined: DEFS, MAX_ERRORS, LTLDFLAGS
ALL_CFLAGS = -I. -I$(srcdir) $(AM_CPPFLAGS) $(DEFS)		\
	$(XCFLAGS) $(AM_CXXFLAGS) $(WARN_CFLAGS) $(MAX_ERRORS)	\
	-DIN_GCC -DIN_TARGET_LIBS -fno-strict-aliasing

%.lo: %.c
	$(LIBTOOL) --mode=compile  $(CC) -c		\
		-o $@ $(ALL_CFLAGS) $(INCLUDES) $<

%.lo: %.cc
	$(LIBTOOL) --mode=compile --tag=CXX $(CXX) -c	\
		-o $@ $(INCLUDES) $(ALL_CFLAGS) $<

doc: info dvi pdf html

# No install-html or install-pdf support
install-html install-pdf install-info:

