## Makefile for the libio subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

# Root level of the include sources.
glibcpp_includedir = $(top_srcdir)/include

# Passed down for cross compilers, canadian crosses.
TOPLEVEL_INCLUDES = -I$(includedir)

if GLIBCPP_NEED_LIBIO
LIBIO_INCLUDES = \
	-I$(top_builddir)/libio -I$(top_srcdir)/libio 
else
LIBIO_INCLUDES = 
endif

CONFIG_INCLUDES = \
	-I$(top_srcdir)/@cpu_include_dir@ \
	-I$(top_srcdir)/@ctype_include_dir@

INCLUDES = \
	-nostdinc++ \
	-I$(top_builddir) -I$(glibcpp_includedir) \
	$(CONFIG_INCLUDES) $(LIBIO_INCLUDES) $(TOPLEVEL_INCLUDES)	

noinst_LTLIBRARIES = libio.la

# These bits are all figured out from configure. Look in acinclude.m4
# or configure.in to see how they are set.
# NB: DEBUGFLAGS have to be at the end so that -O2 can be overridden.
AC_CFLAGS = \
	$(WERROR) @CPU_FLAGS@ @DEBUG_FLAGS@ 

# Need to manually set this option because AC_CXXFLAGS has to be at
# the end of the compile line so that -O2 can be overridden as the
# occasion call for it. (ie, --enable-debug)
AM_CFLAGS = \
	-D_GNU_SOURCE $(AC_CFLAGS) 

COMPILE = $(CC) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(CFLAGS) \
	$(AM_CFLAGS) 

LTCOMPILE = $(LIBTOOL) --mode=compile $(CC) $(DEFS) $(INCLUDES) \
	$(AM_CPPFLAGS) \$(CPPFLAGS)  $(CFLAGS) $(AM_CFLAGS)

LINK = $(LIBTOOL) --mode=link "$(CCLD)" $(CFLAGS) $(AM_CFLAGS) $(LDFLAGS) -o $@


libio_headers = \
        libio.h libioP.h iolibio.h

if GLIBCPP_NEED_LIBIO
LIBIO_SRCS = \
	filedoalloc.c genops.c fileops.c stdfiles.c c_codecvt.c
#	filedoalloc.c genops.c fileops.c stdfiles.c cleanup.c c_codecvt.c
else
LIBIO_SRCS =
endif

if GLIBCPP_NEED_WLIBIO
LIBIO_WSRCS = \
	wfiledoalloc.c wfileops.c wgenops.c
else
LIBIO_WSRCS =
endif

if GLIBCPP_NEED_XTRA_LIBIO
LIBIO_XTRASRCS = \
	iofwide.c iofclose.c iofopen.c stdio.c
else
LIBIO_XTRASRCS =
endif


EXTRA_DIST = iostreamP.h

libio_la_SOURCES = $(LIBIO_SRCS) $(LIBIO_WSRCS) $(LIBIO_XTRASRCS)







