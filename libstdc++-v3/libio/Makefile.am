## Makefile for the libio subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1999, 2000 Cygnus Solutions
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

# Passed down for cross compilers, canadian crosses.
TOPLEVEL_INCLUDES = -I$(includedir)

LIBIO_INCLUDES = @BUILD_LIBIO_INCLUDE@ -I$(top_srcdir)/libio 

CSHADOW_INCLUDES = @CSHADOWFLAGS@ @CSHADOW_INCLUDES@

CONFIG_INCLUDES = \
	-I$(top_srcdir)/@cpu_include_dir@ \
	-I$(top_srcdir)/@ctype_include_dir@

INCLUDES = \
	-D_GNU_SOURCE -nostdinc++ -I$(top_srcdir) \
	$(LIBIO_INCLUDES) $(CONFIG_INCLUDES) $(CSHADOW_INCLUDES) \
	$(TOPLEVEL_INCLUDES)	

noinst_LTLIBRARIES = libio.la

LINK = $(LIBTOOL) --mode=link "$(CCLD)" $(AM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

#includes = 

libio_headers = \
        libio.h libioP.h iolibio.h

if GLIBCPP_NEED_LIBIO
LIBIO_SRCS = \
	filedoalloc.c genops.c fileops.c stdfiles.c cleanup.c
else
LIBIO_SRCS =
endif

if GLIBCPP_NEED_WLIBIO
LIBIO_WSRCS = \
	iofwide.c wfiledoalloc.c wfileops.c wgenops.c
else
LIBIO_WSRCS =
endif


EXTRA_DIST = iostreamP.h

libio_la_LIBADD = $(LIBIO_SRCS) $(LIBIO_WSRCS) 
libio_la_DEPENDENCIES = $(libio_la_LIBADD)
libio_la_SOURCES = $(LIBIO_SRCS) $(LIBIO_WSRCS) 

