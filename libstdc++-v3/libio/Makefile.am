## Makefile for the math subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1999 Cygnus Solutions
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

noinst_LTLIBRARIES = libio.la

if GLIBCPP_NEED_LIBIO_CONFIG_H
LIBIO_CONFIG_H = _G_config.h
else
LIBIO_CONFIG_H = 
endif

includes = 

libio_headers = \
        libio.h libioP.h iolibio.h

if GLIBCPP_NEED_LIBIO
LIBIO_SRCS = \
	filedoalloc.c genops.c fileops.c stdfiles.c cleanup.c
else
LIBIO_SRCS =
endif


EXTRA_DIST = iostreamP.h

libio_la_LIBADD = $(LIBIO_SRCS) 
libio_la_DEPENDENCIES = $(libio_la_LIBADD)
libio_la_SOURCES = $(LIBIO_SRCS)

# Specifying that *.o depend on this one header
$(libio_la_OBJECTS): $(LIBIO_CONFIG_H)

LINK = $(LIBTOOL) --mode=link "$(CCLD)" $(AM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

# Generate this file.
_G_config.h: $(srcdir)/gen-params
	rootme=`pwd`/ ; export rootme; \
	CC="$(CC) $(CINCLUDES)"; export CC; \
	CXX="$(CXX) $(CXXINCLUDES) $(NOSTDINC) $(CXXFLAGS)"; export CXX; \
	CONFIG_NM="$(NM)"; export CONFIG_NM; \
	$(SHELL) $(srcdir)/gen-params LIB_VERSION=$(VERSION) $(G_CONFIG_ARGS) >tmp-params.h
	mv tmp-params.h _G_config.h




