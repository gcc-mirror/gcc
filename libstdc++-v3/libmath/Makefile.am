## Makefile for the math subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1997, 1998, 1999, 2000 Cygnus Solutions
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

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

noinst_LTLIBRARIES = libmath.la

EXTRA_LONG_DOUBLE_yes = \
	hypotl.c signbitl.c

EXTRA_DIST = \
	hypot.c hypotf.c \
	$(EXTRA_LONG_DOUBLE_yes) 

libmath_la_LIBADD = \
	@LIBMATHOBJS@ \
	$(EXTRA_LONG_DOUBLE_$(USE_COMPLEX_LONG_DOUBLE)) 

libmath_la_DEPENDENCIES = $(libmath_la_LIBADD)

libmath_la_SOURCES = signbit.c signbitf.c stubs.c

LINK = $(LIBTOOL) --mode=link "$(CCLD)" $(AM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

# Use common includes from acinclude.m4/GLIBCPP_EXPORT_INCLUDES
GLIBCPP_INCLUDE_DIR = @GLIBCPP_INCLUDE_DIR@
C_INCLUDE_DIR = @C_INCLUDE_DIR@
TOPLEVEL_INCLUDES = @TOPLEVEL_INCLUDES@
LIBMATH_INCLUDES = @LIBMATH_INCLUDES@
LIBSUPCXX_INCLUDES = @LIBSUPCXX_INCLUDES@
LIBIO_INCLUDES = @LIBIO_INCLUDES@
CSHADOW_INCLUDES = @CSHADOW_INCLUDES@

INCLUDES = \
	-I$(GLIBCPP_INCLUDE_DIR) -I$(top_builddir)/include  \
	$(TOPLEVEL_INCLUDES) 

