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

noinst_LTLIBRARIES = libmath.la

EXTRA_LONG_DOUBLE_yes = \
	signbitl.c cabsl.c 

EXTRA_DIST = \
	ccos.c ccosf.c ccosh.c ccoshf.c ccoshl.c ccosl.c \
	cexp.c cexpf.c cexpl.c \
	c_log.c c_logf.c c_logl.c clog10.c clog10f.c clog10l.c \
	cpow.c cpowf.c cpowl.c csin.c csinf.c csinh.c csinhf.c csinhl.c  \
	csinl.c csqrt.c csqrtf.c csqrtl.c ctan.c ctanf.c ctanh.c \
	ctanhf.c ctanhl.c ctanl.c carg.c cargf.c cargl.c hypot.c hypotf.c\
	hypotl.c atan2f.c expf.c \
	$(EXTRA_LONG_DOUBLE_yes) 

libmath_la_LIBADD = \
	@LIBMATHOBJS@ \
	$(EXTRA_LONG_DOUBLE_$(USE_LONG_DOUBLE)) 

libmath_la_DEPENDENCIES = $(libmath_la_LIBADD)

libmath_la_SOURCES = \
	signbit.c signbitf.c cabs.c cabsf.c 

LINK = $(LIBTOOL) --mode=link "$(CCLD)" $(AM_CFLAGS) $(CFLAGS) $(LDFLAGS) -o $@

TOPLEVEL_INCLUDES = -I$(includedir)
INCLUDES = -I..  $(TOPLEVEL_INCLUDES)



