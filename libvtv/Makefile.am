## Makefile for the VTV library.
##
## Copyright (C) 2013 Free Software Foundation, Inc.
##
## Process this file with automake to produce Makefile.in.
##
## This file is part of the Vtable Verification (VTV) Library.  This
## library is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

if ENABLE_VTABLE_VERIFY
SUBDIRS = testsuite
else
SUBDIRS = 
endif

ACLOCAL_AMFLAGS = -I .. -I ../config

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = 
AM_CPPFLAGS = -I$(top_srcdir)/../include
AM_CFLAGS = $(XCFLAGS)
AM_CCASFLAGS = $(XCFLAGS)
AM_CXXFLAGS = $(XCFLAGS)  
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
AM_CXXFLAGS += -Wl,-u_vtable_map_vars_start,-u_vtable_map_vars_end

toolexeclib_LTLIBRARIES = libvtv.la

vtv_headers = \
	vtv_map.h \
	vtv_malloc.h \
	vtv_fail.h \
	vtv_set.h \
	vtv_utils.h \
	vtv_rts.h

vtv_sources = \
	vtv_start.c \
        vtv_malloc.cc \
        vtv_rts.cc \
        vtv_utils.cc \
	vtv_end.c

libvtv_includedir = $(includedir)

# Link in vtv_start and vtv_end. 
BUILT_SOURCES = vtv_start.c vtv_end.c
vtv_start.c:
	rm -f $@
	$(LN_S) $(toplevel_srcdir)/libgcc/vtv_start.c $@

vtv_end.c:
	rm -f $@
	$(LN_S) $(toplevel_srcdir)/libgcc/vtv_end.c $@

if ENABLE_VTABLE_VERIFY
libvtv_la_SOURCES = $(vtv_sources)
libvtv_include_HEADERS = $(vtv_headers)
else
libvtv_la_SOURCES = 
libvtv_include_HEADERS = 
endif

# Least ordering for dependencies mean linking w/o libstdc++ for as
# long as the development of libvtv does not absolutely require it.
CXXVTV=$(CC_FOR_TARGET)
LTCXXCOMPILE = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(CXXVTV) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)

CXXLINK = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(CXXVTV) $(AM_CXXFLAGS) $(CXXFLAGS) $(AM_LDFLAGS) \
	$(LDFLAGS) -o $@
