## Makefile for the GNU C++ Support library.
##
## Copyright (C) 2000 Free Software Foundation, Inc.
##
## Process this file with automake to produce Makefile.in.
##
## This file is part of GNU CC.
##
## GNU CC is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## GNU CC is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU CC; see the file COPYING.  If not, write to
## the Free Software Foundation, 59 Temple Place - Suite 330,
## Boston, MA 02111-1307, USA. 

AUTOMAKE_OPTIONS = 1.3 cygnus
MAINT_CHARSET = latin1

# Cross compiler and multilib support.
# Install a library built with a cross compiler in tooldir, not libdir.
if USE_LIBDIR
toolexeclibdir = $(libdir)$(MULTISUBDIR)
else
toolexecdir = $(exec_prefix)/$(target_alias)
toolexeclibdir = $(toolexecdir)/lib$(MULTISUBDIR)
endif


# Need this library to both be part of libstdc++.a, and installed
# separately too.
# 1) separate libsupc++.la 
toolexeclib_LTLIBRARIES = libsupc++.la
# 2) integrated libsupc++convenience.la that is to be a part of libstdc++.a
noinst_LTLIBRARIES = libsupc++convenience.la


INCLUDES = \
	-I$(top_srcdir)/../gcc -I$(top_srcdir)/libsupc++/include \
	$(CONFIG_INCLUDES)

headers = \
	include/cxxabi.h \
	include/exception \
	include/new \
	include/new.h \
	include/typeinfo

sources = \
	del_op.cc \
	del_opnt.cc \
	del_opv.cc \
	del_opvnt.cc \
	exception.cc \
	new_handler.cc \
	new_op.cc \
	new_opnt.cc \
	new_opv.cc \
	new_opvnt.cc \
	tinfo.cc \
	tinfo2.cc \
	vec.cc

libsupc___la_SOURCES = $(sources)
libsupc__convenience_la_SOURCES = $(sources)

# Flags to force separate libtool library to be static only.
LIBTOOL = @LIBTOOL@ --tag disable-shared
LIBSUPCXX_CXXFLAGS = -prefer-pic


# These bits are all figured out from configure. Look in acinclude.m4
# or configure.in to see how they are set.
# NB: DEBUGFLAGS have to be at the end so that -O2 can be overridden.
AC_CXXFLAGS = \
	$(WERROR) @WFMT_FLAGS@ @CPU_FLAGS@ @EXTRA_CXX_FLAGS@ \
	@SECTION_FLAGS@ @CSHADOWFLAGS@ @DEBUG_FLAGS@ 

# Need to manually set this option because AC_CXXFLAGS has to be at
# the end of the compile line so that -O2 can be overridden as the
# occasion call for it. (ie, --enable-debug)
AM_CXXFLAGS = \
	-fno-implicit-templates $(OPTIMIZE_CXXFLAGS) \
	-Wall -Wno-format -W -Wwrite-strings -Winline \
	$(LIBSUPCXX_CXXFLAGS) $(AC_CXXFLAGS) 

# Need to explicitly set this so that AM_CXXFLAGS is last. In
# addition, we don't use $(DEFS), which tries to set $(top_srcdir) as
# an include diretory, which screws up shadow headers. (That way,
# things like -O2 passed down from the toplevel can be overridden by
# --enable-debug.)
CXXCOMPILE = $(CXX) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
	     $(CXXFLAGS) $(AM_CXXFLAGS) 

LTCXXCOMPILE = $(LIBTOOL) --mode=compile $(CXX) $(INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(AM_CXXFLAGS) 

# We have a problem when building the shared libstdc++ object if the
# rules automake generates would be used.  We cannot allow CXX to be used
# in libtool since this would add -lstdc++ to the link line which of
# course is impossible.
CXXLINK = $(LIBTOOL) --mode=link "$(CC)" @OPT_LDFLAGS@ @SECTION_LDFLAGS@ $(AM_CXXFLAGS) $(LDFLAGS) -o $@
