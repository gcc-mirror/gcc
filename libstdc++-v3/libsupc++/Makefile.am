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
	-I$(top_srcdir)/../gcc -I$(top_srcdir)/../include \
	$(CSHADOW_INCLUDES)  $(LIBSUPCXX_INCLUDES) -I$(GLIBCPP_INCLUDE_DIR) \
	$(CONFIG_INCLUDES) -I$(top_builddir)

headers = \
	cxxabi.h exception new new.h typeinfo

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
	pure.cc \
	tinfo.cc \
	tinfo2.cc \
	vec.cc

libsupc___la_SOURCES = $(sources)
libsupc__convenience_la_SOURCES = $(sources)

# This insanely simple bit of install magic courtesy of Mr. Tromey.
glibcppinstalldir= $(prefix)/include/g++-v3
glibcppinstall_HEADERS = $(headers)

# Flags to force separate libtool library to be static only.
LIBTOOL = @LIBTOOL@ --tag disable-shared
LIBSUPCXX_CXXFLAGS = -prefer-pic

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.
# Need to manually set this option because CONFIG_CXXFLAGS has to be at
# the end of the compile line so that -O2 can be overridden as the
# occasion call for it. (ie, --enable-debug)
AM_CXXFLAGS = \
	-fno-implicit-templates \
	$(LIBSUPCXX_CXXFLAGS) \
	$(WARN_CXXFLAGS) \
	$(OPTIMIZE_CXXFLAGS) \
	$(CONFIG_CXXFLAGS) 

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





