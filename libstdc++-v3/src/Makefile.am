## Makefile for the src subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002
## Free Software Foundation, Inc.
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
MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

# Cross compiler support.
CXX = @glibcpp_CXX@
toolexecdir = @glibcpp_toolexecdir@
toolexeclibdir = @glibcpp_toolexeclibdir@
toolexeclib_LTLIBRARIES = libstdc++.la

# Compile flags that should be constant throughout the build, both for
# SUBDIRS and for libstdc++-v3 in general.
OPTIMIZE_CXXFLAGS = @OPTIMIZE_CXXFLAGS@

# These bits are all figured out from configure. Look in acinclude.m4
# or configure.in to see how they are set. See GLIBCPP_EXPORT_FLAGS
# NB: DEBUGFLAGS have to be at the end so that -O2 can be overridden.
CONFIG_CXXFLAGS = \
	@EXTRA_CXX_FLAGS@ @SECTION_FLAGS@ @DEBUG_FLAGS@ 

# Warning flags to use.
WARN_CXXFLAGS = \
	@WARN_FLAGS@ $(WERROR) -fdiagnostics-show-location=once

# Use common includes from acinclude.m4/GLIBCPP_EXPORT_INCLUDES
GLIBCPP_INCLUDES = @GLIBCPP_INCLUDES@
LIBMATH_INCLUDES = @LIBMATH_INCLUDES@
LIBSUPCXX_INCLUDES = @LIBSUPCXX_INCLUDES@
LIBIO_INCLUDES = @LIBIO_INCLUDES@
TOPLEVEL_INCLUDES = @TOPLEVEL_INCLUDES@

INCLUDES = \
	-nostdinc++ \
	$(GLIBCPP_INCLUDES) \
	$(LIBSUPCXX_INCLUDES) $(LIBIO_INCLUDES) $(LIBMATH_INCLUDES) \
	$(TOPLEVEL_INCLUDES)

# Source files linked in via configuration magic for a particular target.
target_sources = \
	codecvt_members.cc \
	collate_members.cc \
	ctype_members.cc \
	messages_members.cc \
	monetary_members.cc \
	numeric_members.cc \
	time_members.cc

# Source files linked in via configuration magic for a particular
# target, but with ad hoc naming rules.
target_sources_extra = \
	basic_file.cc \
	c++locale.cc 

# Sources present in the src directory.
sources = \
	bitset.cc \
	codecvt.cc \
	complex_io.cc \
	concept-inst.cc \
	ctype.cc \
	ext-inst.cc \
	fstream.cc \
	fstream-inst.cc \
	functexcept.cc \
	globals.cc \
	io-inst.cc \
	ios.cc \
	istream-inst.cc \
	limits.cc \
	locale.cc \
	locale-inst.cc \
	localename.cc \
	misc-inst.cc \
	ostream-inst.cc \
	sstream-inst.cc \
	stdexcept.cc \
	stl-inst.cc \
	streambuf-inst.cc \
	string-inst.cc \
	strstream.cc \
	valarray-inst.cc \
	vterminate.cc \
	wstring-inst.cc \
	${target_sources} \
	${target_sources_extra}

VPATH = $(top_srcdir)/src:$(top_srcdir)

libstdc___la_SOURCES = $(sources)

libstdc___la_LIBADD = \
	../libmath/libmath.la @libio_la@ \
	../libsupc++/libsupc++convenience.la

if GLIBCPP_BUILD_VERSIONED_SHLIB
version_arg=-Wl,--version-script=linker.map
else
version_arg=
endif

libstdc___la_LDFLAGS = \
	-version-info @libtool_VERSION@ ${version_arg} \
	-lm @LIBUNWIND_FLAG@ 

libstdc___la_DEPENDENCIES = $(libstdc___la_LIBADD) linker.map

# Use special rules for the deprecated source files so that they find
# deprecated include files.
GLIBCPP_INCLUDE_DIR=@glibcpp_builddir@/include
strstream.lo: strstream.cc
	$(LTCXXCOMPILE) -I$(GLIBCPP_INCLUDE_DIR)/backward -Wno-deprecated -c $<
strstream.o: strstream.cc
	$(CXXCOMPILE) -I$(GLIBCPP_INCLUDE_DIR)/backward -Wno-deprecated -c $<

# Use special rules for the concept-checking instantiations so that all
# the generated template functions are also instantiated.  Force the checks
# to be on so that the instantiations are actually seen.
concept-inst.lo: concept-inst.cc
	$(LTCXXCOMPILE) -D_GLIBCPP_CONCEPT_CHECKS -fimplicit-templates -c $<
concept-inst.o: concept-inst.cc
	$(CXXCOMPILE) -D_GLIBCPP_CONCEPT_CHECKS -fimplicit-templates -c $<


# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion call for it. (ie, --enable-debug)
AM_CXXFLAGS = \
	-fno-implicit-templates \
	$(LIBSUPCXX_CXXFLAGS) \
	$(WARN_CXXFLAGS) \
	$(OPTIMIZE_CXXFLAGS) \
	$(CONFIG_CXXFLAGS) 


# libstdc++ libtool notes

# 1) Need to explicitly set LTCXXCOMPILE so that AM_CXXFLAGS is
# last. (That way, things like -O2 passed down from the toplevel can
# be overridden by --enable-debug.)

# 2) In general, libtool expects an argument such as `--tag=CXX' when
# using the C++ compiler, because that will enable the settings
# detected when C++ support was being configured.  However, when no
# such flag is given in the command line, libtool attempts to figure
# it out by matching the compiler name in each configuration section
# against a prefix of the command line.  The problem is that, if the
# compiler name and its initial flags stored in the libtool
# configuration file don't match those in the command line, libtool
# can't decide which configuration to use, and it gives up.  The
# correct solution is to add `--tag CXX' to LTCXXCOMPILE and maybe
# CXXLINK, just after $(LIBTOOL), so that libtool doesn't have to
# attempt to infer which configuration to use
LTCXXCOMPILE = $(LIBTOOL) --tag CXX --mode=compile $(CXX) $(INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(AM_CXXFLAGS) 

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = $(LIBTOOL) --tag CXX --mode=link $(CXX) \
	  @OPT_LDFLAGS@ @SECTION_LDFLAGS@ $(AM_CXXFLAGS) $(LDFLAGS) -o $@
