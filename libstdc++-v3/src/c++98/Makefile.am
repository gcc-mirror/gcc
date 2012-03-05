## Makefile for the C++03 sources of the GNU C++ Standard library.
##
## Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
## 2006, 2007, 2008, 2009, 2010, 2011, 2012
## Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/fragment.am

# Convenience library for C++98 runtime.
noinst_LTLIBRARIES = libc++98convenience.la

headers =

# Source files linked in via configuration/make substitution for a
# particular host.
host_sources = \
	atomicity.cc \
	codecvt_members.cc \
	collate_members.cc \
	ctype_configure_char.cc \
	ctype_members.cc \
	messages_members.cc \
	monetary_members.cc \
	numeric_members.cc \
	time_members.cc

codecvt_members.cc: ${glibcxx_srcdir}/$(CCODECVT_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CCODECVT_CC) . || true

collate_members.cc: ${glibcxx_srcdir}/$(CCOLLATE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CCOLLATE_CC) . || true

ctype_configure_char.cc: ${glibcxx_srcdir}/$(OS_INC_SRCDIR)/ctype_configure_char.cc
	$(LN_S) ${glibcxx_srcdir}/$(OS_INC_SRCDIR)/ctype_configure_char.cc . || true

ctype_members.cc: ${glibcxx_srcdir}/$(CCTYPE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CCTYPE_CC) . || true

messages_members.cc: ${glibcxx_srcdir}/$(CMESSAGES_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CMESSAGES_CC) . || true

monetary_members.cc: ${glibcxx_srcdir}/$(CMONEY_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CMONEY_CC) . || true

numeric_members.cc: ${glibcxx_srcdir}/$(CNUMERIC_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CNUMERIC_CC) . || true

time_members.cc: ${glibcxx_srcdir}/$(CTIME_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CTIME_CC) . || true

atomicity_file = ${glibcxx_srcdir}/$(ATOMICITY_SRCDIR)/atomicity.h
atomicity.cc: ${atomicity_file}
	$(LN_S) ${atomicity_file} ./atomicity.cc || true

# Source files linked in via configuration/make substitution for a
# particular host, but with ad hoc naming rules.
host_sources_extra = \
	basic_file.cc c++locale.cc \
	${inst_sources} ${ldbl_compat_sources} ${parallel_sources}

c++locale.cc: ${glibcxx_srcdir}/$(CLOCALE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CLOCALE_CC) ./$@ || true

basic_file.cc: ${glibcxx_srcdir}/$(BASIC_FILE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(BASIC_FILE_CC) ./$@ || true

if ENABLE_PARALLEL
parallel_sources = parallel_settings.cc \
		   compatibility-parallel_list.cc \
		   compatibility-parallel_list-2.cc
else
parallel_sources =
endif

if GLIBCXX_LDBL_COMPAT
ldbl_compat_sources = compatibility-ldbl.cc
else
ldbl_compat_sources =
endif

if ENABLE_EXTERN_TEMPLATE
XTEMPLATE_FLAGS = -fno-implicit-templates
inst_sources = \
	allocator-inst.cc \
	concept-inst.cc \
	ext-inst.cc \
	ios-inst.cc \
	iostream-inst.cc \
	istream-inst.cc \
	locale-inst.cc \
	misc-inst.cc \
	ostream-inst.cc \
	sstream-inst.cc \
	streambuf-inst.cc \
	wlocale-inst.cc
else
XTEMPLATE_FLAGS =
inst_sources =
endif

# Sources present in the src directory, always present.
sources = \
	bitmap_allocator.cc \
	pool_allocator.cc \
	mt_allocator.cc \
	codecvt.cc \
	compatibility.cc \
	compatibility-debug_list.cc \
	compatibility-debug_list-2.cc \
	compatibility-list.cc \
	compatibility-list-2.cc \
	complex_io.cc \
	ctype.cc \
	globals_io.cc \
	hash_tr1.cc \
	hashtable_tr1.cc \
	ios.cc \
	ios_failure.cc \
	ios_init.cc \
	ios_locale.cc \
	list.cc \
	locale.cc \
	locale_init.cc \
	locale_facets.cc \
	localename.cc \
	math_stubs_float.cc \
	math_stubs_long_double.cc \
	stdexcept.cc \
	strstream.cc \
	tree.cc \
	istream.cc \
	streambuf.cc \
	valarray.cc \
	${host_sources} \
	${host_sources_extra}

vpath % $(top_srcdir)/src/c++98
vpath % $(top_srcdir)

libc__98convenience_la_SOURCES = $(sources)

# Use special rules for the deprecated source files so that they find
# deprecated include files.
GLIBCXX_INCLUDE_DIR=$(glibcxx_builddir)/include
strstream.lo: strstream.cc
	$(LTCXXCOMPILE) -I$(GLIBCXX_INCLUDE_DIR)/backward -Wno-deprecated -c $<
strstream.o: strstream.cc
	$(CXXCOMPILE) -I$(GLIBCXX_INCLUDE_DIR)/backward -Wno-deprecated -c $<

# Use special rules for the concept-checking instantiations so that all
# the generated template functions are also instantiated.  Force the checks
# to be on so that the instantiations are actually seen.
concept-inst.lo: concept-inst.cc
	$(LTCXXCOMPILE) -D_GLIBCXX_CONCEPT_CHECKS -fimplicit-templates -c $<
concept-inst.o: concept-inst.cc
	$(CXXCOMPILE) -D_GLIBCXX_CONCEPT_CHECKS -fimplicit-templates -c $<

# Use special rules for parallel mode compilation.
PARALLEL_FLAGS = -fopenmp -D_GLIBCXX_PARALLEL -I$(glibcxx_builddir)/../libgomp
parallel_settings.lo: parallel_settings.cc
	$(LTCXXCOMPILE) $(PARALLEL_FLAGS) -c $<
parallel_settings.o: parallel_settings.cc
	$(CXXCOMPILE) $(PARALLEL_FLAGS) -c $<

compatibility-parallel_list.lo: compatibility-parallel_list.cc
	$(LTCXXCOMPILE) -c $<
compatibility-parallel_list.o: compatibility-parallel_list.cc
	$(CXXCOMPILE) -c $<

compatibility-parallel_list-2.lo: compatibility-parallel_list-2.cc
	$(LTCXXCOMPILE) -c $<
compatibility-parallel_list-2.o: compatibility-parallel_list-2.cc
	$(CXXCOMPILE) -c $<

if GLIBCXX_LDBL_COMPAT
# Use special rules for compatibility-ldbl.cc compilation, as we need to
# pass -mlong-double-64.
compatibility-ldbl.lo: compatibility-ldbl.cc
	$(LTCXXCOMPILE) -mlong-double-64 -c $<
compatibility-ldbl.o: compatibility-ldbl.cc
	$(CXXCOMPILE) -mlong-double-64 -c $<
endif

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion calls for it.
AM_CXXFLAGS = \
	$(PIC_CXXFLAGS) \
	$(XTEMPLATE_FLAGS) \
	$(WARN_CXXFLAGS) \
	$(OPTIMIZE_CXXFLAGS) \
	$(CONFIG_CXXFLAGS)

AM_MAKEFLAGS = \
	"gxx_include_dir=$(gxx_include_dir)"

# Libtool notes

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
# attempt to infer which configuration to use.
# We have to put --tag disable-shared after --tag CXX lest things
# CXX undo the affect of disable-shared.
LTCXXCOMPILE = $(LIBTOOL) --tag CXX --tag disable-shared $(LIBTOOLFLAGS) \
	       --mode=compile $(CXX) $(TOPLEVEL_INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = $(LIBTOOL) --tag CXX --tag disable-shared $(LIBTOOLFLAGS) \
	  --mode=link $(CXX) \
	  $(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CXXFLAGS) $(LTLDFLAGS) -o $@
