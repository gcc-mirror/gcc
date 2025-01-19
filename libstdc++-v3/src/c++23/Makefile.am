## Makefile for the C++23 sources of the GNU C++ Standard library.
##
## Copyright (C) 1997-2025 Free Software Foundation, Inc.
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

# Convenience library for C++23 runtime.
noinst_LTLIBRARIES = libc++23convenience.la

# Module std support.  Not compiled for now, only installed.
# Let's install the interface units in the bits subdirectory.
toolexeclib_DATA = libstdc++.modules.json
includebitsdir = $(gxx_include_dir)/bits
includebits_DATA = std.cc std.compat.cc

# The manifest to be installed uses the relative path between install dirs.
libstdc++.modules.json: libstdc++.modules.json.in
	relpath=$$($(toplevel_srcdir)/contrib/relpath.sh \
		   $(abspath $(toolexeclibdir)) \
		   $(abspath $(includebitsdir))) && \
	sed "s,@MODPATH@,$$relpath," $< > $@

# The uninstalled manifest uses the relative path to builddir.
stamp-module-manifest: libstdc++.modules.json.in
	@-mkdir -p ../.libs
	sed "s,@MODPATH@,../c++23," $< \
	  > ../.libs/libstdc++.modules.json
	@$(STAMP) $@

# Add the C library exports to both modules.
std.cc: std.cc.in std-clib.cc.in
	cat $^ > $@
std.compat.cc: std.compat.cc.in std-clib.cc.in
	cat $^ > $@

# Also put the interface units in the build-includes bits directory.
stamp-modules-bits: $(includebits_DATA)
	@-mkdir -p $(top_builddir)/include/bits
	-cd $(top_builddir)/include/bits && $(LN_S) $(abspath $?) . 2>/dev/null
	@$(STAMP) $@

all-local: stamp-module-manifest stamp-modules-bits

headers =

if ENABLE_EXTERN_TEMPLATE
# XTEMPLATE_FLAGS = -fno-implicit-templates
inst_sources =
else
# XTEMPLATE_FLAGS =
inst_sources =
endif

sources = stacktrace.cc print.cc

vpath % $(top_srcdir)/src/c++23


if GLIBCXX_HOSTED
libc__23convenience_la_SOURCES = $(sources)  $(inst_sources)
else
libc__23convenience_la_SOURCES =
endif

# Use C++26 so that std::filebuf::native_handle() is available.
print.lo: print.cc
	$(LTCXXCOMPILE) -std=gnu++26 -c $<
print.o: print.cc
	$(CXXCOMPILE) -std=gnu++26 -c $<

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion calls for it.
AM_CXXFLAGS = \
	-std=gnu++23 \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(XTEMPLATE_FLAGS) $(VTV_CXXFLAGS) \
	$(WARN_CXXFLAGS) $(OPTIMIZE_CXXFLAGS) $(CONFIG_CXXFLAGS) \
	-fimplicit-templates

AM_MAKEFLAGS = \
	"gxx_include_dir=$(gxx_include_dir)"

# Libtool notes

# 1) In general, libtool expects an argument such as `--tag=CXX' when
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
#
# The second tag argument, `--tag disable-shared` means that libtool
# only compiles each source once, for static objects. In actuality,
# glibcxx_lt_pic_flag and glibcxx_compiler_shared_flag are added to
# the libtool command that is used create the object, which is
# suitable for shared libraries.  The `--tag disable-shared` must be
# placed after --tag CXX lest things CXX undo the affect of
# disable-shared.

# 2) Need to explicitly set LTCXXCOMPILE so that EXTRA_CXX_FLAGS is
# last. (That way, things like -O2 passed down from the toplevel can
# be overridden by --enable-debug.)
LTCXXCOMPILE = \
	$(LIBTOOL) --tag CXX --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(CXX) $(TOPLEVEL_INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) $(EXTRA_CXX_FLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = \
	$(LIBTOOL) --tag CXX --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(CXX) \
	$(VTV_CXXLINKFLAGS) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CXXFLAGS) $(LTLDFLAGS) -o $@
