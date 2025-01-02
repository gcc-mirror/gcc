## Makefile for the C++11 sources of the GNU C++ Standard library.
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

# Convenience library for C++11 runtime.
noinst_LTLIBRARIES = libc++11convenience.la

headers =

# Source files linked in via configuration/make substitution for a
# particular host.
host_sources = \
	ctype_configure_char.cc \
	ctype_members.cc

ctype_configure_char.cc: ${glibcxx_srcdir}/$(OS_INC_SRCDIR)/ctype_configure_char.cc
	$(LN_S) ${glibcxx_srcdir}/$(OS_INC_SRCDIR)/ctype_configure_char.cc . || true

ctype_members.cc: ${glibcxx_srcdir}/$(CCTYPE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(CCTYPE_CC) . || true

# Source files linked in via configuration/make substitution for a
# particular host, but with ad hoc naming rules.
host_sources_extra = \
	basic_file.cc

basic_file.cc: ${glibcxx_srcdir}/$(BASIC_FILE_CC)
	$(LN_S) ${glibcxx_srcdir}/$(BASIC_FILE_CC) ./$@ || true

if ENABLE_DUAL_ABI
cxx11_abi_sources = \
	cow-locale_init.cc \
	cow-shim_facets.cc \
	cxx11-hash_tr1.cc \
	cxx11-ios_failure.cc \
	cxx11-shim_facets.cc \
	cxx11-stdexcept.cc
else
cxx11_abi_sources =
endif

sources_freestanding = \
	limits.cc \
	placeholders.cc

sources = \
	assert_fail.cc \
	chrono.cc \
	codecvt.cc \
	condition_variable.cc \
	cow-stdexcept.cc \
	cow-system_error.cc \
	ctype.cc \
	debug.cc \
	functexcept.cc \
	functional.cc \
	futex.cc \
	future.cc \
	hash_c++0x.cc \
	hashtable_c++0x.cc \
	ios.cc \
	ios_errcat.cc \
	locale_init.cc \
	localename.cc \
	mutex.cc \
	random.cc \
	regex.cc  \
	shared_ptr.cc \
	snprintf_lite.cc \
	system_error.cc \
	thread.cc \
	${cxx11_abi_sources} \
	${host_sources} \
	${host_sources_extra}

if ENABLE_DUAL_ABI
extra_string_inst_sources = \
	cow-fstream-inst.cc \
	cow-sstream-inst.cc \
	cow-string-inst.cc \
	cow-string-io-inst.cc \
	cow-wstring-inst.cc \
	cow-wstring-io-inst.cc \
	cxx11-locale-inst.cc \
	cxx11-wlocale-inst.cc \
	sso_string.cc
else
extra_string_inst_sources =
endif

if ENABLE_EXTERN_TEMPLATE
# XTEMPLATE_FLAGS = -fno-implicit-templates
inst_sources = \
	$(extra_string_inst_sources) \
	ext11-inst.cc \
	fstream-inst.cc \
	ios-inst.cc \
	iostream-inst.cc \
	istream-inst.cc \
	locale-inst.cc \
	ostream-inst.cc \
	sstream-inst.cc \
	streambuf-inst.cc \
	string-inst.cc \
	string-io-inst.cc \
	wlocale-inst.cc \
	wstring-inst.cc \
	wstring-io-inst.cc
else
# XTEMPLATE_FLAGS =
inst_sources =
endif

vpath % $(top_srcdir)/src/c++11

if !GLIBCXX_HOSTED
libc__11convenience_la_SOURCES = $(sources_freestanding)
else
libc__11convenience_la_SOURCES = \
	$(sources_freestanding) \
	$(sources) \
	$(inst_sources)
endif


# Use special rules for the hashtable.cc file so that all
# the generated template functions are also instantiated.
hashtable_c++0x.lo: hashtable_c++0x.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c $<
hashtable_c++0x.o: hashtable_c++0x.cc
	$(CXXCOMPILE) -fimplicit-templates -c $<

# Use special rules for source files that require -fchar8_t.
codecvt.lo: codecvt.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
codecvt.o: codecvt.cc
	$(CXXCOMPILE) -fchar8_t -c $<
limits.lo: limits.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
limits.o: limits.cc
	$(CXXCOMPILE) -fchar8_t -c $<
locale_init.lo: locale_init.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
locale_init.o: locale_init.cc
	$(CXXCOMPILE) -fchar8_t -c $<
localename.lo: localename.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
localename.o: localename.cc
	$(CXXCOMPILE) -fchar8_t -c $<

if ENABLE_DUAL_ABI
# Rewrite the type info for __ios_failure.
rewrite_ios_failure_typeinfo = sed -e '/^_*_ZTISt13__ios_failure:/,/_ZTVN10__cxxabiv120__si_class_type_infoE/s/_ZTVN10__cxxabiv120__si_class_type_infoE/_ZTVSt19__iosfail_type_info/'

cxx11-ios_failure-lt.s: cxx11-ios_failure.cc
	$(LTCXXCOMPILE) -gno-as-loc-support -S $< -o tmp-cxx11-ios_failure-lt.s
	-test -f tmp-cxx11-ios_failure-lt.o && mv -f tmp-cxx11-ios_failure-lt.o tmp-cxx11-ios_failure-lt.s
	$(rewrite_ios_failure_typeinfo) tmp-$@ > $@
	-rm -f tmp-$@
cxx11-ios_failure.s: cxx11-ios_failure.cc
	$(CXXCOMPILE) -gno-as-loc-support -S $< -o tmp-$@
	$(rewrite_ios_failure_typeinfo) tmp-$@ > $@
	-rm -f tmp-$@

cxx11-ios_failure.lo: cxx11-ios_failure-lt.s
	$(LTCXXCOMPILE) -g0 -c $< -o $@
cxx11-ios_failure.o: cxx11-ios_failure.s
	$(CXXCOMPILE) -g0 -c $<
endif

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion calls for it.
AM_CXXFLAGS = \
	-std=gnu++11 \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(XTEMPLATE_FLAGS) $(VTV_CXXFLAGS) \
	$(WARN_CXXFLAGS) $(OPTIMIZE_CXXFLAGS) $(CONFIG_CXXFLAGS)

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
