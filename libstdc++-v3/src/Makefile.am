# Makefile for the src subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.
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

## $Id: Makefile.am,v 1.71.2.7 2001/05/30 07:14:43 hp Exp $

AUTOMAKE_OPTIONS = 1.3 gnits
MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

# Cross compiler and multilib support.
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
	@EXTRA_CXX_FLAGS@ @SECTION_FLAGS@ @CSHADOW_FLAGS@ @DEBUG_FLAGS@ 

# Warning flags to use.
WARN_CXXFLAGS = \
	@WARN_FLAGS@ $(WERROR) -fdiagnostics-show-location=once

# Use common includes from acinclude.m4/GLIBCPP_EXPORT_INCLUDES
GLIBCPP_INCLUDE_DIR = @GLIBCPP_INCLUDE_DIR@
C_INCLUDE_DIR = @C_INCLUDE_DIR@
CSTD_INCLUDES = @CSTD_INCLUDES@
LIBMATH_INCLUDES = @LIBMATH_INCLUDES@
LIBSUPCXX_INCLUDES = @LIBSUPCXX_INCLUDES@
LIBIO_INCLUDES = @LIBIO_INCLUDES@
TOPLEVEL_INCLUDES = @TOPLEVEL_INCLUDES@

INCLUDES = \
	-nostdinc++ \
	-I$(GLIBCPP_INCLUDE_DIR) $(CSTD_INCLUDES) -I$(top_builddir)/include \
	$(LIBSUPCXX_INCLUDES) $(LIBIO_INCLUDES) $(LIBMATH_INCLUDES) \
	$(TOPLEVEL_INCLUDES)

base_headers = \
	bits/cpp_type_traits.h bits/char_traits.h bits/codecvt.h \
	bits/stringfwd.h bits/std_string.h bits/basic_string.h \
	bits/basic_string.tcc \
	bits/generic_shadow.h bits/std_utility.h \
	bits/std_complex.h \
	bits/valarray_array.h bits/valarray_array.tcc bits/valarray_meta.h \
	bits/std_valarray.h bits/mask_array.h bits/slice.h bits/slice_array.h \
	bits/gslice.h bits/gslice_array.h bits/indirect_array.h \
	bits/std_fstream.h bits/std_iomanip.h \
	bits/ios_base.h bits/fpos.h bits/basic_ios.h bits/basic_ios.tcc \
	bits/std_ios.h bits/std_iosfwd.h bits/std_iostream.h \
	bits/std_istream.h bits/istream.tcc bits/std_locale.h \
	bits/fstream.tcc bits/ostream.tcc bits/sbuf_iter.h bits/sstream.tcc \
	bits/std_ostream.h bits/std_sstream.h bits/std_streambuf.h \
	bits/streambuf.tcc bits/basic_file.h \
	bits/locale_facets.h bits/locale_facets.tcc bits/localefwd.h \
	bits/stl_pthread_alloc.h bits/pthread_allocimpl.h \
	bits/stl_threads.h \
	bits/stl_iterator_base_types.h bits/stl_iterator_base_funcs.h \
	bits/std_bitset.h bits/std_deque.h bits/std_functional.h \
	bits/std_iterator.h bits/std_list.h \
	bits/std_map.h bits/std_memory.h bits/std_numeric.h \
	bits/std_queue.h bits/std_set.h bits/std_stack.h \
	bits/std_stdexcept.h bits/functexcept.h bits/std_vector.h \
	bits/stl_algo.h bits/stl_algobase.h bits/stl_alloc.h \
	bits/stl_deque.h bits/stl_function.h \
	bits/stl_heap.h bits/stl_iterator.h bits/stl_list.h bits/stl_map.h \
	bits/stl_multimap.h bits/stl_multiset.h bits/stl_numeric.h \
	bits/stl_pair.h bits/stl_queue.h bits/stl_raw_storage_iter.h \
	bits/stl_relops.h bits/stl_set.h bits/stl_stack.h bits/stl_tempbuf.h \
	bits/stl_tree.h bits/stl_uninitialized.h bits/stl_vector.h \
	bits/stl_bvector.h bits/type_traits.h bits/std_algorithm.h \
	bits/stl_construct.h bits/boost_concept_check.h bits/concept_check.h


backward_headers = \
	backward/complex.h backward/iomanip.h backward/istream.h \
	backward/ostream.h backward/stream.h backward/streambuf.h \
	backward/algo.h backward/algobase.h backward/alloc.h \
	backward/bvector.h backward/defalloc.h backward/deque.h \
	backward/function.h backward/hash_map.h backward/hash_set.h \
	backward/hashtable.h backward/heap.h backward/iostream.h \
	backward/iterator.h backward/list.h backward/map.h \
	backward/multimap.h backward/new.h backward/multiset.h \
	backward/pair.h backward/queue.h backward/rope.h backward/set.h \
	backward/slist.h backward/stack.h backward/tempbuf.h backward/tree.h \
	backward/vector.h backward/fstream.h backward/strstream.h \
	backward/strstream backward/backward_warning.h

ext_headers = \
	ext/rope ext/ropeimpl.h ext/stl_rope.h \
	ext/slist ext/hash_map ext/hash_set \
	ext/stl_hashtable.h ext/stl_hash_fun.h 

c_base_headers = \
	bits/std_cassert.h bits/std_cctype.h bits/std_cerrno.h \
	bits/std_cfloat.h bits/std_climits.h bits/std_clocale.h \
	bits/std_cmath.h bits/std_csetjmp.h bits/std_csignal.h \
	bits/std_cstdarg.h bits/std_cstddef.h bits/std_cstdio.h \
	bits/std_cstdlib.h bits/std_cstring.h bits/std_ctime.h \
	bits/std_cwchar.h bits/std_cwctype.h bits/cmath.tcc

if GLIBCPP_USE_CSHADOW
c_shadow_headers = \
	assert.h ctype.h errno.h float.h limits.h locale.h math.h setjmp.h \
	signal.h stdarg.h stddef.h stdio.h stdlib.h string.h time.h wchar.h \
	wctype.h fcntl.h libio.h iolibio.h libioP.h pthread.h iconv.h \
	features.h langinfo.h \
	bits/wrap_libio.h bits/wrap_iolibio.h bits/wrap_libioP.h \
	bits/wrap_iconv.h bits/wrap_fcntl.h bits/wrap_pthread.h \
	bits/wrap_features.h bits/wrap_langinfo.h \
	sys/cdefs.h 
else
c_shadow_headers =
endif

std_headers = \
	algorithm bitset complex deque fstream functional \
	iomanip ios iosfwd iostream istream iterator limits list locale \
	map memory numeric ostream queue set sstream stack stdexcept \
	streambuf string utility valarray vector \
	cassert cctype cerrno cfloat climits clocale ciso646 \
	cmath csetjmp csignal cstdarg cstddef cstdio cstdlib \
	cstring ctime cwchar cwctype

if GLIBCPP_NEED_LIBIO 
libio_headers = \
	$(top_srcdir)/libio/_G_config.h $(top_srcdir)/libio/libio.h
else
libio_headers =
endif

build_headers = \
	bits/std_limits.h \
	bits/c++config.h bits/c++io.h bits/c++locale.h \
	bits/gthr.h bits/gthr-single.h bits/gthr-default.h \
	bits/basic_file_model.h \
	bits/atomicity.h bits/os_defines.h \
	bits/ctype_base.h bits/ctype_noninline.h bits/ctype_inline.h 

sources = \
	limitsMEMBERS.cc \
	stdexcept.cc functexcept.cc bitset.cc \
	globals.cc \
	basic_file.cc ios.cc complex_io.cc strstream.cc \
	c++locale.cc locale.cc localename.cc codecvt.cc \
	locale-inst.cc stl-inst.cc misc-inst.cc valarray-inst.cc \
	string-inst.cc wstring-inst.cc 


VPATH = $(top_srcdir):$(top_srcdir)/src:$(GLIBCPP_INCLUDE_DIR):$(GLIBCPP_INCLUDE_DIR)/std:$(C_INCLUDE_DIR)

libstdc___la_SOURCES = $(sources)

libstdc___la_LIBADD = \
	../libmath/libmath.la @libio_la@ \
	../libsupc++/libsupc++convenience.la

libstdc___la_LDFLAGS = -version-info 3:0:0 -lm

libstdc___la_DEPENDENCIES = $(libstdc___la_LIBADD)


# Use special rules for the deprecated source files so that they find
# deprecated include files.
strstream.lo: strstream.cc
	$(LTCXXCOMPILE) -I$(GLIBCPP_INCLUDE_DIR)/backward -c $<
strstream.o: strstream.cc
	$(CXXCOMPILE) -I$(GLIBCPP_INCLUDE_DIR)/backward -c $<

# Make sure cshadow headers are built first.
if GLIBCPP_USE_CSHADOW
CSHADOW_H = $(top_builddir)/stamp-cshadow
else
CSHADOW_H =
endif

# Specify that all *.o's depend on this.
$(libstdc___la_OBJECTS): $(CSHADOW_H)

$(top_builddir)/stamp-cshadow: $(top_srcdir)/mkinclosure \
	                       $(top_srcdir)/mkcshadow
	$(top_srcdir)/mkinclosure \
	"-I $(top_builddir)/../../gcc/include -I /usr/include -G fcntl.h unistd.h" | $(top_srcdir)/mkcshadow;
	rm -f  $(top_builddir)/stamp-cshadow 
	echo "done" > $(top_builddir)/stamp-cshadow 


# Check for various configure bits that change where the headers get installed.
gxx_include_dir = @gxx_include_dir@
gxx_target_include_dir = $(gxx_include_dir)/$(target_alias)

# We have our own special, ridiculously complicated installation routine
# here, as automake/autoconf is currently brain-damaged when it comes
# to installing sub-directories of headers. In particular, we want to
# 1) install build headers from (blddir)/include/bits -> (install)/bits
# 2) install source headers from
#      (srcdir)/include/bits -> (install)/bits
#      (srcdir)/include/ext -> (install)/ext
#      (srcdir)/include/backward -> (install)/backward
#      ... and the always entertaining "C" compatibility bits
#      where * represents configure-time directory switching
#      (srcdir)/include/c* -> (install)/
#      (srcdir)/include/c*/bits -> (install)/bits
#      (srcdir)/include/c*/sys -> (install)/sys
install-data-local: myinstalldirs myinstallheaders

# NB: installation of shadow header directories is not attempted.
myinstalldirs:
	if test -z "$(MULTISUBDIR)"; then \
	$(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/backward; \
	$(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/bits; \
	$(mkinstalldirs) $(DESTDIR)$(gxx_include_dir)/ext; \
	$(mkinstalldirs) $(DESTDIR)$(gxx_target_include_dir)/bits; \
	fi

# NB: As libio_headers may be empty, need this to make sure bash doesn't
# choke on an empty for... loop by using libio_headers_install
# NB: installation of shadow headers is not attempted.
src_incdir = @GLIBCPP_INCLUDE_DIR@
bld_incdir = $(top_builddir)/include
c_incdir = @C_INCLUDE_DIR@
myinstallheaders: 
	if test -z "$(MULTISUBDIR)"; then \
	for i in $(base_headers); do \
	echo "$(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/bits/";\
	  $(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/bits/; \
	done; \
	for i in $(ext_headers); do \
	echo "$(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/ext/";\
	  $(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/ext/; \
	done; \
	for i in $(backward_headers); do \
	echo "$(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/backward/";\
	  $(INSTALL_DATA) $(src_incdir)/$$i $(gxx_include_dir)/backward/; \
	done; \
	for i in $(c_base_headers); do \
	echo "$(INSTALL_DATA) $(c_incdir)/$$i $(gxx_include_dir)/bits/";\
	  $(INSTALL_DATA) $(c_incdir)/$$i $(gxx_include_dir)/bits/; \
	done; \
	for i in $(std_headers); do \
	echo "$(INSTALL_DATA) $(src_incdir)/std/$$i $(gxx_include_dir)";\
	  $(INSTALL_DATA) $(src_incdir)/std/$$i $(gxx_include_dir); \
	done; \
	for i in $(build_headers); do \
	echo "$(INSTALL_DATA) $(bld_incdir)/$$i \
	  $(gxx_target_include_dir)/bits/"; $(INSTALL_DATA) \
	  $(bld_incdir)/$$i $(gxx_target_include_dir)/bits/; \
	done; \
	libio_headers_install='$(libio_headers)'; \
	for i in $$libio_headers_install; do \
	echo "$(INSTALL_DATA) $$i $(gxx_include_dir)"; \
	  $(INSTALL_DATA) $$i $(gxx_include_dir); \
	done; \
	fi;


# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion call for it. (ie, --enable-debug)
#
# The no-implicit-templates flag will generate unresolved references to
# the concept-checking symbols.  So we must disable the checks while
# actually building the library.
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
