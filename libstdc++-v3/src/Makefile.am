## Makefile for the src subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

## $Id: Makefile.am,v 1.28 2000/08/16 18:36:57 aoliva Exp $

AUTOMAKE_OPTIONS = 1.3 gnits
MAINT_CHARSET = latin1

# Cross compiler and multilib support.
# Install a library built with a cross compiler in tooldir, not libdir.
if USE_LIBDIR
toolexeclibdir = $(libdir)$(MULTISUBDIR)
else
toolexecdir = $(exec_prefix)/$(target_alias)
toolexeclibdir = $(toolexecdir)/lib$(MULTISUBDIR)
endif

toolexeclib_LTLIBRARIES = libstdc++.la
EXTRA_LTLIBRARIES = libinst-string.la libinst-wstring.la


# Using these options is probably a good idea for thrill-seekers. Just
# uncomment the lines below and make, everything else is ready to go... 
# OPTIMIZE_CXXFLAGS = -O3 -fstrict-aliasing -fvtable-gc 

# These bits are all figured out from configure. Look in acinclude.m4
# or configure.in to see how they are set.
# NB: DEBUGFLAGS have to be at the end so that -O2 can be overridden.
AC_CXXFLAGS = \
	$(WERROR) @WFMT_FLAGS@ @CPU_FLAGS@ @EXTRA_CXX_FLAGS@ \
	@SECTION_FLAGS@ @DEBUG_FLAGS@ 

# Need to manually set this option because AC_CXXFLAGS has to be at
# the end of the compile line so that -O2 can be overridden as the
# occasion call for it. (ie, --enable-debug)
AM_CXXFLAGS = \
	-fno-implicit-templates $(OPTIMIZE_CXXFLAGS) \
	-Wall -Wno-format -W -Wwrite-strings -Winline \
	$(AC_CXXFLAGS) 


# Passed down for cross compilers, canadian crosses.
TOPLEVEL_INCLUDES = -I$(includedir)

if GLIBCPP_NEED_LIBIO
LIBIO_INCLUDES = \
	-I$(top_builddir)/libio -I$(top_srcdir)/libio 
else
LIBIO_INCLUDES = -I$(top_srcdir)/libio
endif

if GLIBCPP_USE_CSHADOW
CSHADOW_INCLUDES = 
#CSHADOW_INCLUDES = \
#	@CSHADOWFLAGS@ -I$(top_srcdir)/shadow -I$(top_builddir)/cshadow
else
CSHADOW_INCLUDES =
endif

CONFIG_INCLUDES = \
	-I$(top_srcdir)/@cpu_include_dir@ \
	-I$(top_srcdir)/@ctype_include_dir@

INCLUDES = \
	-D_GNU_SOURCE -nostdinc++ $(CSHADOW_INCLUDES) -I$(top_srcdir) \
	 $(LIBIO_INCLUDES) $(CONFIG_INCLUDES) $(TOPLEVEL_INCLUDES)	


# Need to explicitly set this so that AM_CXXFLAGS is last. (That way,
# things like -O2 passed down from the toplevel can be overridden by
# --enable-debug.)
CXXCOMPILE = $(CXX) $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
	     $(CXXFLAGS) $(AM_CXXFLAGS) 

LTCXXCOMPILE = $(LIBTOOL) --mode=compile $(CXX) $(DEFS) $(INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(AM_CXXFLAGS) 

# We have a problem when building the shared libstdc++ object if the
# rules automake generates would be used.  We cannot allow CXX to be used
# in libtool since this would add -lstdc++ to the link line which of
# course is impossible.
CXXLINK = $(LIBTOOL) --mode=link "$(CC)" @OPT_LDFLAGS@ @SECTION_LDFLAGS@ $(AM_CXXFLAGS) $(LDFLAGS) -o $@


headers = \
	bits/cpp_type_traits.h \
	bits/std_cctype.h bits/ctype_base.h bits/ctype_specializations.h \
	bits/char_traits.h bits/codecvt.h \
	bits/basic_string.h bits/std_string.h  bits/string.tcc \
	bits/generic_shadow.h bits/std_utility.h \
	bits/std_complex.h \
	bits/valarray_array.h bits/valarray_array.tcc bits/valarray_meta.h \
	bits/std_valarray.h bits/mask_array.h bits/slice.h bits/slice_array.h \
	bits/gslice.h bits/gslice_array.h bits/indirect_array.h \
	bits/std_cassert.h bits/std_cwctype.h \
	bits/std_cerrno.h bits/std_cfloat.h bits/std_climits.h \
	bits/std_clocale.h bits/std_cmath.h \
	bits/std_csetjmp.h bits/std_csignal.h bits/std_cstdarg.h \
	bits/std_cstddef.h bits/std_cstdio.h bits/std_cstdlib.h \
	bits/std_cstring.h bits/std_ctime.h bits/std_cwchar.h \
	bits/std_typeinfo.h bits/std_new.h \
	bits/std_exception.h bits/exception_support.h \
	bits/std_fstream.h bits/std_iomanip.h \
	bits/ios_base.h bits/fpos.h bits/basic_ios.h bits/basic_ios.tcc \
	bits/std_ios.h bits/std_iosfwd.h bits/std_iostream.h \
	bits/std_istream.h bits/istream.tcc bits/std_locale.h \
	bits/fstream.tcc bits/ostream.tcc bits/sbuf_iter.h bits/sstream.tcc \
	bits/std_ostream.h bits/std_sstream.h bits/std_streambuf.h \
	bits/streambuf.tcc bits/basic_file.h \
	bits/locale_facets.h bits/locale_facets.tcc bits/localefwd.h \
	bits/stl_pthread_alloc.h bits/pthread_allocimpl.h \
	bits/stl_threads.h bits/stl_iterator_base.h \
	backward/algo.h backward/algobase.h backward/alloc.h \
	backward/bvector.h backward/defalloc.h backward/deque.h \
	backward/function.h backward/hash_map.h backward/hash_set.h \
	backward/hashtable.h backward/heap.h backward/iterator.h \
	backward/list.h backward/map.h backward/multimap.h backward/new.h \
	backward/multiset.h backward/pair.h backward/iostream.h \
	backward/rope.h backward/set.h backward/slist.h backward/stack.h \
	backward/tempbuf.h backward/tree.h backward/vector.h \
	bits/std_bitset.h bits/std_deque.h bits/std_functional.h \
	bits/std_iterator.h bits/std_list.h \
	bits/std_map.h bits/std_memory.h bits/std_numeric.h \
	bits/std_queue.h bits/std_set.h bits/std_stack.h \
	bits/std_stdexcept.h bits/std_vector.h \
	bits/stl_algo.h bits/stl_algobase.h bits/stl_alloc.h \
	bits/stl_deque.h bits/stl_function.h \
	bits/stl_heap.h bits/stl_iterator.h bits/stl_list.h bits/stl_map.h \
	bits/stl_multimap.h bits/stl_multiset.h bits/stl_numeric.h \
	bits/stl_pair.h bits/stl_queue.h bits/stl_raw_storage_iter.h \
	bits/stl_relops.h bits/stl_set.h \
	bits/stl_stack.h bits/stl_string_fwd.h bits/stl_tempbuf.h \
	bits/stl_tree.h bits/stl_uninitialized.h bits/stl_vector.h \
	bits/type_traits.h bits/stl_range_errors.h bits/std_algorithm.h \
	bits/concept_checks.h bits/container_concepts.h \
	bits/sequence_concepts.h bits/std_strstream.h \
	ext/ropeimpl.h ext/stl_rope.h \
	ext/stl_bvector.h bits/stl_config.h bits/stl_construct.h \
	ext/stl_hashtable.h ext/stl_hash_fun.h \
	ext/hash_map ext/hash_set ext/rope ext/slist \
	ext/tree ext/bvector \
	shadow/assert.h shadow/ctype.h shadow/errno.h shadow/float.h \
	shadow/libio.h \
	shadow/limits.h shadow/locale.h shadow/math.h shadow/setjmp.h \
	shadow/signal.h shadow/stdarg.h shadow/stddef.h shadow/stdio.h \
	shadow/stdlib.h shadow/string.h shadow/time.h \
	shadow/unistd.h shadow/wchar.h shadow/wctype.h \
	shadow/bits/std_cassert.h shadow/bits/std_cctype.h \
	shadow/bits/std_cerrno.h shadow/bits/std_cfloat.h \
	shadow/bits/std_climits.h shadow/bits/std_clocale.h \
	shadow/bits/std_cmath.h shadow/bits/std_csetjmp.h \
	shadow/bits/std_csignal.h shadow/bits/std_cstdarg.h \
	shadow/bits/std_cstddef.h shadow/bits/std_cstdio.h \
	shadow/bits/std_cstdlib.h shadow/bits/std_cstring.h \
	shadow/bits/std_ctime.h shadow/bits/std_cwchar.h \
	shadow/bits/std_cwctype.h \
	shadow/bits/wrap_libio.h shadow/bits/wrap_unistd.h \
	shadow/sys/cdefs.h 

std_headers = \
	algorithm bitset cassert cctype cerrno cfloat climits clocale ciso646 \
	cmath complex csetjmp csignal cstdarg cstddef cstdio cstdlib \
	cstring ctime cwchar cwctype deque exception fstream functional \
	iomanip ios iosfwd iostream istream iterator limits list locale \
	map memory new numeric ostream queue set sstream stack stdexcept \
	streambuf string strstream typeinfo utility valarray vector 

if GLIBCPP_NEED_LIBIO 
libio_headers = \
	$(top_srcdir)/libio/_G_config.h $(top_srcdir)/libio/libio.h
else
libio_headers =
endif

generated_headers = \
	 $(top_builddir)/bits/std_limits.h $(top_builddir)/bits/c++config.h \
	 $(top_builddir)/bits/c++threads.h $(top_builddir)/bits/c++io.h

cpu_headers = \
	 $(top_srcdir)/@cpu_include_dir@/bits/atomicity.h 


sources = \
	limitsMEMBERS.cc \
	cmath.cc \
	complex.cc complexf.cc complexl.cc complex_io.cc \
	stdexcept.cc \
	c++io.cc ios.cc stdstreams.cc strstream.cc \
	locale.cc localename.cc codecvt.cc \
	locale-inst.cc stl-inst.cc misc-inst.cc valarray-inst.cc string-inst.cc

wstring_sources = \
	wstring-inst.cc

VPATH += $(top_srcdir) $(top_srcdir)/std $(top_srcdir)/src
VPATH += $(top_srcdir)/@ctype_include_dir@


# Actual sources for the distro, but don't build these.
#EXTRA_sources = string-inst.cc 

libstdc___la_SOURCES = $(sources)

libinst_wstring_la_SOURCES = $(wstring_sources)

libstdc___la_LIBADD = \
	../math/libmath.la ../libio/libio.la \
	@libinst_wstring_la@

libstdc___la_LDFLAGS = -version-info 3:0:0 -lm

libstdc___la_DEPENDENCIES = $(libstdc___la_LIBADD)


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
	"-I $(top_builddir)/../../gcc/include -I /usr/include -G machine/ansi.h" | $(top_srcdir)/mkcshadow;
	rm -f  $(top_builddir)/stamp-cshadow 
	echo "done" > $(top_builddir)/stamp-cshadow 


# We cannot use the default rules to install headers since we cannot
# statically decide which headers to install.  So we have our own special
# installation routine here.
myincludep = $(DESTDIR)$(includedir)/g++-v3/

install: myinstallheaders

# NB: As libio_headers may be empty, need this to make sure bash doesn't
# choke on an empty for... loop by using libio_headers_install
myinstallheaders: $(headers:%=$(myincludep)%)
	if test -z "$(MULTISUBDIR)"; then \
	for i in $(std_headers); do \
	  echo "$(INSTALL_DATA) $(top_srcdir)/std/$$i $(myincludep)"; \
	        $(INSTALL_DATA) $(top_srcdir)/std/$$i $(myincludep); \
	done; \
	for i in $(generated_headers); do \
	  echo "$(INSTALL_DATA) $$i $(myincludep)bits/"; \
	        $(INSTALL_DATA) $$i $(myincludep)bits/; \
	done; \
	libio_headers_install='$(libio_headers)'; \
	for i in $$libio_headers_install; do \
          echo "$(INSTALL_DATA) $$i $(myincludep)"; \
                $(INSTALL_DATA) $$i $(myincludep); \
	done; \
	echo "$(INSTALL_DATA) $(cpu_headers) $(myincludep)bits/"; \
	$(INSTALL_DATA) $(cpu_headers) $(myincludep)bits/; \
	fi;

$(headers:%=$(myincludep)%): $(myincludep)%: %
	if test -z "$(MULTISUBDIR)"; then \
	$(mkinstalldirs) $(@D); \
	echo " $(INSTALL_DATA) $^ $@"; \
	$(INSTALL_DATA) $^ $@; \
	fi;

# All the machinations with string instantiations messes up the
# automake-generated TAGS rule. Make a simple one here.
TAGS: $(generated_headers) $(cpu_headers) 
	etags -C $(top_srcdir)/bits/*.h $(top_srcdir)/bits/*.tcc \
	$(generated_headers) $(cpu_headers) $(libio_headers) \
	$(top_srcdir)/src/*.cc \
	$(top_builddir)/src/limitsMEMBERS.cc $(top_builddir)/src/c++io.cc \
	-o $(top_builddir)/TAGS

# We have to handle misc-inst.cc and locale-inst.cc in a special way
# since we cannot instantiate all classes due to missing definitions
# for things like vptrs, type_info bits, etc. when using
# -fno-implicit-templates.
misc-inst.o: misc-inst.cc
	$(CXXCOMPILE) -fimplicit-templates -c $<
misc-inst.lo: misc-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c $<
locale-inst.o: locale-inst.cc
	$(CXXCOMPILE) -fimplicit-templates -c $<
locale-inst.lo: locale-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c $<

# We have to instantiate wstring bits too. . . 
wstring-inst.o: string-inst.cc
	$(CXXCOMPILE) -fimplicit-templates -c -DC=wchar_t $< -o $@
wstring-inst.lo: string-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c -DC=wchar_t $< -o $@

all: libstdc++.INC
libstdc++.INC: Makefile
	$(MAKE) \
	  top_builddir=`CDPATH=:. && cd $(top_builddir) && pwd` \
	  top_srcdir=`CDPATH=:. && cd $(top_srcdir) && pwd` \
	  tmp-$@
	-rm -f $@
	mv tmp-$@ $@

tmp-libstdc++.INC: Makefile
	echo -I$(top_builddir) $(INCLUDES) > $@
