## Makefile for the include subdirectory of the GNU C++ Standard library.
##
## Copyright (C) 2001 Free Software Foundation, Inc.
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

## $Id: Makefile.am,v 1.6 2001/08/14 01:24:28 bkoz Exp $

AUTOMAKE_OPTIONS = 1.3 gnits
MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

# Cross compiler and multilib support.
CXX = @glibcpp_CXX@
glibcpp_srcdir=@glibcpp_srcdir@
glibcpp_builddir=@glibcpp_builddir@

bits_srcdir = ${glibcpp_srcdir}/include/bits
bits_builddir = ./bits
bits_headers = \
	${bits_srcdir}/basic_file.h \
	${bits_srcdir}/basic_ios.h \
	${bits_srcdir}/basic_ios.tcc \
	${bits_srcdir}/basic_string.h \
	${bits_srcdir}/basic_string.tcc \
	${bits_srcdir}/boost_concept_check.h \
	${bits_srcdir}/char_traits.h \
	${bits_srcdir}/codecvt.h \
	${bits_srcdir}/concept_check.h \
	${bits_srcdir}/cpp_type_traits.h \
	${bits_srcdir}/fpos.h \
	${bits_srcdir}/fstream.tcc \
	${bits_srcdir}/functexcept.h \
	${bits_srcdir}/generic_shadow.h \
	${bits_srcdir}/gslice.h \
	${bits_srcdir}/gslice_array.h \
	${bits_srcdir}/indirect_array.h \
	${bits_srcdir}/ios_base.h \
	${bits_srcdir}/istream.tcc \
	${bits_srcdir}/locale_facets.h \
	${bits_srcdir}/locale_facets.tcc \
	${bits_srcdir}/localefwd.h \
	${bits_srcdir}/mask_array.h \
	${bits_srcdir}/ostream.tcc \
	${bits_srcdir}/pthread_allocimpl.h \
	${bits_srcdir}/stream_iterator.h \
	${bits_srcdir}/streambuf_iterator.h \
	${bits_srcdir}/slice.h \
	${bits_srcdir}/slice_array.h \
	${bits_srcdir}/sstream.tcc \
	${bits_srcdir}/std_algorithm.h \
	${bits_srcdir}/std_bitset.h \
	${bits_srcdir}/std_complex.h \
	${bits_srcdir}/std_deque.h \
	${bits_srcdir}/std_fstream.h \
	${bits_srcdir}/std_functional.h \
	${bits_srcdir}/std_iomanip.h \
	${bits_srcdir}/std_ios.h \
	${bits_srcdir}/std_iosfwd.h \
	${bits_srcdir}/std_iostream.h \
	${bits_srcdir}/std_istream.h \
	${bits_srcdir}/std_iterator.h \
	${bits_srcdir}/std_limits.h \
	${bits_srcdir}/std_list.h \
	${bits_srcdir}/std_locale.h \
	${bits_srcdir}/std_map.h \
	${bits_srcdir}/std_memory.h \
	${bits_srcdir}/std_numeric.h \
	${bits_srcdir}/std_ostream.h \
	${bits_srcdir}/std_queue.h \
	${bits_srcdir}/std_set.h \
	${bits_srcdir}/std_sstream.h \
	${bits_srcdir}/std_stack.h \
	${bits_srcdir}/std_stdexcept.h \
	${bits_srcdir}/std_streambuf.h \
	${bits_srcdir}/std_string.h \
	${bits_srcdir}/std_utility.h \
	${bits_srcdir}/std_valarray.h \
	${bits_srcdir}/std_vector.h \
	${bits_srcdir}/stl_algo.h \
	${bits_srcdir}/stl_algobase.h \
	${bits_srcdir}/stl_alloc.h \
	${bits_srcdir}/stl_bvector.h \
	${bits_srcdir}/stl_construct.h \
	${bits_srcdir}/stl_deque.h \
	${bits_srcdir}/stl_function.h \
	${bits_srcdir}/stl_heap.h \
	${bits_srcdir}/stl_iterator.h \
	${bits_srcdir}/stl_iterator_base_funcs.h \
	${bits_srcdir}/stl_iterator_base_types.h \
	${bits_srcdir}/stl_list.h \
	${bits_srcdir}/stl_map.h \
	${bits_srcdir}/stl_multimap.h \
	${bits_srcdir}/stl_multiset.h \
	${bits_srcdir}/stl_numeric.h \
	${bits_srcdir}/stl_pair.h \
	${bits_srcdir}/stl_pthread_alloc.h \
	${bits_srcdir}/stl_queue.h \
	${bits_srcdir}/stl_raw_storage_iter.h \
	${bits_srcdir}/stl_relops.h \
	${bits_srcdir}/stl_set.h \
	${bits_srcdir}/stl_stack.h \
	${bits_srcdir}/stl_tempbuf.h \
	${bits_srcdir}/stl_threads.h \
	${bits_srcdir}/stl_tree.h \
	${bits_srcdir}/stl_uninitialized.h \
	${bits_srcdir}/stl_vector.h \
	${bits_srcdir}/streambuf.tcc \
	${bits_srcdir}/stringfwd.h \
	${bits_srcdir}/type_traits.h \
	${bits_srcdir}/valarray_array.h \
	${bits_srcdir}/valarray_array.tcc \
	${bits_srcdir}/valarray_meta.h

backward_srcdir = ${glibcpp_srcdir}/include/backward
backward_builddir = ./backward
backward_headers = \
	${backward_srcdir}/complex.h \
	${backward_srcdir}/iomanip.h \
	${backward_srcdir}/istream.h \
	${backward_srcdir}/ostream.h \
	${backward_srcdir}/stream.h \
	${backward_srcdir}/streambuf.h \
	${backward_srcdir}/algo.h \
	${backward_srcdir}/algobase.h \
	${backward_srcdir}/alloc.h \
	${backward_srcdir}/bvector.h \
	${backward_srcdir}/defalloc.h \
	${backward_srcdir}/deque.h \
	${backward_srcdir}/function.h \
	${backward_srcdir}/hash_map.h \
	${backward_srcdir}/hash_set.h \
	${backward_srcdir}/hashtable.h \
	${backward_srcdir}/heap.h \
	${backward_srcdir}/iostream.h \
	${backward_srcdir}/iterator.h \
	${backward_srcdir}/list.h \
	${backward_srcdir}/map.h \
	${backward_srcdir}/multimap.h \
	${backward_srcdir}/new.h \
	${backward_srcdir}/multiset.h \
	${backward_srcdir}/pair.h \
	${backward_srcdir}/queue.h \
	${backward_srcdir}/rope.h \
	${backward_srcdir}/set.h \
	${backward_srcdir}/slist.h \
	${backward_srcdir}/stack.h \
	${backward_srcdir}/tempbuf.h \
	${backward_srcdir}/tree.h \
	${backward_srcdir}/vector.h \
	${backward_srcdir}/fstream.h \
	${backward_srcdir}/strstream.h \
	${backward_srcdir}/strstream \
	${backward_srcdir}/backward_warning.h

ext_srcdir = ${glibcpp_srcdir}/include/ext
ext_builddir = ./ext
ext_headers = \
	${ext_srcdir}/rope \
	${ext_srcdir}/ropeimpl.h \
	${ext_srcdir}/stl_rope.h \
	${ext_srcdir}/slist \
	${ext_srcdir}/hash_map \
	${ext_srcdir}/hash_set \
	${ext_srcdir}/stl_hashtable.h \
	${ext_srcdir}/stl_hash_fun.h

# This is the common subset of files that all three "C" header models use.
c_base_srcdir = @C_INCLUDE_DIR@/bits
c_base_builddir = ./bits
c_base_headers = \
	${c_base_srcdir}/std_cassert.h \
	${c_base_srcdir}/std_cctype.h \
	${c_base_srcdir}/std_cerrno.h \
	${c_base_srcdir}/std_cfloat.h \
	${c_base_srcdir}/std_climits.h \
	${c_base_srcdir}/std_clocale.h \
	${c_base_srcdir}/std_cmath.h \
	${c_base_srcdir}/std_csetjmp.h \
	${c_base_srcdir}/std_csignal.h \
	${c_base_srcdir}/std_cstdarg.h \
	${c_base_srcdir}/std_cstddef.h \
	${c_base_srcdir}/std_cstdio.h \
	${c_base_srcdir}/std_cstdlib.h \
	${c_base_srcdir}/std_cstring.h \
	${c_base_srcdir}/std_ctime.h \
	${c_base_srcdir}/std_cwchar.h \
	${c_base_srcdir}/std_cwctype.h \
	${c_base_srcdir}/cmath.tcc 

std_srcdir = ${glibcpp_srcdir}/include/std
std_builddir = ./
std_headers = \
	${std_srcdir}/algorithm \
	${std_srcdir}/bitset \
	${std_srcdir}/complex \
	${std_srcdir}/deque \
	${std_srcdir}/fstream \
	${std_srcdir}/functional \
	${std_srcdir}/iomanip \
	${std_srcdir}/ios \
	${std_srcdir}/iosfwd \
	${std_srcdir}/iostream \
	${std_srcdir}/istream \
	${std_srcdir}/iterator \
	${std_srcdir}/limits \
	${std_srcdir}/list \
	${std_srcdir}/locale \
	${std_srcdir}/map \
	${std_srcdir}/memory \
	${std_srcdir}/numeric \
	${std_srcdir}/ostream \
	${std_srcdir}/queue \
	${std_srcdir}/set \
	${std_srcdir}/sstream \
	${std_srcdir}/stack \
	${std_srcdir}/stdexcept \
	${std_srcdir}/streambuf \
	${std_srcdir}/string \
	${std_srcdir}/utility \
	${std_srcdir}/valarray \
	${std_srcdir}/vector \
	${std_srcdir}/cassert \
	${std_srcdir}/cctype \
	${std_srcdir}/cerrno \
	${std_srcdir}/cfloat \
	${std_srcdir}/climits \
	${std_srcdir}/clocale \
	${std_srcdir}/ciso646 \
	${std_srcdir}/cmath \
	${std_srcdir}/csetjmp \
	${std_srcdir}/csignal \
	${std_srcdir}/cstdarg \
	${std_srcdir}/cstddef \
	${std_srcdir}/cstdio \
	${std_srcdir}/cstdlib \
	${std_srcdir}/cstring \
	${std_srcdir}/ctime \
	${std_srcdir}/cwchar \
	${std_srcdir}/cwctype

target_srcdir = ${glibcpp_srcdir}/@OS_INC_SRCDIR@
target_builddir = ./${target_alias}/bits
target_headers = \
	${target_srcdir}/ctype_base.h \
	${target_srcdir}/ctype_inline.h \
	${target_srcdir}/ctype_noninline.h \
	${target_srcdir}/os_defines.h \
	${glibcpp_srcdir}/@ATOMICITY_INC_SRCDIR@/atomicity.h \
	${glibcpp_srcdir}/@LIMITS_INC_SRCDIR@/limits.h

thread_builddir = ./${target_alias}/bits
thread_headers = \
	${thread_builddir}/gthr.h \
	${thread_builddir}/gthr-single.h \
	${thread_builddir}/gthr-default.h

# List of all timestamp files.  By keeping only one copy of this list, both
# CLEANFILES and all-local are kept up-to-date.
allstamps = stamp-std stamp-bits stamp-c_base stamp-backward stamp-ext \
	stamp-target stamp-thread 


# Here are the rules for building the headers
all-local: ${target_builddir}/c++config.h ${allstamps}

stamp-std: ${std_headers}
	@if [ ! -d "${std_builddir}" ]; then \
	  mkdir -p ${std_builddir} ;\
	fi ;\
	(cd ${std_builddir} && @LN_S@ $? .) ;\
	echo `date` > stamp-std 

stamp-bits: ${bits_headers}
	@if [ ! -d "${bits_builddir}" ]; then \
	  mkdir -p ${bits_builddir} ;\
	fi ;\
	(cd ${bits_builddir} && @LN_S@ $? .) ;\
	echo `date` > stamp-bits

stamp-c_base: ${c_base_headers}
	@if [ ! -d "${c_base_builddir}" ]; then \
	  mkdir -p ${c_base_builddir} ;\
	fi ;\
	(cd ${c_base_builddir} && @LN_S@ $? .) ;\
	echo `date` > stamp-c_base

stamp-backward: ${backward_headers}
	@if [ ! -d "${backward_builddir}" ]; then \
	  mkdir -p ${backward_builddir} ;\
	fi ;\
	(cd ${backward_builddir} && @LN_S@ $? .) ;\
	echo `date` > stamp-backward

stamp-ext: ${ext_headers}
	@if [ ! -d "${ext_builddir}" ]; then \
	  mkdir -p ${ext_builddir} ;\
	fi ;\
	(cd ${ext_builddir} && @LN_S@ $? .) ;\
	echo `date` > stamp-ext

# This target is special.  The timestamp on a directory gets modified
# whenever a file it contains gets modified, and that'll throw off all the
# build dependencies that need this target.  On the other hand, someone
# could delete the directory and not the stamp file, faking out make.
${target_builddir}: stamp-${target_alias}
stamp-${target_alias}:
	@if [ ! -d ${target_builddir} ]; then \
	  mkdir -p ${target_builddir} ;\
	  echo `date` > stamp-${target_alias} ;\
	fi

# Target includes static.
stamp-target: ${target_headers} ${target_builddir}
	@cd ${target_builddir} ;\
	if [ ! -f stamp-target ]; then \
	  @LN_S@ $? . ;\
	  @LN_S@ ${glibcpp_srcdir}/@BASIC_FILE_H@ basic_file_model.h ;\
	  @LN_S@ ${glibcpp_srcdir}/@CSTDIO_H@ c++io.h ;\
	  @LN_S@ ${glibcpp_srcdir}/@CLOCALE_H@ c++locale.h ;\
	  @LN_S@ ${glibcpp_srcdir}/@CMESSAGES_H@ messages_members.h ;\
	  @LN_S@ ${glibcpp_srcdir}/@CCODECVT_H@ codecvt_specializations.h ;\
	  echo `date` > stamp-target; \
	fi

# Target includes dynamic.
${target_builddir}/c++config.h: ../config.h \
				${glibcpp_srcdir}/include/bits/c++config \
				${target_builddir}
	@cat ${glibcpp_srcdir}/include/bits/c++config > $@ ;\
	sed -e 's/HAVE_/_GLIBCPP_HAVE_/g' \
	    -e 's/PACKAGE/_GLIBCPP_PACKAGE/g' \
	    -e 's/VERSION/_GLIBCPP_VERSION/g' \
	    -e 's/WORDS_/_GLIBCPP_WORDS_/g' \
	    < ${glibcpp_builddir}/config.h >> $@ ;\
	echo "#endif // _CPP_CPPCONFIG_" >>$@

# Target includes for threads
glibcpp_thread_h = @glibcpp_thread_h@
uppercase = [ABCDEFGHIJKLMNOPQRSTUVWXYZ_]

stamp-thread: ${thread_headers} ${thread_builddir}
	@echo `date` > stamp-thread


${thread_builddir}/gthr.h:
	sed -e '/^#/s/\(${uppercase}${uppercase}*\)/_GLIBCPP_\1/g' \
	    -e 's,^#include "\(.*\)",#include <bits/\1>,g' \
	    < ${glibcpp_srcdir}/../gcc/gthr.h > $@

${thread_builddir}/gthr-single.h:
	sed -e 's/\(UNUSED\)/_GLIBCPP_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCPP_\1/g' \
	    < ${glibcpp_srcdir}/../gcc/gthr-single.h > $@

${thread_builddir}/gthr-default.h:
	sed -e 's/\(UNUSED\)/_GLIBCPP_\1/g' \
	    -e 's/\(GCC${uppercase}*_H\)/_GLIBCPP_\1/g' \
	    -e 's/\(${uppercase}*WEAK\)/_GLIBCPP_\1/g' \
	    < ${glibcpp_srcdir}/../gcc/${glibcpp_thread_h} > $@

# One big happy istallation:  just copy everything from the build to the
# install tree (except for the build stamps).
gxx_include_dir = @gxx_include_dir@
install-data-local:
	for file in `find . ! -name stamp-\* ! -name Makefile -print`; do \
	installFile=${gxx_include_dir}/$${file} ;\
	if [ -d $${file} ]; then \
	  $(INSTALL) -d $${installFile} ;\
	else \
	  $(INSTALL_DATA) $${file} $${installFile} ;\
	fi ;\
	done

# By adding these files here, automake will remove them for 'make clean'
#CLEANFILES = ${allstamps}


