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


WERROR = -Werror
# OPTIMIZE_CXXFLAGS =  -O3 -fstrict-aliasing -fsquangle -fnew-exceptions \
#                      -fdata-sections -ffunction-sections \
#		       -fvtable-gc -Wl,--gc-sections
# AC_CXXFLAGS = @OPTLEVEL@ @DEBUGFLAGS@ @NAMESPACES@
AC_CXXFLAGS = @DEBUGFLAGS@ 
AM_CXXFLAGS = -fno-implicit-templates \
	      -Wall -Wno-format -W -Wwrite-strings -Winline $(WERROR) \
	      $(OPTIMIZE_CXXFLAGS) $(AC_CXXFLAGS) 

# Need to explicitly set this so that AM_CXXFLAGS is last. (That way,
# things like -O2 passed down from the toplevel can be overridden by
# --enable-debug.)
CXXCOMPILE = $(CXX) $(DEFS) $(INCLUDES) \
	     $(AM_CPPFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(AM_CXXFLAGS) 

LTCXXCOMPILE = $(LIBTOOL) --mode=compile $(CXX) $(DEFS) $(INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(CXXFLAGS) $(AM_CXXFLAGS) 

# We have a problem when building the shared libstdc++ object if the
# rules automake generates would be used.  We cannot allow CXX to be used
# in libtool since this would add -lstdc++ to the link line which of
# course is impossible.
CXXLINK = $(LIBTOOL) --mode=link "$(CC)" $(AM_CXXFLAGS) $(LDFLAGS) -o $@

TOPLEVEL_INCLUDES = -I$(includedir)
LIBIO_INCLUDES = @BUILD_LIBIO_INCLUDE@ -I$(top_srcdir)/libio 

CONFIG_INCLUDES = \
	-I$(top_srcdir)/@cpu_include_dir@ \
	-I$(top_srcdir)/@ctype_include_dir@

INCLUDES = \
	-nostdinc++ -I$(top_srcdir) \
	$(LIBIO_INCLUDES) $(CONFIG_INCLUDES) $(TOPLEVEL_INCLUDES)	

headers = \
	bits/cpp_type_traits.h \
	bits/std_cctype.h bits/ctype_base.h bits/ctype_specializations.h \
	bits/char_traits.h \
	bits/basic_string.h bits/std_string.h  bits/string.tcc \
	bits/generic_shadow.h bits/std_utility.h \
	bits/std_complex.h \
	bits/valarray_array.h bits/valarray_array.tcc bits/valarray_meta.h \
	bits/std_valarray.h bits/mask_array.h bits/slice.h bits/slice_array.h \
	bits/gslice.h bits/gslice_array.h bits/indirect_array.h \
	bits/std_cassert.h bits/std_cwctype.h bits/std_ciso646.h \
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
	backward/list.h backward/map.h backward/multimap.h \
	backward/multiset.h backward/pair.h \
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
	bits/std_strstream.h \
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
	$(top_builddir)/libio/_G_config.h $(top_srcdir)/libio/libio.h
else
libio_headers =
endif

generated_headers = \
	bits/std_limits.h bits/c++config.h bits/c++threads.h bits/c++io.h

cpu_headers = \
	 $(top_srcdir)/@cpu_include_dir@/atomicity.h 

string_sources = \
	stringMAIN.cc stringCTORNC.cc stringCTORAL.cc \
	stringCTORCPR.cc stringCTORCPRAL.cc stringCTORPRAL.cc \
	stringCTORPAL.cc stringCTORDUPAL.cc stringCTORPP.cc stringCTORII.cc \
	stringMUTATE.cc stringRESERVE.cc \
	stringSWAP.cc stringSLOP.cc \
	stringRESIZE.cc stringAPPCOPY.cc stringAPPCPR.cc stringAPPPR.cc \
	stringAPPDUP.cc stringAPPII.cc stringASSCP.cc stringASSII.cc \
	stringINSII.cc stringREPRR.cc stringREPIIDUP.cc stringREPII.cc \
	stringREPIII.cc stringREP4I.cc stringREPIIPP.cc stringREPIIPP2.cc \
	stringCOPY.cc stringCONII.cc stringCONIIF.cc stringCONPPF.cc \
	stringCONPPF2.cc stringCONSC.cc \
	stringFIND.cc stringFINDC.cc stringRFIND.cc \
	stringRFINDC.cc stringFFO.cc stringFLO.cc stringFFNO.cc \
	stringFLNO.cc stringFLNOC.cc stringCOMPARE.cc stringCOMPARE2.cc \
	stringCOMPAREP.cc stringCOMPAREP2.cc stringADDPS.cc stringADDCS.cc \
	stringEXTRACT.cc stringINSERT.cc stringGETLINE.cc stringSCOPY.cc \
	stringEQ.cc

wstring_sources = \
	wstringMAIN.cc wstringCTORNC.cc wstringCTORAL.cc \
	wstringCTORCPR.cc wstringCTORCPRAL.cc wstringCTORPRAL.cc \
	wstringCTORPAL.cc wstringCTORDUPAL.cc wstringCTORPP.cc \
	wstringCTORII.cc \
	wstringMUTATE.cc wstringRESERVE.cc \
	wstringSWAP.cc wstringSLOP.cc \
	wstringRESIZE.cc wstringAPPCOPY.cc wstringAPPCPR.cc wstringAPPPR.cc \
	wstringAPPDUP.cc wstringAPPII.cc wstringASSCP.cc wstringASSII.cc \
	wstringINSII.cc wstringREPRR.cc wstringREPIIDUP.cc wstringREPII.cc \
	wstringREPIII.cc wstringREP4I.cc wstringREPIIPP.cc wstringREPIIPP2.cc \
	wstringCOPY.cc wstringCONII.cc wstringCONIIF.cc wstringCONPPF.cc \
	wstringCONPPF2.cc wstringCONSC.cc \
	wstringFIND.cc wstringFINDC.cc wstringRFIND.cc \
	wstringRFINDC.cc wstringFFO.cc wstringFLO.cc wstringFFNO.cc \
	wstringFLNO.cc wstringFLNOC.cc wstringCOMPARE.cc wstringCOMPARE2.cc \
	wstringCOMPAREP.cc wstringCOMPAREP2.cc wstringADDPS.cc \
	wstringADDCS.cc wstringEXTRACT.cc wstringINSERT.cc wstringGETLINE.cc \
	wstringSCOPY.cc wstringEQ.cc

sources = \
	limitsMEMBERS.cc c++io.cc \
	cmath.cc \
	complex.cc complexf.cc complexl.cc complex_io.cc \
	stdexcept.cc ios.cc stdstreams.cc strstream.cc \
	locale.cc localename.cc  \
	locale-inst.cc stl-inst.cc misc-inst.cc valarray-inst.cc \
	$(string_sources) $(wstring_sources)

VPATH += $(top_srcdir) $(top_srcdir)/std $(top_srcdir)/src
VPATH += $(top_srcdir)/@ctype_include_dir@


# Actual sources for the distro, but don't build these.
EXTRA_sources = string-inst.cc 

libstdc___la_SOURCES = $(sources)

libstdc___la_LIBADD = ../math/libmath.la ../libio/libio.la 

libstdc___la_LDFLAGS = -version-info 3:0:0 -lm


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
	  echo "$(INSTALL_DATA) $(top_builddir)/$$i $(myincludep)bits/"; \
	        $(INSTALL_DATA) $(top_builddir)/$$i $(myincludep)bits/; \
	done; \
	libio_headers_install='$(libio_headers)'; \
	for i in $$libio_headers_install; do \
          echo "$(INSTALL_DATA) $$i $(myincludep)"; \
                $(INSTALL_DATA) $$i $(myincludep); \
	done; \
	echo "$(INSTALL_DATA) $(cpu_headers) $(myincludep)"; \
	$(INSTALL_DATA) $(cpu_headers) $(myincludep); \
	fi;

$(headers:%=$(myincludep)%): $(myincludep)%: %
	if test -z "$(MULTISUBDIR)"; then \
	$(mkinstalldirs) $(@D); \
	echo " $(INSTALL_DATA) $^ $@"; \
	$(INSTALL_DATA) $^ $@; \
	fi;

# For the next time we have to handle misc-inst.cc and locale-inst.cc
# in a special way since we cannot instantiate all classes due to
# missing definitions for things like vptrs, type_info bits, etc. when
# using -fno-implicit-templates. 
misc-inst.o: misc-inst.cc
	$(CXXCOMPILE) -fimplicit-templates -c $<
misc-inst.lo: misc-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c $<
locale-inst.o: locale-inst.cc
	$(CXXCOMPILE) -fimplicit-templates -c $<
locale-inst.lo: locale-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates -c $<

stringMAIN.lo \
stringCTORNC.lo \
stringCTORAL.lo \
stringCTORCPR.lo \
stringCTORCPRAL.lo \
stringCTORPRAL.lo \
stringCTORPAL.lo \
stringCTORDUPAL.lo \
stringCTORPP.lo \
stringCTORII.lo \
stringMUTATE.lo \
stringRESERVE.lo \
stringSWAP.lo \
stringSLOP.lo \
stringRESIZE.lo \
stringAPPCOPY.lo \
stringAPPCPR.lo \
stringAPPPR.lo \
stringAPPDUP.lo \
stringAPPII.lo \
stringASSCP.lo \
stringASSII.lo \
stringINSII.lo \
stringREPRR.lo \
stringREPIIDUP.lo \
stringREPII.lo \
stringREPIII.lo \
stringREP4I.lo \
stringREPIIPP.lo \
stringREPIIPP2.lo \
stringCOPY.lo \
stringCONII.lo \
stringCONIIF.lo \
stringCONPPF.lo \
stringCONPPF2.lo \
stringCONSC.lo \
stringFIND.lo \
stringFINDC.lo \
stringRFIND.lo \
stringRFINDC.lo \
stringFFO.lo \
stringFLO.lo \
stringFFNO.lo \
stringFLNO.lo \
stringFLNOC.lo \
stringCOMPARE.lo \
stringCOMPARE2.lo \
stringCOMPAREP.lo \
stringCOMPAREP2.lo \
stringADDPS.lo \
stringADDCS.lo \
stringEXTRACT.lo \
stringINSERT.lo \
stringGETLINE.lo \
stringSCOPY.lo \
stringEQ.lo : string-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates \
	-c -D`echo $@ | tr -dc 'A-Z'` $< -o $@

wstringMAIN.lo \
wstringCTORNC.lo \
wstringCTORAL.lo \
wstringCTORCPR.lo \
wstringCTORCPRAL.lo \
wstringCTORPRAL.lo \
wstringCTORPAL.lo \
wstringCTORDUPAL.lo \
wstringCTORPP.lo \
wstringCTORII.lo \
wstringMUTATE.lo \
wstringRESERVE.lo \
wstringSWAP.lo \
wstringSLOP.lo \
wstringRESIZE.lo \
wstringAPPCOPY.lo \
wstringAPPCPR.lo \
wstringAPPPR.lo \
wstringAPPDUP.lo \
wstringAPPII.lo \
wstringASSCP.lo \
wstringASSII.lo \
wstringINSII.lo \
wstringREPRR.lo \
wstringREPIIDUP.lo \
wstringREPII.lo \
wstringREPIII.lo \
wstringREP4I.lo \
wstringREPIIPP.lo \
wstringREPIIPP2.lo \
wstringCOPY.lo \
wstringCONII.lo \
wstringCONIIF.lo \
wstringCONPPF.lo \
wstringCONPPF2.lo \
wstringCONSC.lo \
wstringFIND.lo \
wstringFINDC.lo \
wstringRFIND.lo \
wstringRFINDC.lo \
wstringFFO.lo \
wstringFLO.lo \
wstringFFNO.lo \
wstringFLNO.lo \
wstringFLNOC.lo \
wstringCOMPARE.lo \
wstringCOMPARE2.lo \
wstringCOMPAREP.lo \
wstringCOMPAREP2.lo \
wstringADDPS.lo \
wstringADDCS.lo \
wstringEXTRACT.lo \
wstringINSERT.lo \
wstringGETLINE.lo \
wstringSCOPY.lo \
wstringEQ.lo : string-inst.cc
	$(LTCXXCOMPILE) -fimplicit-templates \
	-c -D`echo $@ | tr -dc 'A-Z'` -DC=wchar_t $< -o $@

stringMAIN.o \
stringCTORNC.o \
stringCTORAL.o \
stringCTORCPR.o \
stringCTORCPRAL.o \
stringCTORPRAL.o \
stringCTORPAL.o \
stringCTORDUPAL.o \
stringCTORPP.o \
stringCTORII.o \
stringMUTATE.o \
stringRESERVE.o \
stringSWAP.o \
stringSLOP.o \
stringRESIZE.o \
stringAPPCOPY.o \
stringAPPCPR.o \
stringAPPPR.o \
stringAPPDUP.o \
stringAPPII.o \
stringASSCP.o \
stringASSII.o \
stringINSII.o \
stringREPRR.o \
stringREPIIDUP.o \
stringREPII.o \
stringREPIII.o \
stringREP4I.o \
stringREPIIPP.o \
stringREPIIPP2.o \
stringCOPY.o \
stringCONII.o \
stringCONIIF.o \
stringCONPPF.o \
stringCONPPF2.o \
stringCONSC.o \
stringFIND.o \
stringFINDC.o \
stringRFIND.o \
stringRFINDC.o \
stringFFO.o \
stringFLO.o \
stringFFNO.o \
stringFLNO.o \
stringFLNOC.o \
stringCOMPARE.o \
stringCOMPARE2.o \
stringCOMPAREP.o \
stringCOMPAREP2.o \
stringADDPS.o \
stringADDCS.o \
stringEXTRACT.o \
stringINSERT.o \
stringGETLINE.o \
stringSCOPY.o \
stringEQ.o : string-inst.cc
	$(CXXCOMPILE) -fimplicit-templates \
	-c -D`echo $@ | tr -dc 'A-Z'` $< -o $@

wstringMAIN.o \
wstringCTORNC.o \
wstringCTORAL.o \
wstringCTORCPR.o \
wstringCTORCPRAL.o \
wstringCTORPRAL.o \
wstringCTORPAL.o \
wstringCTORDUPAL.o \
wstringCTORPP.o \
wstringCTORII.o \
wstringMUTATE.o \
wstringRESERVE.o \
wstringSWAP.o \
wstringSLOP.o \
wstringRESIZE.o \
wstringAPPCOPY.o \
wstringAPPCPR.o \
wstringAPPPR.o \
wstringAPPDUP.o \
wstringAPPII.o \
wstringASSCP.o \
wstringASSII.o \
wstringINSII.o \
wstringREPRR.o \
wstringREPIIDUP.o \
wstringREPII.o \
wstringREPIII.o \
wstringREP4I.o \
wstringREPIIPP.o \
wstringREPIIPP2.o \
wstringCOPY.o \
wstringCONII.o \
wstringCONIIF.o \
wstringCONPPF.o \
wstringCONPPF2.o \
wstringCONSC.o \
wstringFIND.o \
wstringFINDC.o \
wstringRFIND.o \
wstringRFINDC.o \
wstringFFO.o \
wstringFLO.o \
wstringFFNO.o \
wstringFLNO.o \
wstringFLNOC.o \
wstringCOMPARE.o \
wstringCOMPARE2.o \
wstringCOMPAREP.o \
wstringCOMPAREP2.o \
wstringADDPS.o \
wstringADDCS.o \
wstringEXTRACT.o \
wstringINSERT.o \
wstringGETLINE.o \
wstringSCOPY.o \
wstringEQ.o : string-inst.cc
	$(CXXCOMPILE) -fimplicit-templates \
	-c -D`echo $@ | tr -dc 'A-Z'` -DC=wchar_t $< -o $@
