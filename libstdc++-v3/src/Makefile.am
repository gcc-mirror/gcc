## Makefile for the C++11 sources of the GNU C++ Standard library.
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

SUBDIRS = c++98 c++11

# Cross compiler support.
toolexeclib_LTLIBRARIES = libstdc++.la

vpath % $(top_srcdir)/src/c++98
vpath % $(top_srcdir)/src/c++11

if GLIBCXX_LDBL_COMPAT
ldbl_compat_sources = compatibility-ldbl.cc
else
ldbl_compat_sources =
endif

if ENABLE_PARALLEL
parallel_compat_sources = \
	compatibility-parallel_list.cc  compatibility-parallel_list-2.cc
else
parallel_compat_sources =
endif

cxx98_sources = \
	compatibility.cc \
	compatibility-debug_list.cc \
	compatibility-debug_list-2.cc \
	compatibility-list.cc \
	compatibility-list-2.cc \
	${ldbl_compat_sources} \
	${parallel_compat_sources}

cxx11_sources = \
	compatibility-c++0x.cc \
	compatibility-atomic-c++0x.cc \
	compatibility-thread-c++0x.cc

libstdc___la_SOURCES = $(cxx98_sources) $(cxx11_sources)

libstdc___la_LIBADD = \
	$(GLIBCXX_LIBS) \
	$(top_builddir)/libsupc++/libsupc++convenience.la \
	$(top_builddir)/src/c++98/libc++98convenience.la \
	$(top_builddir)/src/c++11/libc++11convenience.la

libstdc___la_DEPENDENCIES = \
	${version_dep} \
	$(top_builddir)/libsupc++/libsupc++convenience.la \
	$(top_builddir)/src/c++98/libc++98convenience.la \
	$(top_builddir)/src/c++11/libc++11convenience.la

libstdc___la_LDFLAGS = \
	-version-info $(libtool_VERSION) ${version_arg} -lm

libstdc___la_LINK = $(CXXLINK) $(libstdc___la_LDFLAGS)


# Use special rules for parallel mode compilation.
PARALLEL_FLAGS = -fopenmp -D_GLIBCXX_PARALLEL -I$(glibcxx_builddir)/../libgomp
compatibility-parallel_list.lo: compatibility-parallel_list.cc
	$(LTCXXCOMPILE) -c $<
compatibility-parallel_list.o: compatibility-parallel_list.cc
	$(CXXCOMPILE) -c $<

compatibility-parallel_list-2.lo: compatibility-parallel_list-2.cc
	$(LTCXXCOMPILE) -c $<
compatibility-parallel_list-2.o: compatibility-parallel_list-2.cc
	$(CXXCOMPILE) -c $<

# Use special rules for compatibility-ldbl.cc compilation, as we need to
# pass -mlong-double-64.
if GLIBCXX_LDBL_COMPAT
compatibility-ldbl.lo: compatibility-ldbl.cc
	$(LTCXXCOMPILE) -mlong-double-64 -c $<
compatibility-ldbl.o: compatibility-ldbl.cc
	$(CXXCOMPILE) -mlong-double-64 -c $<
endif

# Use special rules for C++11 files/objects.
compatibility-c++0x.lo: compatibility-c++0x.cc
	$(LTCXXCOMPILE) -std=gnu++11 -c $<
compatibility-c++0x.o: compatibility-c++0x.cc
	$(CXXCOMPILE) -std=gnu++11 -c $<

compatibility-atomic-c++0x.lo: compatibility-atomic-c++0x.cc
	$(LTCXXCOMPILE) -std=gnu++11 -c $<
compatibility-atomic-c++0x.o: compatibility-atomic-c++0x.cc
	$(CXXCOMPILE) -std=gnu++11 -c $<

compatibility-thread-c++0x.lo: compatibility-thread-c++0x.cc
	$(LTCXXCOMPILE) -std=gnu++11 -c $<
compatibility-thread-c++0x.o: compatibility-thread-c++0x.cc
	$(CXXCOMPILE) -std=gnu++11 -c $<

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion calls for it.
AM_CXXFLAGS = \
	$(XTEMPLATE_FLAGS) \
	$(WARN_CXXFLAGS) $(OPTIMIZE_CXXFLAGS) $(CONFIG_CXXFLAGS)

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
# attempt to infer which configuration to use
LTCXXCOMPILE = \
	$(LIBTOOL) --tag CXX \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(CXX) $(INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = \
	$(LIBTOOL) --tag CXX \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(CXX) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CXXFLAGS) $(LTLDFLAGS) -o $@


# Symbol versioning for shared libraries.
if ENABLE_SYMVERS
libstdc++-symbols.ver:  ${glibcxx_srcdir}/$(SYMVER_FILE) \
		$(port_specific_symbol_files)
	cp ${glibcxx_srcdir}/$(SYMVER_FILE) $@.tmp
	chmod +w $@.tmp
	if test "x$(port_specific_symbol_files)" != x; then \
	  if grep '^# Appended to version file.' \
	       $(port_specific_symbol_files) /dev/null > /dev/null 2>&1; then \
	    cat $(port_specific_symbol_files) >> $@.tmp; \
	  else \
	    sed -n '1,/DO NOT DELETE/p' $@.tmp > tmp.top; \
	    sed -n '/DO NOT DELETE/,$$p' $@.tmp > tmp.bottom; \
	    cat tmp.top $(port_specific_symbol_files) tmp.bottom > $@.tmp; \
	    rm tmp.top tmp.bottom; \
	  fi; \
	fi
	$(EGREP) -v '^[ 	]*#(#| |$$)' $@.tmp | \
	  $(CC) -E -P -include ../config.h - > $@ || (rm -f $@ ; exit 1)
	rm -f $@.tmp

CLEANFILES = libstdc++-symbols.ver

if ENABLE_SYMVERS_GNU
version_arg = -Wl,--version-script=libstdc++-symbols.ver
version_dep = libstdc++-symbols.ver
endif
if ENABLE_SYMVERS_GNU_NAMESPACE
version_arg = -Wl,--version-script=libstdc++-symbols.ver
version_dep = libstdc++-symbols.ver
endif
if ENABLE_SYMVERS_SUN
version_arg = -Wl,-M,libstdc++-symbols.ver-sun
version_dep = libstdc++-symbols.ver-sun
libstdc++-symbols.ver-sun : libstdc++-symbols.ver \
		$(toplevel_srcdir)/contrib/make_sunver.pl \
		$(libstdc___la_OBJECTS) $(libstdc___la_LIBADD)
	CXXFILT="$(CXXFILT)"; export CXXFILT; \
	perl $(toplevel_srcdir)/contrib/make_sunver.pl \
	  libstdc++-symbols.ver \
	  $(libstdc___la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libstdc___la_LIBADD) | \
	    sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif
if ENABLE_SYMVERS_DARWIN
version_arg = -Wl,-exported_symbols_list,libstdc++-symbols.explist
version_dep = libstdc++-symbols.explist
libstdc++-symbols.explist : libstdc++-symbols.ver \
		${glibcxx_srcdir}/scripts/make_exports.pl \
		$(libstdc___la_OBJECTS) $(libstdc___la_LIBADD)
	perl ${glibcxx_srcdir}/scripts/make_exports.pl \
	  libstdc++-symbols.ver \
	  $(libstdc___la_OBJECTS:%.lo=.libs/%.o) \
	 `echo $(libstdc___la_LIBADD) | \
	    sed 's,/\([^/.]*\)\.la,/.libs/\1.a,g'` \
	 > $@ || (rm -f $@ ; exit 1)
endif

CLEANFILES += $(version_dep)
else
version_arg =
version_dep =
endif


# Control additional build primary rules.
all-once: libstdc++convenience.la $(STAMP_DEBUG)
install-data-once: $(STAMP_INSTALL_DEBUG)

all-local: all-once
install-data-local: install-data-once
clean-local:
	rm -rf libstdc++convenience.la stamp* $(CLEAN_DEBUG)

# Make a non-installed convenience library, so that --disable-static
# may work.
libstdc++convenience.la: $(toolexeclib_LTLIBRARIES)
	$(CXXLINK) $(libstdc___la_LIBADD) $(LIBS); \
	if test ! -f .libs/libstdc++.a; then \
	  cp .libs/libstdc++convenience.a .libs/libstdc++.a; \
	fi; \
	echo `date` > stamp-libstdc++convenience;

# Added rules.
# 1 debug library
# 2 supra-convenience library
if GLIBCXX_BUILD_DEBUG
STAMP_DEBUG = build-debug
STAMP_INSTALL_DEBUG = install-debug
CLEAN_DEBUG = debug
else
STAMP_DEBUG =
STAMP_INSTALL_DEBUG =
CLEAN_DEBUG =
endif

# Build a debug variant.
# Take care to fix all possibly-relative paths.
debugdir = ${glibcxx_builddir}/src/debug
stamp-debug:
	if test ! -d ${debugdir}; then \
	  mkdir -p ${debugdir}; \
	  for d in $(SUBDIRS); do mkdir -p  ${debugdir}/$$d; done; \
	  (cd ${debugdir}; \
	  sed -e 's/top_builddir = \.\./top_builddir = ..\/../' \
	      -e 's/top_build_prefix = \.\./top_build_prefix = ..\/../' \
	      -e 's/srcdir = \.\./srcdir = ..\/../' \
	      -e 's/VPATH = \.\./VPATH = ..\/../' \
	      -e 's/glibcxx_basedir = \.\./glibcxx_basedir = ..\/../' \
	      -e 's/MKDIR_P = \.\./MKDIR_P = ..\/../' \
	  < ../Makefile > Makefile ; \
	  for d in . $(SUBDIRS); do \
	  sed -e 's/top_builddir = \.\./top_builddir = ..\/../' \
	      -e 's/top_build_prefix = \.\./top_build_prefix = ..\/../' \
	      -e 's/srcdir = \.\./srcdir = ..\/../' \
	      -e 's/VPATH = \.\./VPATH = ..\/../' \
	      -e 's/glibcxx_basedir = \.\./glibcxx_basedir = ..\/../' \
	      -e 's/MKDIR_P = \.\./MKDIR_P = ..\/../' \
	  < ../$$d/Makefile > $$d/Makefile ; \
	  done) ; \
	fi; \
	echo `date` > stamp-debug;

build-debug: stamp-debug
	  (cd ${debugdir}; \
	  mv Makefile Makefile.tmp; \
	  sed -e 's,all-local: all-once,all-local:,' \
	      -e 's,install-data-local: install-data-once,install-data-local:,' \
	      -e '/vpath/!s,src/c,src/debug/c,' \
	  < Makefile.tmp > Makefile ; \
	  rm -f Makefile.tmp ; \
	  $(MAKE) CXXFLAGS='$(DEBUG_FLAGS)' \
	  toolexeclibdir=$(glibcxx_toolexeclibdir)/debug all) ;

# Install debug library.
install-debug: build-debug
	(cd ${debugdir} && $(MAKE) CXXFLAGS='$(DEBUG_FLAGS)' \
	toolexeclibdir=$(glibcxx_toolexeclibdir)/debug install) ;
