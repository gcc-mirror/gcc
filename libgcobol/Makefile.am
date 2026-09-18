# Copyright (C) 2026 Free Software Foundation, Inc.
# Contributed by the Symas Corporation, 2025

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Written de novo for libgcobol.

# Standard autoconf-set variables.
SHELL = @SHELL@
srcdir = @srcdir@
libdir = @libdir@
build = @build@
target = @target@
prefix = @prefix@

AM_COBC = ../../gcc/gcobol -B $(PWD)/../../gcc
AM_COBFLAGS = -dialect gnu -ffixed-form					\
		-I ${srcdir}/compat/gnu/cpy -I ${srcdir}/posix/cpy

target_noncanonical = @target_noncanonical@
version := $(shell @get_gcc_base_ver@ $(srcdir)/../gcc/BASE-VER)

AUTOMAKE_OPTIONS = 1.8 foreign subdir-objects
ACLOCAL_AMFLAGS = -I .. -I ../config

# May be used by various substitution variables.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

# Skip the whole process if we are not building libgcobol.
if BUILD_LIBGCOBOL
toolexeclib_LTLIBRARIES  = libgcobol.la libgcobol_posix.la libgcobol_compat_gnu.la
toolexeclib_DATA = libgcobol.spec

libsubdir := $(libdir)/gcc/$(target_noncanonical)/$(version)$(MULTISUBDIR)/cobol
libsubincludedir = $(libsubdir)

## 2.2.12 Automatic Dependency Tracking
## Automake generates code for automatic dependency tracking by default

libgcobol_la_SOURCES =      \
    charmaps.cc             \
    constants.cc            \
    gfileio.cc              \
    gmath.cc                \
    inspect.cc              \
    intrinsic.cc            \
    io.cc                   \
    libgcobol.cc            \
    posix/shim/errno.cc     \
    posix/shim/fstat.cc     \
    posix/shim/localtime.cc \
    posix/shim/lseek.cc     \
    posix/shim/open.cc      \
    posix/shim/stat.cc      \
    stringbin.cc            \
    unstring.cc             \
    valconv.cc              \
    xmlparse.cc

libgcobol_compat_gnu_la_SOURCES =		\
	compat/gnu/lib/cobrt-file-status.cbl	\
	compat/gnu/lib/stored-char-length.cbl	\
	compat/gnu/lib/CBL_ALLOC_MEM.cbl	\
	compat/gnu/lib/CBL_CHECK_FILE_EXIST.cbl	\
	compat/gnu/lib/CBL_CLOSE_FILE.cbl	\
	compat/gnu/lib/CBL_CREATE_FILE.cbl	\
	compat/gnu/lib/CBL_DELETE_FILE.cbl	\
	compat/gnu/lib/CBL_FREE_MEM.cbl		\
	compat/gnu/lib/CBL_GET_PROGRAM_INFO.cbl	\
	compat/gnu/lib/CBL_OPEN_FILE.cbl	\
	compat/gnu/lib/CBL_READ_FILE.cbl	\
	compat/gnu/lib/CBL_WRITE_FILE.cbl	\
	compat/gnu/lib/cbl_gpi.cc

libgcobol_posix_la_SOURCES =			\
	posix/udf/posix-close.cbl		\
	posix/udf/posix-errno.cbl		\
	posix/udf/posix-exit.cbl		\
	posix/udf/posix-fstat.cbl		\
	posix/udf/posix-ftruncate.cbl		\
	posix/udf/posix-localtime.cbl		\
	posix/udf/posix-lseek.cbl		\
	posix/udf/posix-mkdir.cbl		\
	posix/udf/posix-open.cbl		\
	posix/udf/posix-read.cbl		\
	posix/udf/posix-stat.cbl		\
	posix/udf/posix-unlink.cbl		\
	posix/udf/posix-write.cbl

# Install the COBOL for the POSIX and compatibility libraries.
nobase_libsubinclude_HEADERS =			\
	posix/cpy/posix-errno.cpy		\
	posix/cpy/posix-exit.cpy		\
	posix/cpy/posix-fstat.cpy		\
	posix/cpy/posix-ftruncate.cpy		\
	posix/cpy/posix-localtime.cpy		\
	posix/cpy/posix-lseek.cpy		\
	posix/cpy/posix-mkdir.cpy		\
	posix/cpy/posix-open.cpy		\
	posix/cpy/posix-read.cpy		\
	posix/cpy/posix-stat.cpy		\
	posix/cpy/psx-lseek.cpy			\
	posix/cpy/psx-open.cpy			\
	posix/cpy/statbuf.cpy			\
	posix/cpy/tm.cpy			\
	compat/gnu/cpy/stored-char-length.cpy	\
	compat/gnu/cpy/cblproto.cpy		\
	compat/gnu/cpy/cbltypes.cpy		\
	compat/gnu/lib/CBL_ALLOC_MEM.cbl	\
	compat/gnu/lib/CBL_CHECK_FILE_EXIST.cbl	\
	compat/gnu/lib/CBL_CLOSE_FILE.cbl	\
	compat/gnu/lib/CBL_CREATE_FILE.cbl	\
	compat/gnu/lib/CBL_DELETE_FILE.cbl	\
	compat/gnu/lib/CBL_FREE_MEM.cbl		\
	compat/gnu/lib/CBL_OPEN_FILE.cbl	\
	compat/gnu/lib/CBL_READ_FILE.cbl	\
	compat/gnu/lib/CBL_WRITE_FILE.cbl

libgcobol_la_LIBADD = -lxml2

WARN_CFLAGS = -W -Wall -Wwrite-strings

AM_CPPFLAGS = -I. -I posix/shim $(LIBQUADINCLUDE)
AM_CPPFLAGS += $(LIBXML2_CPPFLAGS)

AM_CFLAGS = $(XCFLAGS)
AM_CXXFLAGS = $(XCFLAGS)
AM_CXXFLAGS += $(WARN_CFLAGS)
AM_CXXFLAGS += -DIN_TARGET_LIBS
AM_CXXFLAGS += -fstrict-aliasing -Wstrict-aliasing -Wstrict-aliasing=3

if ENABLE_DARWIN_AT_RPATH
# Handle embedded rpaths for Darwin.
extra_ldflags_libgcobol += -Wc,-nodefaultrpaths
extra_ldflags_libgcobol += -Wl,-rpath,@loader_path
endif

# Bring compat copybooks into the build tree because dg tests rely on them.
LC_COPYDIR_BUILD = $(PWD)/compat/gnu/cpy
LC_COPYBOOKS = \
	$(LC_COPYDIR_BUILD)/cblproto.cpy \
	$(LC_COPYDIR_BUILD)/cbltypes.cpy \
	$(LC_COPYDIR_BUILD)/stored-char-length.cpy

$(LC_COPYDIR_BUILD)/%.cpy: ${srcdir}/compat/gnu/cpy/%.cpy
	mkdir -p `dirname $@`
	cp $< $@

# Bring posix copybooks into the build tree.
LP_COPYDIR_BUILD = $(PWD)/posix/cpy
LP_COPYBOOKS = \
	$(LP_COPYDIR_BUILD)/posix-errno.cpy \
	$(LP_COPYDIR_BUILD)/posix-exit.cpy \
	$(LP_COPYDIR_BUILD)/posix-fstat.cpy \
	$(LP_COPYDIR_BUILD)/posix-ftruncate.cpy \
	$(LP_COPYDIR_BUILD)/posix-localtime.cpy \
	$(LP_COPYDIR_BUILD)/posix-lseek.cpy \
	$(LP_COPYDIR_BUILD)/posix-mkdir.cpy \
	$(LP_COPYDIR_BUILD)/posix-close.cpy \
	$(LP_COPYDIR_BUILD)/posix-open.cpy \
	$(LP_COPYDIR_BUILD)/posix-read.cpy \
	$(LP_COPYDIR_BUILD)/posix-stat.cpy \
	$(LP_COPYDIR_BUILD)/psx-lseek.cpy \
	$(LP_COPYDIR_BUILD)/psx-open.cpy \
	$(LP_COPYDIR_BUILD)/statbuf.cpy \
	$(LP_COPYDIR_BUILD)/tm.cpy

$(LP_COPYDIR_BUILD)/%.cpy: ${srcdir}/posix/cpy/%.cpy
	mkdir -p `dirname $@`
	cp $< $@

# We want to link with the c++ runtime.
libgcobol_la_LINK = $(CXXLINK) $(libgcobol_la_LDFLAGS)
version_arg = -version-info $(LIBGCOBOL_VERSION)
libgcobol_la_LDFLAGS = $(LTLDFLAGS) $(LIBQUADLIB) $(LTLIBICONV) \
	$(extra_ldflags_libgcobol) $(LIBS) $(LIBXML2_LIBS) $(version_arg)
libgcobol_la_DEPENDENCIES = libgcobol.spec $(LIBQUADLIB_DEP)

# Rules for libgcobol_posix.so and libgcobol_compat_gnu.so, which have
# COBOL sources.  They require gcobol and libgcobol to have already
# been built.
#
# Here we adopt for the first time what is perhaps a useful convention:
# 1.  CC => COBC
#     Because CC means "C compiler", COBC means "COBOL compiler".
# 2.  CFLAGS => COBFLAGS
#     Because CFLAGS means "C flags", COBFLAGS means "COBOL flags".


libgcobol_posix_la_LINK = $(CXXLINK) $(libgcobol_posix_la_LDFLAGS)
libgcobol_posix_la_LDFLAGS = $(LTLDFLAGS) $(LIBQUADLIB) $(LTLIBICONV) \
	$(extra_ldflags_libgcobol) $(LIBS) $(LIBXML2_LIBS) $(version_arg)
libgcobol_posix_la_DEPENDENCIES = libgcobol.spec $(LIBQUADLIB_DEP) \
	$(LP_COPYBOOKS)

libgcobol_compat_gnu_la_LINK = $(CXXLINK) $(libgcobol_compat_gnu_la_LDFLAGS)
libgcobol_compat_gnu_la_LDFLAGS = $(LTLDFLAGS) $(LIBQUADLIB) $(LTLIBICONV) \
	$(extra_ldflags_libgcobol) $(LIBS) $(LIBXML2_LIBS) $(version_arg)
libgcobol_compat_gnu_la_DEPENDENCIES = libgcobol.spec $(LIBQUADLIB_DEP) \
	$(LC_COPYBOOKS)


LTCOBCOMPILE = $(LIBTOOL) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(AM_COBC) $(AM_COBFLAGS)

.cbl.o:
	$(COBC) -o $@ $(COBFLAGS) -c $<

.cbl.lo:
	$(LTCOBCOMPILE) $(COBFLAGS) $(MULTIFLAGS) -c -o $@ $<



endif BUILD_LIBGCOBOL
