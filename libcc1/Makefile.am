## Copyright (C) 2014 Free Software Foundation, Inc.

## This file is part of GCC.

## GCC is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free
## Software Foundation; either version 3, or (at your option) any later
## version.

## GCC is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.

## You should have received a copy of the GNU General Public License
## along with GCC; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

ACLOCAL_AMFLAGS = -I .. -I ../config
gcc_build_dir = ../$(host_subdir)/gcc
AM_CPPFLAGS = -I $(srcdir)/../include -I $(srcdir)/../libgcc \
	-I $(gcc_build_dir) -I$(srcdir)/../gcc \
	-I $(srcdir)/../gcc/c -I $(srcdir)/../gcc/c-family \
	-I $(srcdir)/../libcpp/include
WERROR_FLAG = -Werror
AM_CXXFLAGS = $(WARN_FLAGS) $(WERROR_FLAG) $(visibility)
libiberty = ../libiberty/pic/libiberty.a


plugindir = $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/plugin
cc1libdir = $(libdir)/$(libsuffix)

if ENABLE_PLUGIN
plugin_LTLIBRARIES = libcc1plugin.la
cc1lib_LTLIBRARIES = libcc1.la
endif

BUILT_SOURCES = compiler-name.h

# Put this in a header so we don't run sed for each compilation.  This
# is also simpler to debug as one can easily see the constant.
compiler-name.h: Makefile
	echo "#define COMPILER_NAME \"`echo gcc | sed '$(transform)'`\"" > compiler-name.h


shared_source = callbacks.cc callbacks.hh connection.cc connection.hh \
    marshall.cc marshall.hh rpc.hh status.hh

libcc1plugin_la_LDFLAGS = -module -export-symbols $(srcdir)/libcc1plugin.sym
libcc1plugin_la_SOURCES = plugin.cc $(shared_source)
libcc1plugin_la_LIBADD = $(libiberty)

libcc1_la_LDFLAGS = -module -export-symbols $(srcdir)/libcc1.sym
libcc1_la_SOURCES = findcomp.cc libcc1.cc names.cc names.hh $(shared_source)
libcc1_la_LIBADD = $(libiberty)
