## Copyright (C) 2014-2021 Free Software Foundation, Inc.

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
gcc_build_dir = ../gcc
AM_CPPFLAGS = -I $(srcdir)/../include -I $(srcdir)/../libgcc \
	-I $(gcc_build_dir) -I$(srcdir)/../gcc $($@_CPPFLAGS) $(GMPINC) \
	-I $(srcdir)/../gcc/c-family -I $(srcdir)/../libcpp/include
CPPFLAGS_FOR_C = -I $(srcdir)/../gcc/c
CPPFLAGS_FOR_CXX = -I $(srcdir)/../gcc/cp
AM_CXXFLAGS = $(WARN_FLAGS) $(WERROR) $(visibility) $(CET_HOST_FLAGS)
if DARWIN_DYNAMIC_LOOKUP
AM_CXXFLAGS += -Wl,-undefined,dynamic_lookup
endif
override CXXFLAGS := $(filter-out -fsanitize=address,$(CXXFLAGS))
override LDFLAGS := $(filter-out -fsanitize=address,$(LDFLAGS))
# Can be simplified when libiberty becomes a normal convenience library.
libiberty_normal = ../libiberty/libiberty.a
libiberty_noasan = ../libiberty/noasan/libiberty.a
libiberty_pic = ../libiberty/pic/libiberty.a
Wc=-Wc,
libiberty = $(if $(wildcard $(libiberty_noasan)),$(Wc)$(libiberty_noasan), \
	    $(if $(wildcard $(libiberty_pic)),$(Wc)$(libiberty_pic), \
	    $(Wc)$(libiberty_normal)))
libiberty_dep = $(patsubst $(Wc)%,%,$(libiberty))

plugindir = $(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/plugin
cc1libdir = $(libdir)/$(libsuffix)

if ENABLE_PLUGIN
plugin_LTLIBRARIES = libcc1plugin.la libcp1plugin.la
cc1lib_LTLIBRARIES = libcc1.la
endif

shared_source = callbacks.cc callbacks.hh connection.cc connection.hh \
    marshall.cc marshall.hh rpc.hh status.hh

marshall_c_source = marshall-c.hh
marshall_cxx_source = marshall-cp.hh

libcc1plugin_la_LDFLAGS = -module -export-symbols $(srcdir)/libcc1plugin.sym
libcc1plugin_la_SOURCES = libcc1plugin.cc context.cc context.hh \
	$(shared_source) $(marshall_c_source)
libcc1plugin.lo_CPPFLAGS = $(CPPFLAGS_FOR_C)
libcc1plugin_la_LIBADD = $(libiberty)
libcc1plugin_la_DEPENDENCIES = $(libiberty_dep)
libcc1plugin_la_LINK = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CXXLD) $(AM_CXXFLAGS) \
	$(CXXFLAGS) $(libcc1plugin_la_LDFLAGS) $(LTLDFLAGS) -o $@

libcp1plugin_la_LDFLAGS = -module -export-symbols $(srcdir)/libcp1plugin.sym
libcp1plugin_la_SOURCES = libcp1plugin.cc context.cc context.hh \
	$(shared_source) $(marshall_cxx_source)
libcp1plugin.lo_CPPFLAGS = $(CPPFLAGS_FOR_CXX)
libcp1plugin_la_LIBADD = $(libiberty)
libcp1plugin_la_DEPENDENCIES = $(libiberty_dep)
libcp1plugin_la_LINK = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CXXLD) $(AM_CXXFLAGS) \
	$(CXXFLAGS) $(libcp1plugin_la_LDFLAGS) $(LTLDFLAGS) -o $@

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))
libcc1_la_LDFLAGS = -module -export-symbols $(srcdir)/libcc1.sym
libcc1_la_SOURCES = findcomp.cc libcc1.cc libcp1.cc \
		compiler.cc compiler.hh names.cc names.hh $(shared_source) \
		$(marshall_c_source) $(marshall_cxx_source)
libcc1_la_LIBADD = $(libiberty)
libcc1_la_DEPENDENCIES = $(libiberty_dep)
libcc1_la_LINK = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CXXLD) $(AM_CXXFLAGS) \
	$(CXXFLAGS) $(libcc1_la_LDFLAGS) $(LTLDFLAGS) -o $@
