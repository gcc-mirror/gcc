# Plugin for offload execution on Intel MIC devices.
#
# Copyright (C) 2014 Free Software Foundation, Inc.
#
# Contributed by Ilya Verbin <ilya.verbin@intel.com> and
# Andrey Turetskiy <andrey.turetskiy@intel.com>.
#
# This file is part of the GNU Offloading and Multi Processing Library
# (libgomp).
#
# Libgomp is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.


AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I ../.. -I ../../config

# Directories
build_dir = $(top_builddir)
source_dir = $(top_srcdir)
coi_inc_dir = $(top_srcdir)/../include/coi
include_src_dir = $(top_srcdir)/../../include
libgomp_src_dir = $(top_srcdir)/../../libgomp
libgomp_dir = $(build_dir)/../../libgomp
liboffload_src_dir = $(top_srcdir)/../runtime
liboffload_dir = $(top_builddir)/..

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../../gcc/BASE-VER)
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)$(MULTISUBDIR)/include
# Search for main_target_image.h in these directories
target_prefix_dir = $(libdir)/gcc/$(accel_target)/$(gcc_version)$(MULTISUBDIR)
target_build_dir = $(accel_search_dir)/$(accel_target)$(MULTISUBDIR)/liboffloadmic/plugin
target_install_dir = $(accel_search_dir)/lib/gcc/$(accel_target)/$(gcc_version)$(MULTISUBDIR)

if PLUGIN_HOST
  toolexeclib_LTLIBRARIES = libgomp-plugin-intelmic.la
  libgomp_plugin_intelmic_la_SOURCES = libgomp-plugin-intelmic.cpp
  libgomp_plugin_intelmic_la_CPPFLAGS = $(CPPFLAGS) -DLINUX -DCOI_LIBRARY_VERSION=2 -DOFFLOAD_DEBUG=1 -DSEP_SUPPORT -DTIMING_SUPPORT -DHOST_LIBRARY=1 -I$(coi_inc_dir) -I$(liboffload_src_dir) -I$(libgomp_src_dir) -I$(libgomp_dir) -I$(include_src_dir) -I$(target_prefix_dir)/include -I$(target_build_dir) -I$(target_install_dir)/include
  libgomp_plugin_intelmic_la_LDFLAGS = -L$(liboffload_dir)/.libs -loffloadmic_host -version-info 1:0:0
else # PLUGIN_TARGET
  plugin_includedir = $(libsubincludedir)
  plugin_include_HEADERS = main_target_image.h
  AM_CPPFLAGS = $(CPPFLAGS) -DLINUX -DCOI_LIBRARY_VERSION=2 -DOFFLOAD_DEBUG=1 -DSEP_SUPPORT -DTIMING_SUPPORT -DHOST_LIBRARY=0 -I$(coi_inc_dir) -I$(liboffload_src_dir) -I$(libgomp_dir)
  AM_CXXFLAGS = $(CXXFLAGS)
  AM_LDFLAGS = -L$(liboffload_dir)/.libs -L$(libgomp_dir)/.libs -loffloadmic_target -lcoi_device -lgomp -rdynamic
endif

main_target_image.h: offload_target_main
	@echo -n "const int image_size = " > $@
	@stat -c '%s' $< >> $@
	@echo ";" >> $@
	@echo "struct MainTargetImage {" >> $@
	@echo "  int64_t size;" >> $@
	@echo "  char name[sizeof \"offload_target_main\"];" >> $@
	@echo "  uint8_t data[image_size];" >> $@
	@echo "};" >> $@
	@echo "extern \"C\" const MainTargetImage main_target_image = {" >> $@
	@echo "  image_size, \"offload_target_main\"," >> $@
	@cat $< | xxd -include >> $@
	@echo "};" >> $@

offload_target_main: $(liboffload_dir)/ofldbegin.o offload_target_main.o $(liboffload_dir)/ofldend.o
	$(CXX) $(AM_LDFLAGS) $^ -o $@

offload_target_main.o: offload_target_main.cpp
	$(CXX) $(AM_CXXFLAGS) $(AM_CPPFLAGS) -c $< -o $@

# Work around what appears to be a GNU make bug handling MAKEFLAGS
# values defined in terms of make variables, as is the case for CC and
# friends when we are called from the top level Makefile.
AM_MAKEFLAGS = \
       "AR_FLAGS=$(AR_FLAGS)" \
       "CC_FOR_BUILD=$(CC_FOR_BUILD)" \
       "CFLAGS=$(CFLAGS)" \
       "CXXFLAGS=$(CXXFLAGS)" \
       "CFLAGS_FOR_BUILD=$(CFLAGS_FOR_BUILD)" \
       "CFLAGS_FOR_TARGET=$(CFLAGS_FOR_TARGET)" \
       "INSTALL=$(INSTALL)" \
       "INSTALL_DATA=$(INSTALL_DATA)" \
       "INSTALL_PROGRAM=$(INSTALL_PROGRAM)" \
       "INSTALL_SCRIPT=$(INSTALL_SCRIPT)" \
       "JC1FLAGS=$(JC1FLAGS)" \
       "LDFLAGS=$(LDFLAGS)" \
       "LIBCFLAGS=$(LIBCFLAGS)" \
       "LIBCFLAGS_FOR_TARGET=$(LIBCFLAGS_FOR_TARGET)" \
       "MAKE=$(MAKE)" \
       "MAKEINFO=$(MAKEINFO) $(MAKEINFOFLAGS)" \
       "PICFLAG=$(PICFLAG)" \
       "PICFLAG_FOR_TARGET=$(PICFLAG_FOR_TARGET)" \
       "SHELL=$(SHELL)" \
       "RUNTESTFLAGS=$(RUNTESTFLAGS)" \
       "exec_prefix=$(exec_prefix)" \
       "infodir=$(infodir)" \
       "libdir=$(libdir)" \
       "prefix=$(prefix)" \
       "includedir=$(includedir)" \
       "AR=$(AR)" \
       "AS=$(AS)" \
       "LD=$(LD)" \
       "LIBCFLAGS=$(LIBCFLAGS)" \
       "NM=$(NM)" \
       "PICFLAG=$(PICFLAG)" \
       "RANLIB=$(RANLIB)" \
       "DESTDIR=$(DESTDIR)"

MAKEOVERRIDES =

