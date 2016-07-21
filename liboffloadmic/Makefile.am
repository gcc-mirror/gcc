#
#    Copyright (c) 2014 Intel Corporation.  All Rights Reserved.
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions
#    are met:
#
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in the
#        documentation and/or other materials provided with the distribution.
#      * Neither the name of Intel Corporation nor the names of its
#        contributors may be used to endorse or promote products derived
#        from this software without specific prior written permission.
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#


AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I .. -I ../config

# Build plugin for Intel MIC
SUBDIRS = . plugin

# Directories.
build_dir = $(top_builddir)
coi_inc_dir = $(top_srcdir)/include/coi
libgomp_dir = $(build_dir)/../libgomp
source_dir = $(top_srcdir)/runtime

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)
libsubincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include

# Target list.
if LIBOFFLOADMIC_HOST
  nodist_toolexeclib_HEADERS = liboffloadmic_host.spec
  toolexeclib_LTLIBRARIES = libcoi_host.la liboffloadmic_host.la
  nodist_libsubinclude_HEADERS = runtime/compiler_if_host.h
  ofld_obj =
else # LIBOFFLOADMIC_TARGET
  nodist_toolexeclib_HEADERS = liboffloadmic_target.spec
  toolexeclib_LTLIBRARIES = libcoi_device.la liboffloadmic_target.la
  nodist_libsubinclude_HEADERS = runtime/compiler_if_target.h
  ofld_obj = ofldbegin.o ofldend.o
endif

# Liboffloadmic.
liboffloadmic_sources = runtime/dv_util.cpp \
			runtime/liboffload_error.c \
			runtime/liboffload_msg.c \
			runtime/offload_common.cpp \
			runtime/offload_table.cpp \
			runtime/offload_trace.cpp \
			runtime/offload_util.cpp

liboffloadmic_cppflags = -DLINUX -DCOI_LIBRARY_VERSION=2 -DOFFLOAD_DEBUG=1 -DSEP_SUPPORT -DTIMING_SUPPORT -I$(coi_inc_dir) -I$(source_dir) -I$(libgomp_dir)

liboffloadmic_host_la_SOURCES = $(liboffloadmic_sources) \
				runtime/cean_util.cpp \
				runtime/coi/coi_client.cpp \
				runtime/compiler_if_host.cpp \
				runtime/offload_engine.cpp \
				runtime/offload_env.cpp \
				runtime/offload_host.cpp \
				runtime/offload_omp_host.cpp \
				runtime/offload_orsl.cpp \
				runtime/offload_timer_host.cpp \
				runtime/orsl-lite/lib/orsl-lite.c

liboffloadmic_host_la_CPPFLAGS = $(liboffloadmic_cppflags) -DHOST_LIBRARY=1
liboffloadmic_host_la_LDFLAGS = @lt_cv_dlopen_libs@ -version-info 5:0:0

liboffloadmic_target_la_SOURCES = $(liboffloadmic_sources) \
				  runtime/coi/coi_server.cpp \
				  runtime/compiler_if_target.cpp \
				  runtime/offload_omp_target.cpp \
				  runtime/offload_target.cpp \
				  runtime/offload_timer_target.cpp

liboffloadmic_target_la_CPPFLAGS = $(liboffloadmic_cppflags) -DHOST_LIBRARY=0
liboffloadmic_target_la_LDFLAGS = @lt_cv_dlopen_libs@ -version-info 5:0:0
liboffloadmic_target_la_LIBADD = libcoi_device.la
liboffloadmic_target_la_DEPENDENCIES = $(liboffloadmic_target_la_LIBADD)

# Emulator.
libcoi_host_la_SOURCES = runtime/emulator/coi_host.cpp
libcoi_device_la_SOURCES = runtime/emulator/coi_device.cpp

libcoi_host_la_DEPENDENCIES = runtime/emulator/coi_version_linker_script.map
libcoi_device_la_DEPENDENCIES = runtime/emulator/coi_version_linker_script.map

libcoi_host_la_CPPFLAGS = -I$(coi_inc_dir)
libcoi_device_la_CPPFLAGS = -I$(coi_inc_dir)

libcoi_host_la_LDFLAGS = -lrt -Wl,--version-script=$(source_dir)/emulator/coi_version_linker_script.map
libcoi_device_la_LDFLAGS = -lrt -Wl,--version-script=$(source_dir)/emulator/coi_version_linker_script.map

# ofldbegin, ofldend
all-local: $(ofld_obj)

ofldbegin.o: runtime/ofldbegin.cpp
	$(CXXCOMPILE) $(liboffloadmic_target_la_CPPFLAGS) -c $< -o $@

ofldend.o: runtime/ofldend.cpp
	$(CXXCOMPILE) $(liboffloadmic_target_la_CPPFLAGS) -c $< -o $@

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

