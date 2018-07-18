# Makefile.am -- libhsail-rt library Makefile.

# Starting point copied from libcilkrts:
#  @copyright
#  Copyright (C) 2011, 2013, Intel Corporation
#  All rights reserved.
#
#  @copyright
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#    * Neither the name of Intel Corporation nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
#  @copyright
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
#  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
#  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
#  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.

# libhsail-rt modifications:
# Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
# for General Processor Tech.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Process this file with autoreconf to produce Makefile.in.

AUTOMAKE_OPTIONS = foreign subdir-objects

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

MAINT_CHARSET = latin1

mkinstalldirs = $(SHELL) $(toplevel_srcdir)/mkinstalldirs

ACLOCAL_AMFLAGS = -I .. -I ../config

WARN_CFLAGS = $(WARN_FLAGS) $(WERROR)

# -I/-D flags to pass when compiling.
AM_CPPFLAGS = -I$(srcdir)/rt -I$(srcdir)/include/internal

AM_CFLAGS = \
	-I $(srcdir)/../include \
	-I $(srcdir)/../libgcc \
	-I $(MULTIBUILDTOP)../../gcc/include $(PTH_CFLAGS)

toolexeclib_LTLIBRARIES = libhsail-rt.la

runtime_files = \
	rt/arithmetic.c \
	rt/atomics.c \
	rt/bitstring.c \
	rt/fbarrier.c \
	rt/fp16.c \
	rt/misc.c \
	rt/multimedia.c \
	rt/queue.c \
	rt/sat_arithmetic.c \
	rt/segment.c \
	rt/workitems.c \
	rt/fibers.c

libhsail_rt_la_SOURCES = $(runtime_files)
libhsail_rt_la_LDFLAGS  = -rpath '$(libdir)'

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

MAKEOVERRIDES=
