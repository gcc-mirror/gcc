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

AUTOMAKE_OPTIONS = foreign

# Use when building GCC
ACLOCAL_AMFLAGS = -I .. -I ../config

# Compiler and linker flags.
GENERAL_FLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/runtime -I$(top_srcdir)/runtime/config/$(config_dir) -DIN_CILK_RUNTIME=1
GENERAL_FLAGS += -D_Cilk_spawn="" -D_Cilk_sync="" -D_Cilk_for=for

# Enable Intel Cilk Plus extension
GENERAL_FLAGS += -fcilkplus

AM_CFLAGS = $(GENERAL_FLAGS) -std=c99
AM_CPPFLAGS = $(GENERAL_FLAGS)
AM_LDFLAGS = -lpthread 

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

# Target list.
toolexeclib_LTLIBRARIES = libcilkrts.la

libcilkrts_la_SOURCES =            \
  runtime/config/$(config_dir)/cilk-abi-vla.c           \
  runtime/config/$(config_dir)/os-unix-sysdep.c \
  runtime/bug.cpp                  \
  runtime/cilk-abi.c               \
  runtime/cilk-abi-cilk-for.cpp    \
  runtime/cilk-abi-vla-internal.c  \
  runtime/cilk_api.c               \
  runtime/cilk_fiber.cpp           \
  runtime/cilk_fiber-unix.cpp      \
  runtime/cilk_malloc.c            \
  runtime/c_reducers.c             \
  runtime/except-gcc.cpp           \
  runtime/frame_malloc.c           \
  runtime/full_frame.c             \
  runtime/global_state.cpp         \
  runtime/jmpbuf.c                 \
  runtime/local_state.c            \
  runtime/metacall_impl.c          \
  runtime/os_mutex-unix.c          \
  runtime/os-unix.c                \
  runtime/pedigrees.c              \
  runtime/record-replay.cpp        \
  runtime/reducer_impl.cpp         \
  runtime/scheduler.c              \
  runtime/signal_node.c            \
  runtime/spin_mutex.c             \
  runtime/stats.c                  \
  runtime/symbol_test.c            \
  runtime/sysdep-unix.c            \
  runtime/worker_mutex.c


# Load the $(REVISION) value.
include include/internal/rev.mk

#libcilkrts_la_LDFLAGS  = -rpath '$(libdir)'
libcilkrts_la_LDFLAGS = -version-info 5:0:0
libcilkrts_la_LDFLAGS += -lpthread -ldl

# If we're building on Linux, use the Linux version script
if LINUX_LINKER_SCRIPT
    libcilkrts_la_LDFLAGS += -Wl,--version-script,$(srcdir)/runtime/linux-symbols.ver
endif

# If we're building on MacOS, use the Mac versioning
if MAC_LINKER_SCRIPT
  libcilkrts_la_LDFLAGS += -Wl,-exported_symbols_list,$(srcdir)/runtime/mac-symbols.txt
endif


# Hack for Cygwin
libcilkrts_la_LDFLAGS += -no-undefined

# C/C++ header files for Cilk.
cilkincludedir = $(includedir)/cilk
cilkinclude_HEADERS =              \
  include/cilk/cilk_api.h          \
  include/cilk/cilk_api_linux.h    \
  include/cilk/cilk.h              \
  include/cilk/cilk_stub.h         \
  include/cilk/cilk_undocumented.h \
  include/cilk/common.h            \
  include/cilk/holder.h            \
  include/cilk/hyperobject_base.h  \
  include/cilk/metaprogramming.h   \
  include/cilk/reducer_file.h      \
  include/cilk/reducer.h           \
  include/cilk/reducer_list.h      \
  include/cilk/reducer_max.h       \
  include/cilk/reducer_min.h       \
  include/cilk/reducer_min_max.h   \
  include/cilk/reducer_opadd.h     \
  include/cilk/reducer_opand.h     \
  include/cilk/reducer_opmul.h     \
  include/cilk/reducer_opor.h      \
  include/cilk/reducer_opxor.h     \
  include/cilk/reducer_ostream.h   \
  include/cilk/reducer_string.h


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

