# Makefile.am -- Backtrace in libsanitizer Makefile.
# Copyright (C) 2012-2013 Free Software Foundation, Inc.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

#     (1) Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer. 

#     (2) Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.  

#     (3) The name of the author may not be used to
#     endorse or promote products derived from this software without
#     specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
# IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

ACLOCAL_AMFLAGS = -I ../.. -I ../../config

AM_CPPFLAGS = -I $(top_srcdir)/../include -I $(top_srcdir)/../libgcc \
	-I ../../libgcc -I .. -I $(top_srcdir) -I $(top_srcdir)/../libbacktrace

WARN_FLAGS = -W -Wall -Wwrite-strings -Wmissing-format-attribute \
	     -Wcast-qual -Werror
C_WARN_FLAGS = $(WARN_FLAGS) -Wstrict-prototypes -Wmissing-prototypes -Wold-style-definition
CXX_WARN_FLAGS = $(WARN_FLAGS) -Wno-unused-parameter
AM_CFLAGS = $(C_WARN_FLAGS)
AM_CFLAGS += $(EXTRA_CFLAGS)
AM_CXXFLAGS = $(CXX_WARN_FLAGS) -fno-rtti -fno-exceptions
AM_CXXFLAGS += -std=gnu++11
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)

noinst_LTLIBRARIES = libsanitizer_libbacktrace.la

libsanitizer_libbacktrace_la_SOURCES = \
	../../libbacktrace/backtrace.h \
	../../libbacktrace/atomic.c \
	../../libbacktrace/dwarf.c \
	../../libbacktrace/fileline.c \
	../../libbacktrace/internal.h \
	../../libbacktrace/posix.c \
	../../libbacktrace/sort.c \
	../../libbacktrace/state.c \
	../../libiberty/cp-demangle.c \
	bridge.cc

FORMAT_FILES = \
	../../libbacktrace/elf.c \
	../../libbacktrace/unknown.c

VIEW_FILES = \
	../../libbacktrace/read.c \
	../../libbacktrace/mmapio.c

ALLOC_FILES = \
	../../libbacktrace/alloc.c \
	../../libbacktrace/mmap.c

EXTRA_libsanitizer_libbacktrace_la_SOURCES = \
	$(FORMAT_FILES) \
	$(VIEW_FILES) \
	$(ALLOC_FILES)

libsanitizer_libbacktrace_la_LIBADD = \
	$(FORMAT_FILE) \
	$(VIEW_FILE) \
	$(ALLOC_FILE)

libsanitizer_libbacktrace_la_DEPENDENCIES = $(libsanitizer_libbacktrace_la_LIBADD)
