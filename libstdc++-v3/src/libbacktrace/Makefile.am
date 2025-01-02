# Makefile.am -- Backtrace in libstdc++ Makefile.
# Copyright (C) 2012-2025 Free Software Foundation, Inc.

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

include $(top_srcdir)/fragment.am

# Convenience library for C++23 runtime.
noinst_LTLIBRARIES = libstdc++_libbacktrace.la

ACLOCAL_AMFLAGS = -I ../.. -I ../../config

# This will be used instead of the common AM_CPPFLAGS from fragment.am
libstdc___libbacktrace_la_CPPFLAGS = \
	-I $(top_srcdir)/../include -I $(top_srcdir)/../libgcc \
	-I ../../../libgcc -I .. -I $(top_srcdir) \
	-I $(top_srcdir)/../libbacktrace \
	-I $(top_srcdir)/../libiberty \
	-include $(top_srcdir)/src/libbacktrace/backtrace-rename.h \
	$(BACKTRACE_CPPFLAGS)

WARN_FLAGS = -W -Wall -Wwrite-strings -Wmissing-format-attribute \
	     -Wcast-qual
C_WARN_FLAGS = $(WARN_FLAGS) -Wstrict-prototypes -Wmissing-prototypes -Wold-style-definition -Wno-unused-but-set-variable
CXX_WARN_FLAGS = $(WARN_FLAGS) -Wno-unused-parameter
AM_CFLAGS = \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(C_WARN_FLAGS) -funwind-tables
AM_CFLAGS += $(EXTRA_CFLAGS)
AM_CXXFLAGS = \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(CXX_WARN_FLAGS) -fno-rtti -fno-exceptions
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)

obj_prefix = std_stacktrace

# Each FILE.c in SOURCES will be compiled to SHORTNAME-FILE.o
libstdc___libbacktrace_la_SHORTNAME = $(obj_prefix)

libstdc___libbacktrace_la_SOURCES = \
	atomic.c \
	backtrace.c \
	dwarf.c \
	fileline.c \
	posix.c \
	sort.c \
	simple.c \
	state.c \
	cp-demangle.c

FORMAT_FILES = \
	elf.c \
	pecoff.c \
	unknown.c

VIEW_FILES = \
	read.c \
	mmapio.c

ALLOC_FILES = \
	alloc.c \
	mmap.c

EXTRA_libstdc___libbacktrace_la_SOURCES = \
	$(FORMAT_FILES) \
	$(VIEW_FILES) \
	$(ALLOC_FILES)

# These three files are chosen by configure and added to the link.
# We need the SHORTNAME- prefix so that they use the custom CPPFLAGS above.
libstdc___libbacktrace_la_LIBADD = \
	$(obj_prefix)-$(FORMAT_FILE) \
	$(obj_prefix)-$(VIEW_FILE) \
	$(obj_prefix)-$(ALLOC_FILE)

libstdc___libbacktrace_la_DEPENDENCIES = $(libstdc___libbacktrace_la_LIBADD)

# Use symlinks for the sources

%.c: ../../../libbacktrace/%.c
	$(LN_S) $< $@

cp-demangle.c: ../../../libiberty/cp-demangle.c
	$(LN_S) $< $@

LTCOMPILE = \
	$(LIBTOOL) --tag CC --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=compile $(CC) $(TOPLEVEL_INCLUDES) \
	$(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CFLAGS) $(CFLAGS) $(EXTRA_CFLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

LINK = \
	$(LIBTOOL) --tag CC --tag disable-shared \
	$(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(CC) \
	$(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CFLAGS) $(LTLDFLAGS) -o $@
