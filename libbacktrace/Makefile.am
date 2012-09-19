# Makefile.am -- Backtrace Makefile.
# Copyright (C) 2012 Free Software Foundation, Inc.

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

ACLOCAL_AMFLAGS = -I .. -I ../config

AM_CPPFLAGS = -I $(top_srcdir)/../include -I $(top_srcdir)/../libgcc \
	-I ../libgcc -I ../gcc/include -I $(MULTIBUILDTOP)../../gcc/include

AM_CFLAGS = $(WARN_FLAGS) $(PIC_FLAG)

noinst_LTLIBRARIES = libbacktrace.la

libbacktrace_la_SOURCES = \
	backtrace.h \
	dwarf.c \
	fileline.c \
	internal.h \
	posix.c \
	print.c \
	state.c

BACKTRACE_FILES = \
	backtrace.c \
	simple.c \
	nounwind.c

FORMAT_FILES = \
	elf.c \
	unknown.c

VIEW_FILES = \
	read.c \
	mmapio.c

ALLOC_FILES = \
	alloc.c \
	mmap.c

EXTRA_libbacktrace_la_SOURCES = \
	$(BACKTRACE_FILES) \
	$(FORMAT_FILES) \
	$(VIEW_FILES) \
	$(ALLOC_FILES)

libbacktrace_la_LIBADD = \
	$(BACKTRACE_FILE) \
	$(FORMAT_FILE) \
	$(VIEW_FILE) \
	$(ALLOC_FILE)

libbacktrace_la_DEPENDENCIES = $(libbacktrace_la_LIBADD)

# Testsuite.

check_PROGRAMS =

TESTS = $(check_PROGRAMS)

if NATIVE

btest_SOURCES = btest.c
btest_CFLAGS = $(AM_CFLAGS) -g -O
btest_LDADD = libbacktrace.la

check_PROGRAMS += btest

endif NATIVE
