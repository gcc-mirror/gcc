# Makefile.am -- Backtrace Makefile.
# Copyright (C) 2012-2019 Free Software Foundation, Inc.

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
	-I ../libgcc

AM_CFLAGS = $(EXTRA_FLAGS) $(WARN_FLAGS) $(PIC_FLAG)

noinst_LTLIBRARIES = libbacktrace.la

libbacktrace_la_SOURCES = \
	backtrace.h \
	atomic.c \
	dwarf.c \
	fileline.c \
	internal.h \
	posix.c \
	print.c \
	sort.c \
	state.c

BACKTRACE_FILES = \
	backtrace.c \
	simple.c \
	nounwind.c

FORMAT_FILES = \
	elf.c \
	pecoff.c \
	unknown.c \
	xcoff.c

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
check_LTLIBRARIES = libbacktrace_alloc.la

libbacktrace_alloc_la_SOURCES = $(libbacktrace_la_SOURCES)
libbacktrace_alloc_la_LIBADD = $(BACKTRACE_FILE) $(FORMAT_FILE) read.lo alloc.lo

libbacktrace_alloc_la_DEPENDENCIES = $(libbacktrace_alloc_la_LIBADD)

check_LTLIBRARIES += libbacktrace_noformat.la

libbacktrace_noformat_la_SOURCES = $(libbacktrace_la_SOURCES)
libbacktrace_noformat_la_LIBADD = $(BACKTRACE_FILE) $(VIEW_FILE) $(ALLOC_FILE)

libbacktrace_noformat_la_DEPENDENCIES = $(libbacktrace_noformat_la_LIBADD)

xcoff_%.c: xcoff.c
	SEARCH='#error "Unknown BACKTRACE_XCOFF_SIZE"'; \
	REPLACE='#undef BACKTRACE_XCOFF_SIZE\
	#define BACKTRACE_XCOFF_SIZE'; \
	$(SED) "s/^$$SEARCH\$$/$$REPLACE $*/" \
		$(srcdir)/xcoff.c \
		> $@

test_elf_SOURCES = test_format.c testlib.c
test_elf_LDADD = libbacktrace_noformat.la elf.lo

check_PROGRAMS += test_elf

test_xcoff_32_SOURCES = test_format.c testlib.c
test_xcoff_32_LDADD = libbacktrace_noformat.la xcoff_32.lo

check_PROGRAMS += test_xcoff_32

test_xcoff_64_SOURCES = test_format.c testlib.c
test_xcoff_64_LDADD = libbacktrace_noformat.la xcoff_64.lo

check_PROGRAMS += test_xcoff_64

test_pecoff_SOURCES = test_format.c testlib.c
test_pecoff_LDADD = libbacktrace_noformat.la pecoff.lo

check_PROGRAMS += test_pecoff

test_unknown_SOURCES = test_format.c testlib.c
test_unknown_LDADD = libbacktrace_noformat.la unknown.lo

check_PROGRAMS += test_unknown

unittest_SOURCES = unittest.c testlib.c
unittest_LDADD = libbacktrace.la

check_PROGRAMS += unittest

unittest_alloc_SOURCES = $(unittest_SOURCES)
unittest_alloc_LDADD = libbacktrace_alloc.la

check_PROGRAMS += unittest_alloc

check_LTLIBRARIES += libbacktrace_instrumented_alloc.la

libbacktrace_instrumented_alloc_la_SOURCES = $(libbacktrace_la_SOURCES)
libbacktrace_instrumented_alloc_la_LIBADD = $(BACKTRACE_FILE) $(FORMAT_FILE) \
	read.lo instrumented_alloc.lo

libbacktrace_instrumented_alloc_la_DEPENDENCIES = \
	$(libbacktrace_instrumented_alloc_la_LIBADD)

instrumented_alloc.lo: alloc.c

allocfail_SOURCES = allocfail.c testlib.c
allocfail_LDADD = libbacktrace_instrumented_alloc.la

check_PROGRAMS += allocfail

allocfail.sh: allocfail

TESTS += allocfail.sh

btest_SOURCES = btest.c testlib.c
btest_CFLAGS = $(AM_CFLAGS) -g -O
btest_LDADD = libbacktrace.la

check_PROGRAMS += btest

btest_alloc_SOURCES = $(btest_SOURCES)
btest_alloc_CFLAGS = $(btest_CFLAGS)
btest_alloc_LDADD = libbacktrace_alloc.la

check_PROGRAMS += btest_alloc

stest_SOURCES = stest.c
stest_LDADD = libbacktrace.la

check_PROGRAMS += stest

stest_alloc_SOURCES = $(stest_SOURCES)
stest_alloc_LDADD = libbacktrace_alloc.la

check_PROGRAMS += stest_alloc

ztest_SOURCES = ztest.c testlib.c
ztest_CFLAGS = -DSRCDIR=\"$(srcdir)\"
ztest_LDADD = libbacktrace.la
ztest_alloc_LDADD = libbacktrace_alloc.la

if HAVE_ZLIB
ztest_LDADD += -lz
ztest_alloc_LDADD += -lz
endif
ztest_LDADD += $(CLOCK_GETTIME_LINK)
ztest_alloc_LDADD += $(CLOCK_GETTIME_LINK)

check_PROGRAMS += ztest

ztest_alloc_SOURCES = $(ztest_SOURCES)
ztest_alloc_CFLAGS = $(ztest_CFLAGS)

check_PROGRAMS += ztest_alloc

edtest_SOURCES = edtest.c edtest2_build.c testlib.c
edtest_LDADD = libbacktrace.la

check_PROGRAMS += edtest

edtest_alloc_SOURCES = $(edtest_SOURCES)
edtest_alloc_LDADD = libbacktrace_alloc.la

check_PROGRAMS += edtest_alloc

edtest2_build.c: gen_edtest2_build; @true
gen_edtest2_build: $(srcdir)/edtest2.c
	cat $(srcdir)/edtest2.c > tmp-edtest2_build.c
	$(SHELL) $(srcdir)/../move-if-change tmp-edtest2_build.c edtest2_build.c
	echo timestamp > $@

if HAVE_PTHREAD

check_PROGRAMS += ttest

ttest_SOURCES = ttest.c testlib.c
ttest_CFLAGS = $(AM_CFLAGS) -pthread
ttest_LDADD = libbacktrace.la

check_PROGRAMS += ttest_alloc

ttest_alloc_SOURCES = $(ttest_SOURCES)
ttest_alloc_CFLAGS = $(ttest_CFLAGS)
ttest_alloc_LDADD = libbacktrace_alloc.la

endif HAVE_PTHREAD

if HAVE_OBJCOPY_DEBUGLINK

TESTS += dtest

dtest: btest
	$(OBJCOPY) --only-keep-debug btest btest.debug
	$(OBJCOPY) --strip-debug --add-gnu-debuglink=btest.debug btest dtest

endif HAVE_OBJCOPY_DEBUGLINK

if HAVE_COMPRESSED_DEBUG

ctestg_SOURCES = btest.c testlib.c
ctestg_CFLAGS = $(AM_CFLAGS) -g
ctestg_LDFLAGS = -Wl,--compress-debug-sections=zlib-gnu
ctestg_LDADD = libbacktrace.la

ctesta_SOURCES = btest.c testlib.c
ctesta_CFLAGS = $(AM_CFLAGS) -g
ctesta_LDFLAGS = -Wl,--compress-debug-sections=zlib-gabi
ctesta_LDADD = libbacktrace.la

check_PROGRAMS += ctestg ctesta

ctestg_alloc_SOURCES = $(ctestg_SOURCES)
ctestg_alloc_CFLAGS = $(ctestg_CFLAGS)
ctestg_alloc_LDFLAGS = $(ctestg_LDFLAGS)
ctestg_alloc_LDADD = libbacktrace_alloc.la

ctesta_alloc_SOURCES = $(ctesta_SOURCES)
ctesta_alloc_CFLAGS = $(ctesta_CFLAGS)
ctesta_alloc_LDFLAGS = $(ctesta_LDFLAGS)
ctesta_alloc_LDADD = libbacktrace_alloc.la

check_PROGRAMS += ctestg_alloc ctesta_alloc

endif

endif NATIVE

# We can't use automake's automatic dependency tracking, because it
# breaks when using bootstrap-lean.  Automatic dependency tracking
# with GCC bootstrap will cause some of the objects to depend on
# header files in prev-gcc/include, e.g., stddef.h and stdarg.h.  When
# using bootstrap-lean, prev-gcc is removed after each stage.  When
# running "make install", those header files will be gone, causing the
# library to be rebuilt at install time.  That may not succeed.

# These manual dependencies do not include dependencies on unwind.h,
# even though that is part of GCC, because where to find it depends on
# whether we are being built as a host library or a target library.

INCDIR = $(top_srcdir)/../include
alloc.lo: config.h backtrace.h internal.h
backtrace.lo: config.h backtrace.h internal.h
btest.lo: (INCDIR)/filenames.h backtrace.h backtrace-supported.h
dwarf.lo: config.h $(INCDIR)/dwarf2.h $(INCDIR)/dwarf2.def \
	$(INCDIR)/filenames.h backtrace.h internal.h
elf.lo: config.h backtrace.h internal.h
fileline.lo: config.h backtrace.h internal.h
mmap.lo: config.h backtrace.h internal.h
mmapio.lo: config.h backtrace.h internal.h
nounwind.lo: config.h internal.h
pecoff.lo: config.h backtrace.h internal.h
posix.lo: config.h backtrace.h internal.h
print.lo: config.h backtrace.h internal.h
read.lo: config.h backtrace.h internal.h
simple.lo: config.h backtrace.h internal.h
sort.lo: config.h backtrace.h internal.h
stest.lo: config.h backtrace.h internal.h
state.lo: config.h backtrace.h backtrace-supported.h internal.h
unknown.lo: config.h backtrace.h internal.h
xcoff.lo: config.h backtrace.h internal.h

include $(top_srcdir)/../multilib.am
