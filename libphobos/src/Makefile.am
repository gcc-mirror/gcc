## Makefile for the Phobos standard library.
## Copyright (C) 2012-2019 Free Software Foundation, Inc.
##
## GCC is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## GCC is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GCC; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

# Include D build rules
include $(top_srcdir)/d_rules.am

# Make sure GDC can find libdruntime and libphobos include files
D_EXTRA_DFLAGS=-nostdinc -I $(srcdir) \
	-I $(top_srcdir)/libdruntime -I ../libdruntime -I .

# C flags for zlib compilation
AM_CFLAGS=@DEFS@ -I. -I$(srcdir)/../libdruntime/gcc -I$(top_srcdir)/../zlib

# D flags for compilation
AM_DFLAGS=$(phobos_compiler_pic_flag)

# Install all D files
ALL_PHOBOS_INSTALL_DSOURCES = $(PHOBOS_DSOURCES)

# Setup source files depending on configure
ALL_PHOBOS_COMPILE_DSOURCES = $(PHOBOS_DSOURCES)

ALL_PHOBOS_SOURCES = $(ALL_PHOBOS_COMPILE_DSOURCES)
PHOBOS_TEST_LOBJECTS = $(ALL_PHOBOS_COMPILE_DSOURCES:.d=.t.lo)
PHOBOS_TEST_OBJECTS = $(ALL_PHOBOS_COMPILE_DSOURCES:.d=.t.o)

# Main library build definitions
if DRUNTIME_ZLIB_SYSTEM
    ZLIB_SRC =
else
    ZLIB_SRC = $(ZLIB_CSOURCES)
endif
check_PROGRAMS =
if ENABLE_SHARED
    check_LTLIBRARIES = libgphobos_t.la
    check_PROGRAMS += unittest
endif
if ENABLE_STATIC
    check_PROGRAMS += unittest_static
endif

toolexeclib_DATA = libgphobos.spec
toolexeclib_LTLIBRARIES = libgphobos.la
libgphobos_la_SOURCES = $(ALL_PHOBOS_SOURCES) $(ZLIB_SRC)
libgphobos_la_LIBTOOLFLAGS =
libgphobos_la_LDFLAGS = -Wc,-nophoboslib,-dstartfiles,-B../libdruntime/gcc \
    -version-info $(libtool_VERSION)
libgphobos_la_LIBADD = ../libdruntime/libgdruntime.la
libgphobos_la_DEPENDENCIES = libgphobos.spec

# For static unittest, link objects directly
unittest_static_SOURCES = ../testsuite/test_runner.d $(ZLIB_SRC)
unittest_static_LIBTOOLFLAGS =
unittest_static_LDFLAGS = -Wc,-nophoboslib -static-libtool-libs
unittest_static_LDADD = $(PHOBOS_TEST_OBJECTS) \
    ../libdruntime/libgdruntime.la
EXTRA_unittest_static_DEPENDENCIES = $(PHOBOS_TEST_OBJECTS)

# For unittest with dynamic library
libgphobos_t_la_SOURCES = $(ZLIB_SRC)
libgphobos_t_la_LIBTOOLFLAGS =
libgphobos_t_la_LDFLAGS = -Wc,-nophoboslib -rpath /foo -shared
libgphobos_t_la_LIBADD = $(PHOBOS_TEST_LOBJECTS) \
    ../libdruntime/libgdruntime.la
EXTRA_libgphobos_t_la_DEPENDENCIES = $(PHOBOS_TEST_LOBJECTS)

# For unittest
unittest_SOURCES = ../testsuite/test_runner.d
unittest_LIBTOOLFLAGS =
unittest_LDFLAGS = -Wc,-nophoboslib -shared
unittest_LDADD = libgphobos_t.la ../libdruntime/libgdruntime.la

# Extra install and clean rules.
# This does not delete the .libs/.t.o files, but deleting
# the .lo is good enough to rerun the rules
clean-local:
	rm -f $(PHOBOS_TEST_LOBJECTS)
	rm -f $(PHOBOS_TEST_OBJECTS)

# Handles generated files as well
install-data-local:
	for file in $(ALL_PHOBOS_INSTALL_DSOURCES); do \
	  $(MKDIR_P) `dirname $(DESTDIR)$(gdc_include_dir)/$$file` ; \
	  if test -f $$file; then \
	    $(INSTALL_HEADER) $$file $(DESTDIR)$(gdc_include_dir)/$$file ; \
	  else \
	    $(INSTALL_HEADER) $(srcdir)/$$file \
	      $(DESTDIR)$(gdc_include_dir)/$$file ; \
	  fi ; \
	done

# Zlib sources when not using system libz
ZLIB_CSOURCES=$(top_srcdir)/../zlib/adler32.c $(top_srcdir)/../zlib/compress.c \
	$(top_srcdir)/../zlib/crc32.c $(top_srcdir)/../zlib/deflate.c \
	$(top_srcdir)/../zlib/gzclose.c $(top_srcdir)/../zlib/gzlib.c \
	$(top_srcdir)/../zlib/gzread.c $(top_srcdir)/../zlib/gzwrite.c \
	$(top_srcdir)/../zlib/infback.c $(top_srcdir)/../zlib/inffast.c \
	$(top_srcdir)/../zlib/inflate.c $(top_srcdir)/../zlib/inftrees.c \
	$(top_srcdir)/../zlib/trees.c $(top_srcdir)/../zlib/uncompr.c \
	$(top_srcdir)/../zlib/zutil.c

# Source file definitions. Boring stuff, auto-generated with
# https://gist.github.com/jpf91/8744acebc9dcf1e9d1a35cdff20afbb2
# Can't use wildcards here:
# https://www.gnu.org/software/automake/manual/html_node/Wildcards.html
PHOBOS_DSOURCES = etc/c/curl.d etc/c/sqlite3.d etc/c/zlib.d \
	std/algorithm/comparison.d std/algorithm/internal.d \
	std/algorithm/iteration.d std/algorithm/mutation.d \
	std/algorithm/package.d std/algorithm/searching.d \
	std/algorithm/setops.d std/algorithm/sorting.d std/array.d std/ascii.d \
	std/base64.d std/bigint.d std/bitmanip.d std/compiler.d std/complex.d \
	std/concurrency.d std/container/array.d std/container/binaryheap.d \
	std/container/dlist.d std/container/package.d std/container/rbtree.d \
	std/container/slist.d std/container/util.d std/conv.d std/csv.d \
	std/datetime/date.d std/datetime/interval.d std/datetime/package.d \
	std/datetime/stopwatch.d std/datetime/systime.d \
	std/datetime/timezone.d std/demangle.d std/digest/crc.d \
	std/digest/digest.d std/digest/hmac.d std/digest/md.d \
	std/digest/murmurhash.d std/digest/package.d std/digest/ripemd.d \
	std/digest/sha.d std/encoding.d std/exception.d \
	std/experimental/allocator/building_blocks/affix_allocator.d \
	std/experimental/allocator/building_blocks/allocator_list.d \
	std/experimental/allocator/building_blocks/bitmapped_block.d \
	std/experimental/allocator/building_blocks/bucketizer.d \
	std/experimental/allocator/building_blocks/fallback_allocator.d \
	std/experimental/allocator/building_blocks/free_list.d \
	std/experimental/allocator/building_blocks/free_tree.d \
	std/experimental/allocator/building_blocks/kernighan_ritchie.d \
	std/experimental/allocator/building_blocks/null_allocator.d \
	std/experimental/allocator/building_blocks/package.d \
	std/experimental/allocator/building_blocks/quantizer.d \
	std/experimental/allocator/building_blocks/region.d \
	std/experimental/allocator/building_blocks/scoped_allocator.d \
	std/experimental/allocator/building_blocks/segregator.d \
	std/experimental/allocator/building_blocks/stats_collector.d \
	std/experimental/allocator/common.d \
	std/experimental/allocator/gc_allocator.d \
	std/experimental/allocator/mallocator.d \
	std/experimental/allocator/mmap_allocator.d \
	std/experimental/allocator/package.d \
	std/experimental/allocator/showcase.d \
	std/experimental/allocator/typed.d std/experimental/checkedint.d \
	std/experimental/logger/core.d std/experimental/logger/filelogger.d \
	std/experimental/logger/multilogger.d \
	std/experimental/logger/nulllogger.d std/experimental/logger/package.d \
	std/experimental/typecons.d std/file.d std/format.d std/functional.d \
	std/getopt.d std/internal/cstring.d std/internal/math/biguintcore.d \
	std/internal/math/biguintnoasm.d std/internal/math/errorfunction.d \
	std/internal/math/gammafunction.d std/internal/scopebuffer.d \
	std/internal/test/dummyrange.d std/internal/test/range.d \
	std/internal/test/uda.d std/internal/unicode_comp.d \
	std/internal/unicode_decomp.d std/internal/unicode_grapheme.d \
	std/internal/unicode_norm.d std/internal/unicode_tables.d \
	std/internal/windows/advapi32.d std/json.d std/math.d \
	std/mathspecial.d std/meta.d std/mmfile.d std/net/curl.d \
	std/net/isemail.d std/numeric.d std/outbuffer.d std/parallelism.d \
	std/path.d std/process.d std/random.d std/range/interfaces.d \
	std/range/package.d std/range/primitives.d \
	std/regex/internal/backtracking.d std/regex/internal/generator.d \
	std/regex/internal/ir.d std/regex/internal/kickstart.d \
	std/regex/internal/parser.d std/regex/internal/tests.d \
	std/regex/internal/thompson.d std/regex/package.d std/signals.d \
	std/socket.d std/stdint.d std/stdio.d std/string.d std/system.d \
	std/traits.d std/typecons.d std/typetuple.d std/uni.d std/uri.d \
	std/utf.d std/uuid.d std/variant.d std/windows/charset.d \
	std/windows/registry.d std/windows/syserror.d std/xml.d std/zip.d \
	std/zlib.d
