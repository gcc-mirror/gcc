## Makefile for the Phobos standard library.
## Copyright (C) 2012-2025 Free Software Foundation, Inc.
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
D_EXTRA_DFLAGS=-fpreview=dip1000 -fpreview=dtorfields -fpreview=fieldwise \
	-nostdinc -I $(srcdir) -I $(top_srcdir)/libdruntime \
	-I ../libdruntime -I .

# D flags for compilation
AM_DFLAGS= \
	$(phobos_lt_pic_flag) $(phobos_compiler_shared_flag) \
	$(WARN_DFLAGS) $(CHECKING_DFLAGS) $(SECTION_FLAGS) $(CET_FLAGS) \
	$(OS_DFLAGS)

# Flags for other kinds of sources
AM_CFLAGS=$(CET_FLAGS)

# Install all D files
ALL_PHOBOS_INSTALL_DSOURCES = $(PHOBOS_DSOURCES)

# Setup source files depending on configure
ALL_PHOBOS_COMPILE_DSOURCES = $(PHOBOS_DSOURCES)

ALL_PHOBOS_SOURCES = $(ALL_PHOBOS_COMPILE_DSOURCES)

# Main library build definitions
toolexeclib_DATA = libgphobos.spec
toolexeclib_LTLIBRARIES = libgphobos.la
libgphobos_la_SOURCES = $(ALL_PHOBOS_SOURCES)
libgphobos_la_LIBTOOLFLAGS =
if ENABLE_DARWIN_AT_RPATH
libgphobos_darwin_rpath = -Wl,-rpath,@loader_path
endif
libgphobos_la_LDFLAGS = -Wc,-nophoboslib,-dstartfiles,-B../libdruntime/gcc \
    -version-info $(libtool_VERSION) $(libgphobos_darwin_rpath)
if ENABLE_LIBDRUNTIME_ONLY
libgphobos_la_LIBADD = ../libdruntime/libgdruntime_convenience.la
else
libgphobos_la_LIBADD = \
    ../libdruntime/libgdruntime_convenience.la $(LIBZ)
endif
libgphobos_la_DEPENDENCIES = \
    ../libdruntime/libgdruntime_convenience.la libgphobos.spec

# Also override library link commands: This is not strictly
# required, but we want to record additional dependencies such
# as pthread in the library
libgphobos_la_LINK = $(LIBTOOL) --tag=D $(libgphobos_la_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(GDC) $(AM_CFLAGS) $(CFLAGS) \
	$(libgphobos_la_LDFLAGS) $(LDFLAGS) -o $@

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

# Source file definitions. Boring stuff, auto-generated with
# libphobos/scripts/gen_phobos_sources.d
# Can't use wildcards here:
# https://www.gnu.org/software/automake/manual/html_node/Wildcards.html
if ENABLE_LIBDRUNTIME_ONLY

PHOBOS_DSOURCES =

else

PHOBOS_DSOURCES = etc/c/curl.d etc/c/odbc/odbc32.d etc/c/odbc/odbc64.d \
	etc/c/odbc/package.d etc/c/odbc/sql.d etc/c/odbc/sqlext.d \
	etc/c/odbc/sqltypes.d etc/c/odbc/sqlucode.d etc/c/sqlite3.d \
	etc/c/zlib.d std/algorithm/comparison.d std/algorithm/internal.d \
	std/algorithm/iteration.d std/algorithm/mutation.d \
	std/algorithm/package.d std/algorithm/searching.d \
	std/algorithm/setops.d std/algorithm/sorting.d std/array.d std/ascii.d \
	std/base64.d std/bigint.d std/bitmanip.d std/checkedint.d \
	std/compiler.d std/complex.d std/concurrency.d std/container/array.d \
	std/container/binaryheap.d std/container/dlist.d \
	std/container/package.d std/container/rbtree.d std/container/slist.d \
	std/container/util.d std/conv.d std/csv.d std/datetime/date.d \
	std/datetime/interval.d std/datetime/package.d \
	std/datetime/stopwatch.d std/datetime/systime.d \
	std/datetime/timezone.d std/demangle.d std/digest/crc.d \
	std/digest/hmac.d std/digest/md.d std/digest/murmurhash.d \
	std/digest/package.d std/digest/ripemd.d std/digest/sha.d \
	std/encoding.d std/exception.d \
	std/experimental/allocator/building_blocks/affix_allocator.d \
	std/experimental/allocator/building_blocks/aligned_block_list.d \
	std/experimental/allocator/building_blocks/allocator_list.d \
	std/experimental/allocator/building_blocks/ascending_page_allocator.d \
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
	std/file.d std/format/internal/floats.d std/format/internal/read.d \
	std/format/internal/write.d std/format/package.d std/format/read.d \
	std/format/spec.d std/format/write.d std/functional.d std/getopt.d \
	std/int128.d std/internal/attributes.d std/internal/cstring.d \
	std/internal/math/biguintcore.d std/internal/math/biguintnoasm.d \
	std/internal/math/errorfunction.d std/internal/math/gammafunction.d \
	std/internal/memory.d std/internal/scopebuffer.d \
	std/internal/test/dummyrange.d std/internal/test/range.d \
	std/internal/test/sumtype_example_overloads.d std/internal/test/uda.d \
	std/internal/unicode_comp.d std/internal/unicode_decomp.d \
	std/internal/unicode_grapheme.d std/internal/unicode_norm.d \
	std/internal/unicode_tables.d std/internal/windows/advapi32.d \
	std/internal/windows/bcrypt.d std/json.d std/logger/core.d \
	std/logger/filelogger.d std/logger/multilogger.d \
	std/logger/nulllogger.d std/logger/package.d std/math/algebraic.d \
	std/math/constants.d std/math/exponential.d std/math/hardware.d \
	std/math/operations.d std/math/package.d std/math/remainder.d \
	std/math/rounding.d std/math/traits.d std/math/trigonometry.d \
	std/mathspecial.d std/meta.d std/mmfile.d std/net/curl.d \
	std/net/isemail.d std/numeric.d std/outbuffer.d std/package.d \
	std/parallelism.d std/path.d std/process.d std/random.d \
	std/range/interfaces.d std/range/package.d std/range/primitives.d \
	std/regex/internal/backtracking.d std/regex/internal/generator.d \
	std/regex/internal/ir.d std/regex/internal/kickstart.d \
	std/regex/internal/parser.d std/regex/internal/tests.d \
	std/regex/internal/tests2.d std/regex/internal/thompson.d \
	std/regex/package.d std/signals.d std/socket.d std/stdint.d \
	std/stdio.d std/string.d std/sumtype.d std/system.d std/traits.d \
	std/typecons.d std/typetuple.d std/uni/package.d std/uri.d std/utf.d \
	std/uuid.d std/variant.d std/windows/charset.d std/windows/registry.d \
	std/windows/syserror.d std/zip.d std/zlib.d

endif
