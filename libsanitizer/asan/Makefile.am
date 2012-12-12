AM_CPPFLAGS = -I $(top_srcdir)/include -I $(top_srcdir)
 
# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DASAN_HAS_EXCEPTIONS=1 -DASAN_FLEXIBLE_MAPPING_AND_OFFSET=0 -DASAN_NEEDS_SEGV=1
if USING_MAC_INTERPOSE
DEFS += -DMAC_INTERPOSE_FUNCTIONS -DMISSING_BLOCKS_SUPPORT
endif
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros -Wno-c99-extensions 
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
ACLOCAL_AMFLAGS = -I $(top_srcdir) -I $(top_srcdir)/config

toolexeclib_LTLIBRARIES = libasan.la

asan_files = \
	asan_allocator.cc \
	asan_interceptors.cc \
	asan_mac.cc \
	asan_malloc_mac.cc \
	asan_new_delete.cc \
	asan_posix.cc \
	asan_rtl.cc \
	asan_stats.cc \
	asan_thread_registry.cc \
	asan_globals.cc \
	asan_linux.cc \
	asan_malloc_linux.cc \
	asan_malloc_win.cc \
	asan_poisoning.cc \
	asan_report.cc \
	asan_stack.cc \
	asan_thread.cc \
	asan_win.cc

libasan_la_SOURCES = $(asan_files)
if USING_MAC_INTERPOSE
libasan_la_SOURCES += dynamic/asan_interceptors_dynamic.cc
libasan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la
else
libasan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/interception/libinterception.la
endif
libasan_la_LIBADD += $(LIBSTDCXX_RAW_CXX_LDLAGS)

libasan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` -lpthread -ldl

MAKEOVERRIDES=

## ################################################################

