AM_CPPFLAGS = -I $(top_srcdir) -I $(top_srcdir)/include

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
ACLOCAL_AMFLAGS = -I m4

toolexeclib_LTLIBRARIES = libtsan.la

tsan_files = \
        tsan_clock.cc \
        tsan_interface_atomic.cc \
        tsan_mutex.cc \
        tsan_report.cc \
        tsan_rtl_thread.cc \
        tsan_symbolize.cc \
        tsan_flags.cc \
        tsan_interface.cc \
        tsan_platform_linux.cc \
        tsan_rtl.cc \
        tsan_stat.cc \
        tsan_sync.cc \
        tsan_interceptors.cc \
        tsan_md5.cc \
        tsan_platform_mac.cc \
        tsan_rtl_mutex.cc \
        tsan_suppressions.cc \
        tsan_interface_ann.cc \
        tsan_mman.cc \
        tsan_rtl_report.cc \
        tsan_symbolize_addr2line_linux.cc

libtsan_la_SOURCES = $(tsan_files) 
libtsan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/interception/libinterception.la $(LIBSTDCXX_RAW_CXX_LDLAGS)
libtsan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` -lpthread -ldl

MAKEOVERRIDES=

## ################################################################

