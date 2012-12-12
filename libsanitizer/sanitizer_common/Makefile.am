AM_CPPFLAGS = -I $(top_srcdir)/include 
 
# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros -Wno-c99-extensions 
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
ACLOCAL_AMFLAGS = -I m4

noinst_LTLIBRARIES = libsanitizer_common.la

sanitizer_common_files = \
        sanitizer_allocator.cc \
        sanitizer_common.cc \
        sanitizer_flags.cc \
        sanitizer_libc.cc \
        sanitizer_linux.cc \
        sanitizer_mac.cc \
        sanitizer_posix.cc \
        sanitizer_printf.cc \
        sanitizer_stackdepot.cc \
        sanitizer_stacktrace.cc \
        sanitizer_symbolizer.cc \
        sanitizer_symbolizer_linux.cc \
        sanitizer_symbolizer_mac.cc \
        sanitizer_symbolizer_win.cc \
        sanitizer_win.cc

libsanitizer_common_la_SOURCES = $(sanitizer_common_files) 

## ################################################################

