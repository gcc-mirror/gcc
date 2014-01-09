AM_CPPFLAGS = -I $(top_srcdir)/include
 
# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
if LIBBACKTRACE_SUPPORTED
AM_CXXFLAGS += -DSANITIZER_LIBBACKTRACE -I $(top_srcdir)/../libbacktrace \
	       -I $(top_builddir)/libbacktrace \
	       -include $(top_srcdir)/libbacktrace/backtrace-rename.h
endif
ACLOCAL_AMFLAGS = -I m4

noinst_LTLIBRARIES = libsanitizer_common.la

sanitizer_common_files = \
	sanitizer_allocator.cc \
	sanitizer_common.cc \
	sanitizer_common_libcdep.cc \
	sanitizer_coverage.cc \
	sanitizer_flags.cc \
	sanitizer_libc.cc \
	sanitizer_libignore.cc \
	sanitizer_linux.cc \
	sanitizer_linux_libcdep.cc \
	sanitizer_mac.cc \
	sanitizer_platform_limits_linux.cc \
	sanitizer_platform_limits_posix.cc \
	sanitizer_posix_libcdep.cc \
	sanitizer_posix.cc \
	sanitizer_printf.cc \
	sanitizer_stackdepot.cc \
	sanitizer_stacktrace.cc \
	sanitizer_stacktrace_libcdep.cc \
	sanitizer_stoptheworld_linux_libcdep.cc \
	sanitizer_suppressions.cc \
	sanitizer_symbolizer_posix_libcdep.cc \
	sanitizer_symbolizer_win.cc \
	sanitizer_symbolizer.cc \
	sanitizer_symbolizer_libbacktrace.cc \
	sanitizer_symbolizer_libcdep.cc \
	sanitizer_thread_registry.cc \
	sanitizer_win.cc

libsanitizer_common_la_SOURCES = $(sanitizer_common_files) 

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

## ################################################################

