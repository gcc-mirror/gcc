AM_CPPFLAGS = -I $(top_srcdir) -I $(top_srcdir)/include

# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS 
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
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
	tsan_fd.cc \
        tsan_interface_java.cc \
        tsan_mutexset.cc \
        tsan_symbolize_addr2line_linux.cc \
        tsan_rtl_amd64.S

libtsan_la_SOURCES = $(tsan_files) 
libtsan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/interception/libinterception.la $(LIBSTDCXX_RAW_CXX_LDFLAGS)
libtsan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(link_libtsan)

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

