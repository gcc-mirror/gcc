AM_CPPFLAGS = -I $(top_srcdir)/include -I $(top_srcdir)
 
# May be used by toolexeclibdir.
gcc_version := $(shell cat $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DASAN_HAS_EXCEPTIONS=1 -DASAN_FLEXIBLE_MAPPING_AND_OFFSET=0 -DASAN_NEEDS_SEGV=1
if USING_MAC_INTERPOSE
DEFS += -DMAC_INTERPOSE_FUNCTIONS -DMISSING_BLOCKS_SUPPORT
endif
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
ACLOCAL_AMFLAGS = -I $(top_srcdir) -I $(top_srcdir)/config

toolexeclib_LTLIBRARIES = libasan.la
nodist_toolexeclib_HEADERS = libasan_preinit.o

asan_files = \
	asan_allocator2.cc \
	asan_dll_thunk.cc \
	asan_fake_stack.cc \
	asan_globals.cc \
	asan_interceptors.cc \
	asan_linux.cc \
	asan_mac.cc \
	asan_malloc_linux.cc \
	asan_malloc_mac.cc \
	asan_malloc_win.cc \
	asan_new_delete.cc \
	asan_poisoning.cc \
	asan_posix.cc \
	asan_report.cc \
	asan_rtl.cc \
	asan_stack.cc \
	asan_stats.cc \
	asan_thread.cc \
	asan_win.cc

libasan_la_SOURCES = $(asan_files)
libasan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/lsan/libsanitizer_lsan.la
if !USING_MAC_INTERPOSE
libasan_la_LIBADD += $(top_builddir)/interception/libinterception.la
endif
if LIBBACKTRACE_SUPPORTED
libasan_la_LIBADD += $(top_builddir)/libbacktrace/libsanitizer_libbacktrace.la
endif
libasan_la_LIBADD += $(LIBSTDCXX_RAW_CXX_LDFLAGS)

libasan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(link_libasan)

libasan_preinit.o: asan_preinit.o
	cp $< $@

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

