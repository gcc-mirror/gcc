AM_CPPFLAGS = -I $(top_srcdir)/include -I $(top_srcdir)
 
# May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DASAN_HAS_EXCEPTIONS=1 -DASAN_NEEDS_SEGV=1 -DCAN_SANITIZE_UB=0 -DASAN_HAS_CXA_RETHROW_PRIMARY_EXCEPTION=0
if USING_MAC_INTERPOSE
DEFS += -DMAC_INTERPOSE_FUNCTIONS -DMISSING_BLOCKS_SUPPORT
endif
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros -fno-ipa-icf
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
AM_CXXFLAGS += -std=gnu++14
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)
ACLOCAL_AMFLAGS = -I $(top_srcdir) -I $(top_srcdir)/config

toolexeclib_LTLIBRARIES = libasan.la
nodist_toolexeclib_HEADERS = libasan_preinit.o

asan_files = \
	asan_activation.cpp \
	asan_allocator.cpp \
	asan_debugging.cpp \
	asan_descriptions.cpp \
	asan_errors.cpp \
	asan_fake_stack.cpp \
	asan_flags.cpp \
	asan_globals.cpp \
	asan_interceptors.cpp \
	asan_interceptors_memintrinsics.cpp \
	asan_linux.cpp \
	asan_mac.cpp \
	asan_malloc_linux.cpp \
	asan_malloc_mac.cpp \
	asan_malloc_win.cpp \
	asan_memory_profile.cpp \
	asan_new_delete.cpp \
	asan_poisoning.cpp \
	asan_posix.cpp \
	asan_premap_shadow.cpp \
	asan_report.cpp \
	asan_rtems.cpp \
	asan_rtl.cpp \
	asan_shadow_setup.cpp \
	asan_stack.cpp \
	asan_stats.cpp \
	asan_suppressions.cpp \
	asan_thread.cpp \
	asan_win.cpp \
	asan_win_dll_thunk.cpp \
	asan_win_dynamic_runtime_thunk.cpp \
  asan_interceptors_vfork.S

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

