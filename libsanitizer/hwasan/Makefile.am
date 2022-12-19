AM_CPPFLAGS = -I $(top_srcdir)/include -I $(top_srcdir)

# May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DCAN_SANITIZE_UB=0 -DHWASAN_WITH_INTERCEPTORS=1
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -pedantic -Wno-long-long  -fPIC -fno-builtin -fno-exceptions -fno-rtti -funwind-tables -fvisibility=hidden -Wno-variadic-macros -fno-ipa-icf
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
AM_CXXFLAGS += -std=gnu++14
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)
AM_CCASFLAGS = $(EXTRA_ASFLAGS)
ACLOCAL_AMFLAGS = -I $(top_srcdir) -I $(top_srcdir)/config

toolexeclib_LTLIBRARIES = libhwasan.la
nodist_toolexeclib_HEADERS = libhwasan_preinit.o

hwasan_files = \
	hwasan_allocation_functions.cpp \
	hwasan_allocator.cpp \
	hwasan.cpp \
	hwasan_dynamic_shadow.cpp \
	hwasan_exceptions.cpp \
	hwasan_flags.inc \
	hwasan_fuchsia.cpp \
	hwasan_globals.cpp \
	hwasan_interceptors.cpp \
	hwasan_interceptors_vfork.S \
	hwasan_linux.cpp \
	hwasan_memintrinsics.cpp \
	hwasan_new_delete.cpp \
	hwasan_poisoning.cpp \
	hwasan_report.cpp \
	hwasan_setjmp_aarch64.S \
	hwasan_setjmp_x86_64.S \
	hwasan_tag_mismatch_aarch64.S \
	hwasan_thread.cpp \
	hwasan_thread_list.cpp \
	hwasan_type_test.cpp

libhwasan_la_SOURCES = $(hwasan_files)
libhwasan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la
if !USING_MAC_INTERPOSE
libhwasan_la_LIBADD += $(top_builddir)/interception/libinterception.la
endif
if LIBBACKTRACE_SUPPORTED
libhwasan_la_LIBADD += $(top_builddir)/libbacktrace/libsanitizer_libbacktrace.la
endif
libhwasan_la_LIBADD += $(LIBSTDCXX_RAW_CXX_LDFLAGS)

libhwasan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` $(link_libhwasan)

libhwasan_preinit.o: hwasan_preinit.o
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


