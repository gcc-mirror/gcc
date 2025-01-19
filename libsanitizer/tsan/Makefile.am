AM_CPPFLAGS = -I $(top_srcdir) -I $(top_srcdir)/include

# May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DCAN_SANITIZE_UB=0
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -Wno-long-long -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
AM_CXXFLAGS += -std=gnu++17
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)
AM_CCASFLAGS = $(EXTRA_ASFLAGS)
ACLOCAL_AMFLAGS = -I m4

toolexeclib_LTLIBRARIES = libtsan.la
nodist_toolexeclib_HEADERS = libtsan_preinit.o

tsan_files = \
	tsan_debugging.cpp \
	tsan_external.cpp \
	tsan_fd.cpp \
	tsan_flags.cpp \
	tsan_ignoreset.cpp \
	tsan_interceptors_posix.cpp \
	tsan_interceptors_mac.cpp \
	tsan_interceptors_memintrinsics.cpp \
	tsan_interface_ann.cpp \
	tsan_interface_atomic.cpp \
	tsan_interface.cpp \
	tsan_interface_java.cpp \
	tsan_malloc_mac.cpp \
	tsan_md5.cpp \
	tsan_mman.cpp \
	tsan_mutexset.cpp \
	tsan_new_delete.cpp \
	tsan_platform_linux.cpp \
	tsan_platform_mac.cpp \
	tsan_platform_posix.cpp \
	tsan_platform_windows.cpp \
	tsan_report.cpp \
	tsan_rtl.cpp \
	tsan_rtl_access.cpp \
	tsan_rtl_mutex.cpp \
	tsan_rtl_proc.cpp \
	tsan_rtl_report.cpp \
	tsan_rtl_thread.cpp \
	tsan_stack_trace.cpp \
	tsan_suppressions.cpp \
	tsan_symbolize.cpp \
	tsan_sync.cpp \
	tsan_vector_clock.cpp

libtsan_la_SOURCES = $(tsan_files)
EXTRA_libtsan_la_SOURCES = tsan_rtl_amd64.S tsan_rtl_aarch64.S tsan_rtl_loongarch64.S tsan_rtl_mips64.S tsan_rtl_ppc64.S tsan_rtl_s390x.S tsan_rtl_riscv64.S
libtsan_la_LIBADD = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/interception/libinterception.la $(TSAN_TARGET_DEPENDENT_OBJECTS)
libtsan_la_DEPENDENCIES = $(top_builddir)/sanitizer_common/libsanitizer_common.la $(top_builddir)/interception/libinterception.la $(TSAN_TARGET_DEPENDENT_OBJECTS)
if LIBBACKTRACE_SUPPORTED
libtsan_la_LIBADD += $(top_builddir)/libbacktrace/libsanitizer_libbacktrace.la
libtsan_la_DEPENDENCIES +=$(top_builddir)/libbacktrace/libsanitizer_libbacktrace.la
endif
libtsan_la_LIBADD += $(LIBSTDCXX_RAW_CXX_LDFLAGS)
if ENABLE_DARWIN_AT_RPATH
libtsan_darwin_rpath = -nodefaultrpaths -Wl,-rpath,@loader_path/
endif
libtsan_la_LDFLAGS = -version-info `grep -v '^\#' $(srcdir)/libtool-version` \
	$(link_libtsan) $(libtsan_darwin_rpath)

libtsan_preinit.o: tsan_preinit.o
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

