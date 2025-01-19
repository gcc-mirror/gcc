AM_CPPFLAGS = -I $(top_srcdir)/include -I $(top_srcdir) -isystem $(top_srcdir)/include/system

# May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

DEFS = -D_GNU_SOURCE -D_DEBUG -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS @RPC_DEFS@ @AS_SYM_ASSIGN_DEFS@
AM_CXXFLAGS = -Wall -W -Wno-unused-parameter -Wwrite-strings -Wno-long-long -fPIC -fno-builtin -fno-exceptions -fno-rtti -fomit-frame-pointer -funwind-tables -fvisibility=hidden -Wno-variadic-macros
AM_CXXFLAGS += $(LIBSTDCXX_RAW_CXX_CXXFLAGS)
AM_CXXFLAGS += -std=gnu++17
AM_CXXFLAGS += $(EXTRA_CXXFLAGS)
if LIBBACKTRACE_SUPPORTED
AM_CXXFLAGS += -DSANITIZER_LIBBACKTRACE -DSANITIZER_CP_DEMANGLE \
	       -I $(top_srcdir)/../libbacktrace \
	       -I $(top_builddir)/libbacktrace \
	       -I $(top_srcdir)/../include \
	       -include $(top_srcdir)/libbacktrace/backtrace-rename.h
endif
AM_CCASFLAGS = $(EXTRA_ASFLAGS)
ACLOCAL_AMFLAGS = -I m4
sanitizer_platform_limits_freebsd.lo: AM_CXXFLAGS += -DUSE_SYSTEM_MD5

noinst_LTLIBRARIES = libsanitizer_common.la

sanitizer_common_files = \
	sancov_flags.cpp \
	sanitizer_allocator.cpp \
	sanitizer_allocator_checks.cpp \
	sanitizer_allocator_report.cpp \
	sanitizer_common.cpp \
	sanitizer_common_libcdep.cpp \
	sanitizer_coverage_libcdep_new.cpp \
	sanitizer_deadlock_detector1.cpp \
	sanitizer_deadlock_detector2.cpp \
	sanitizer_dl.cpp \
	sanitizer_errno.cpp \
	sanitizer_file.cpp \
	sanitizer_flags.cpp \
	sanitizer_flag_parser.cpp \
	sanitizer_libc.cpp \
	sanitizer_libignore.cpp \
	sanitizer_linux.cpp \
	sanitizer_linux_libcdep.cpp \
	sanitizer_linux_s390.cpp \
	sanitizer_mac.cpp \
	sanitizer_mac_libcdep.cpp \
	sanitizer_mutex.cpp \
	sanitizer_netbsd.cpp \
	sanitizer_platform_limits_freebsd.cpp \
	sanitizer_platform_limits_linux.cpp \
	sanitizer_platform_limits_posix.cpp \
	sanitizer_platform_limits_solaris.cpp \
	sanitizer_posix.cpp \
	sanitizer_posix_libcdep.cpp \
	sanitizer_printf.cpp \
	sanitizer_procmaps_bsd.cpp \
	sanitizer_procmaps_common.cpp \
	sanitizer_procmaps_linux.cpp \
	sanitizer_procmaps_mac.cpp \
	sanitizer_procmaps_solaris.cpp \
	sanitizer_range.cpp \
	sanitizer_solaris.cpp \
	sanitizer_stack_store.cpp \
	sanitizer_stackdepot.cpp \
	sanitizer_stacktrace.cpp \
	sanitizer_stacktrace_libcdep.cpp \
	sanitizer_stacktrace_sparc.cpp \
	sanitizer_symbolizer_mac.cpp \
	sanitizer_symbolizer_report.cpp \
	sanitizer_stacktrace_printer.cpp \
	sanitizer_stoptheworld_linux_libcdep.cpp \
	sanitizer_stoptheworld_mac.cpp \
	sanitizer_suppressions.cpp \
	sanitizer_symbolizer.cpp \
	sanitizer_symbolizer_libbacktrace.cpp \
	sanitizer_symbolizer_libcdep.cpp \
	sanitizer_symbolizer_markup.cpp \
	sanitizer_symbolizer_posix_libcdep.cpp \
	sanitizer_symbolizer_win.cpp \
	sanitizer_termination.cpp \
	sanitizer_thread_arg_retval.cpp \
	sanitizer_thread_history.cpp \
	sanitizer_thread_registry.cpp \
	sanitizer_tls_get_addr.cpp \
	sanitizer_unwind_linux_libcdep.cpp \
	sanitizer_unwind_win.cpp \
	sanitizer_win.cpp \
	sanitizer_win_interception.cpp


libsanitizer_common_la_SOURCES = $(sanitizer_common_files) 
libsanitizer_common_la_LIBADD = $(SANITIZER_COMMON_TARGET_DEPENDENT_OBJECTS)
libsanitizer_common_la_DEPENDENCIES = $(SANITIZER_COMMON_TARGET_DEPENDENT_OBJECTS)

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

