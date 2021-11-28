ACLOCAL_AMFLAGS = -I .. -I ../config

sanincludedir = $(libdir)/gcc/$(target_alias)/$(gcc_version)/include/sanitizer

nodist_saninclude_HEADERS =

if SANITIZER_SUPPORTED
SUBDIRS = sanitizer_common
nodist_saninclude_HEADERS += \
  include/sanitizer/common_interface_defs.h
if !USING_MAC_INTERPOSE
SUBDIRS += interception
endif
if LIBBACKTRACE_SUPPORTED
SUBDIRS += libbacktrace
endif
SUBDIRS += lsan asan ubsan
nodist_saninclude_HEADERS += \
  include/sanitizer/lsan_interface.h \
  include/sanitizer/asan_interface.h \
  include/sanitizer/tsan_interface.h \
  include/sanitizer/hwasan_interface.h
if TSAN_SUPPORTED
SUBDIRS += tsan
endif
if HWASAN_SUPPORTED
SUBDIRS += hwasan
endif
endif

## Force DIST_SUBDIRS so that make distclean works
DIST_SUBDIRS = $(SUBDIRS)

## May be used by toolexeclibdir.
gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)

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

nodist_toolexeclib_HEADERS = libsanitizer.spec

## ################################################################

include $(top_srcdir)/../multilib.am
