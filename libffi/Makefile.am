## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus

SUBDIRS = include

EXTRA_DIST = LICENSE ChangeLog.v1 src/mips/ffi.c src/mips/n32.S \
		src/mips/n32.s src/mips/o32.S src/mips/o32.s \
		src/sparc/ffi.c src/sparc/v8.S src/sparc/v9.S \
		src/x86/ffi.c src/x86/sysv.S \
		src/alpha/ffi.c src/alpha/osf.S \
		src/m68k/ffi.c src/m68k/sysv.S \
		src/powerpc/ffi.c src/powerpc/sysv.S src/powerpc/asm.h \
		src/arm/ffi.c src/arm/sysv.S

VPATH = @srcdir@:@srcdir@/src:@srcdir@/src/@TARGETDIR@

## ################################################################

##
## This section is for make and multilib madness.
##

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
	"exec_prefix=$(exec_prefix)" \
	"infodir=$(infodir)" \
	"libdir=$(libdir)" \
	"prefix=$(prefix)" \
	"AR=$(AR)" \
	"AS=$(AS)" \
	"CC=$(CC)" \
	"CXX=$(CXX)" \
	"LD=$(LD)" \
	"LIBCFLAGS=$(LIBCFLAGS)" \
	"NM=$(NM)" \
	"PICFLAG=$(PICFLAG)" \
	"RANLIB=$(RANLIB)" \
	"DESTDIR=$(DESTDIR)"

MAKEOVERRIDES=

# Multilib support variables.
MULTISRCTOP =
MULTIBUILDTOP =
MULTIDIRS =
MULTISUBDIR =
MULTIDO = true
MULTICLEAN = true

## Install a library built with a cross compiler in tooldir, not
## libdir.
if USE_LIBDIR
toolexeclibdir = $(libdir)$(MULTISUBDIR)
else
toolexecdir = $(exec_prefix)/$(target_alias)
toolexeclibdir = $(toolexecdir)/lib$(MULTISUBDIR)
endif

toolexeclib_LTLIBRARIES = libffi.la

noinst_PROGRAMS = ffitest

ffitest_SOURCES = src/ffitest.c
ffitest_LDADD = libffi.la

TARGET_SRC_MIPS_GCC = src/mips/ffi.c src/mips/o32.S src/mips/n32.S
TARGET_SRC_MIPS_SGI = src/mips/ffi.c src/mips/o32.s src/mips/n32.s
TARGET_SRC_X86 = src/x86/ffi.c src/x86/sysv.S
TARGET_SRC_SPARC = src/sparc/ffi.c src/sparc/v8.S src/sparc/v9.S
TARGET_SRC_ALPHA = src/alpha/ffi.c src/alpha/osf.S
TARGET_SRC_IA64 = src/ia64/ffi.c src/ia64/unix.S
TARGET_SRC_M68K = src/m68k/ffi.c src/m68k/sysv.S
TARGET_SRC_POWERPC = src/powerpc/ffi.c src/powerpc/sysv.S
TARGET_SRC_ARM =  src/arm/sysv.S src/arm/ffi.c

##libffi_la_SOURCES = src/debug.c src/prep_cif.c src/types.c $(TARGET_SRC_@TARGET@)
## Work around automake deficiency
libffi_la_common_SOURCES = src/debug.c src/prep_cif.c src/types.c \
		src/raw_api.c src/java_raw_api.c
if MIPS_GCC
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_GCC)
endif
if MIPS_SGI
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_SGI)
endif
if X86
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86)
endif
if SPARC
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SPARC)
endif
if ALPHA
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ALPHA)
endif
if IA64
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_IA64)
endif
if M68K
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_M68K)
endif
if POWERPC
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC)
endif
if ARM
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ARM)
endif

AM_CFLAGS = -fexceptions

libffi_la_LDFLAGS = -release $(VERSION) 

INCLUDES = -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

# Multilib support.
.PHONY: all-multi mostlyclean-multi clean-multi distclean-multi \
	maintainer-clean-multi

all-recursive: all-multi
install-recursive: install-multi
mostlyclean-recursive: mostlyclean-multi
clean-recursive: clean-multi
distclean-recursive: distclean-multi
maintainer-clean-recursive: maintainer-clean-multi

all-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
install-multi:
	$(MULTIDO) $(AM_MAKEFLAGS) DO=install multi-do
mostlyclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=mostlyclean multi-clean
clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=clean multi-clean
distclean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=distclean multi-clean
maintainer-clean-multi:
	$(MULTICLEAN) $(AM_MAKEFLAGS) DO=maintainer-clean multi-clean


## ################################################################

