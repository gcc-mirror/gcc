## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus

SUBDIRS = include testsuite

EXTRA_DIST = LICENSE ChangeLog.v1 \
	src/alpha/ffi.c src/alpha/osf.S src/alpha/ffitarget.h \
	src/arm/ffi.c src/arm/sysv.S src/arm/ffitarget.h \
	src/mips/ffi.c src/mips/n32.S src/mips/o32.S \
	src/mips/ffitarget.h \
	src/m68k/ffi.c src/m68k/sysv.S src/m68k/ffitarget.h \
	src/powerpc/ffi.c src/powerpc/sysv.S \
	src/powerpc/linux64.S src/powerpc/linux64_closure.S \
	src/powerpc/ppc_closure.S src/powerpc/asm.h \
	src/powerpc/aix.S src/powerpc/darwin.S \
	src/powerpc/aix_closure.S src/powerpc/darwin_closure.S \
	src/powerpc/ffi_darwin.c src/powerpc/ffitarget.h \
	src/s390/ffi.c src/s390/sysv.S src/s390/ffitarget.h \
	src/sh/ffi.c src/sh/sysv.S src/sh/ffitarget.h \
	src/sh64/ffi.c src/sh64/sysv.S src/sh64/ffitarget.h \
	src/sparc/v8.S src/sparc/v9.S src/sparc/ffitarget.h \
	src/sparc/ffi.c \
	src/x86/ffi.c src/x86/sysv.S src/x86/win32.S \
	src/x86/ffi64.c src/x86/unix64.S src/x86/ffitarget.h


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
	"RUNTESTFLAGS=$(RUNTESTFLAGS)" \
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

toolexeclib_LTLIBRARIES = libffi.la
noinst_LTLIBRARIES = libffi_convenience.la



TARGET_SRC_MIPS_IRIX = src/mips/ffi.c src/mips/o32.S src/mips/n32.S
TARGET_SRC_MIPS_LINUX = src/mips/ffi.c src/mips/o32.S
TARGET_SRC_X86 = src/x86/ffi.c src/x86/sysv.S
TARGET_SRC_X86_WIN32 = src/x86/ffi.c src/x86/win32.S
TARGET_SRC_SPARC = src/sparc/ffi.c src/sparc/v8.S src/sparc/v9.S
TARGET_SRC_ALPHA = src/alpha/ffi.c src/alpha/osf.S
TARGET_SRC_IA64 = src/ia64/ffi.c src/ia64/unix.S
TARGET_SRC_M68K = src/m68k/ffi.c src/m68k/sysv.S
TARGET_SRC_POWERPC = src/powerpc/ffi.c src/powerpc/sysv.S src/powerpc/ppc_closure.S src/powerpc/linux64.S src/powerpc/linux64_closure.S
TARGET_SRC_POWERPC_AIX = src/powerpc/ffi_darwin.c src/powerpc/aix.S src/powerpc/aix_closure.S
TARGET_SRC_POWERPC_DARWIN = src/powerpc/ffi_darwin.c src/powerpc/darwin.S src/powerpc/darwin_closure.S
TARGET_SRC_ARM =  src/arm/sysv.S src/arm/ffi.c
TARGET_SRC_S390 =  src/s390/sysv.S src/s390/ffi.c
TARGET_SRC_X86_64 = src/x86/ffi64.c src/x86/unix64.S src/x86/ffi.c src/x86/sysv.S
TARGET_SRC_SH =  src/sh/sysv.S src/sh/ffi.c
TARGET_SRC_SH64 =  src/sh64/sysv.S src/sh64/ffi.c

##libffi_la_SOURCES = src/debug.c src/prep_cif.c src/types.c $(TARGET_SRC_@TARGET@)
## Work around automake deficiency
libffi_la_common_SOURCES = src/debug.c src/prep_cif.c src/types.c \
		src/raw_api.c src/java_raw_api.c
if MIPS_IRIX
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_IRIX)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_IRIX)
endif
if MIPS_LINUX
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_LINUX)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_MIPS_LINUX)
endif
if X86
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86)
endif
if X86_WIN32
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86_WIN32)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86_WIN32)
endif
if SPARC
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SPARC)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SPARC)
endif
if ALPHA
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ALPHA)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ALPHA)
endif
if IA64
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_IA64)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_IA64)
endif
if M68K
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_M68K)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_M68K)
endif
if POWERPC
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC)
endif
if POWERPC_AIX
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC_AIX)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC_AIX)
endif
if POWERPC_DARWIN
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC_DARWIN)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_POWERPC_DARWIN)
endif
if ARM
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ARM)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_ARM)
endif
if S390
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_S390)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_S390)
endif
if X86_64
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86_64)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_X86_64)
endif
if SH
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SH)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SH)
endif
if SH64
libffi_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SH64)
libffi_convenience_la_SOURCES = $(libffi_la_common_SOURCES) $(TARGET_SRC_SH64)
endif

AM_CFLAGS = -Wall -g -fexceptions

libffi_la_LDFLAGS = -release $(VERSION) 

INCLUDES = -I. -I$(top_srcdir)/include -Iinclude -I$(top_srcdir)/src

# Override these rules so that object files get put in the correct 
# subdirectories. 
.c.o:
	$(LTCOMPILE) -c -o $@ $<

.s.o:
	$(LTCOMPILE) -c -o $@ $<

.S.o:
	$(LTCOMPILE) -c -o $@ $<

.c.lo:
	$(LTCOMPILE) -c -o $@ $<

.s.lo:
	$(LTCOMPILE) -c -o $@ $<

.S.lo:
	$(LTCOMPILE) -c -o $@ $<

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
	: $(MAKE) ; exec $(MULTIDO) $(AM_MAKEFLAGS) DO=all multi-do
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

# Our hacked automake doesn't clean subdirectories properly.
mostlyclean-am: mostlyclean-sub
mostlyclean-sub:
	-rm -f src/*.o src/*.lo
	-rm -f src/@TARGETDIR@/*.o src/@TARGETDIR@/*.lo

clean-am: clean-sub
clean-sub:
	-rm -rf src/.libs src/_libs
	-rm -rf src/@TARGETDIR@/.libs src/@TARGETDIR@/_libs


