## Process this with automake to create Makefile.in

AUTOMAKE_OPTIONS = cygnus

EXTRA_DIST = LICENSE ChangeLog.v1 src/mips/ffi.c src/mips/n32.S \
		src/mips/n32.s src/mips/o32.S src/mips/o32.s \
		src/sparc/ffi.c src/sparc/v8.S \
		src/x86/ffi.c src/x86/sysv.S \
		src/alpha/ffi.c src/alpha/osf.S \
		src/m68k/ffi.c src/m68k/sysv.S \
		src/powerpc/ffi.c src/powerpc/sysv.S src/powerpc/asm.h \
		src/arm/ffi.c src/arm/sysv.S

VPATH = @srcdir@:@srcdir@/src:@srcdir@/src/@TARGETDIR@

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
TARGET_SRC_SPARC = src/sparc/ffi.c src/sparc/v8.S
TARGET_SRC_ALPHA = src/alpha/ffi.c src/alpha/osf.S
TARGET_SRC_M68K = src/m68k/ffi.c src/m68k/sysv.S
TARGET_SRC_POWERPC = src/powerpc/ffi.c src/powerpc/sysv.S
TARGET_SRC_ARM =  src/arm/sysv.S src/arm/ffi.c

##libffi_la_SOURCES = src/debug.c src/prep_cif.c src/types.c $(TARGET_SRC_@TARGET@)
## Work around automake deficiency
libffi_la_common_SOURCES = src/debug.c src/prep_cif.c src/types.c src/raw_api.c 
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
