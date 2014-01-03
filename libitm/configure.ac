# Process this file with autoreconf to produce a configure script.
#   Copyright (C) 2011-2014 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  

AC_PREREQ(2.59)
AC_INIT([GNU TM Runtime Library], 1.0,,[libitm])
AC_CONFIG_HEADER(config.h)

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
LIBITM_ENABLE(version-specific-runtime-libs, no, ,
   [Specify that runtime libraries should be installed in a compiler-specific directory],
   permit yes|no)
AC_MSG_RESULT($enable_version_specific_runtime_libs)

# We would like our source tree to be readonly. However when releases or
# pre-releases are generated, the flex/bison generated files as well as the
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do 
# just that.
AC_MSG_CHECKING([for --enable-generated-files-in-srcdir])
LIBITM_ENABLE(generated-files-in-srcdir, no, ,
   [put copies of generated files in source dir intended for creating source 
    tarballs for users without texinfo bison or flex.],
   permit yes|no)
AC_MSG_RESULT($enable_generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test "$enable_generated_files_in_srcdir" = yes)


# -------
# -------

# Gets build, host, target, *_vendor, *_cpu, *_os, etc.
#
# You will slowly go insane if you do not grok the following fact:  when
# building this library, the top-level /target/ becomes the library's /host/.
#
# configure then causes --target to default to --host, exactly like any
# other package using autoconf.  Therefore, 'target' and 'host' will
# always be the same.  This makes sense both for native and cross compilers
# just think about it for a little while.  :-)
#
# Also, if this library is being configured as part of a cross compiler, the
# top-level configure script will pass the "real" host as $with_cross_host.
#
# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

# Sets up automake.  Must come after AC_CANONICAL_SYSTEM.  Each of the
# following is magically included in AUTOMAKE_OPTIONS in each Makefile.am.
#  1.9.0:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  no-dist:  we don't want 'dist' and related rules.
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is required.
#  -Wno-override: ... and this one, since we do want this in testsuite.
AM_INIT_AUTOMAKE([1.9.0 foreign no-dist -Wall -Wno-portability -Wno-override])
AM_ENABLE_MULTILIB(, ..)

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${enable_version_specific_runtime_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_alias)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_alias)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
      toolexeclibdir='$(libdir)'
    fi
    multi_os_directory=`$CC -print-multi-os-directory`
    case $multi_os_directory in
      .) ;; # Avoid trailing /.
      *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
    esac
    ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

# In order to override CFLAGS_FOR_TARGET, all of our special flags go
# in XCFLAGS.  But we need them in CFLAGS during configury.  So put them
# in both places for now and restore CFLAGS at the end of config.
save_CFLAGS="$CFLAGS"

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PATH_PROG(PERL, perl, perl-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER([MAKEINFO], [makeinfo], [--version],
                   [GNU texinfo.* \([0-9][0-9.]*\)],
                   [4.[4-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
AM_CONDITIONAL(BUILD_INFO, test $gcc_cv_prog_makeinfo_modern = "yes")


# Configure libtool
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AM_MAINTAINER_MODE

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

# Check header files.
AC_STDC_HEADERS
AC_HEADER_TIME
ACX_HEADER_STRING
AC_CHECK_HEADERS(unistd.h semaphore.h sys/time.h sys/auxv.h malloc.h)

GCC_HEADER_STDINT(gstdint.h)

GCC_AC_FUNC_MMAP_BLACKLIST

AC_C_BIGENDIAN
# I don't like the default behaviour of WORDS_BIGENDIAN undefined for LE.
AH_BOTTOM(
[#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif])

# Check to see if -pthread or -lpthread is needed.  Prefer the former.
# In case the pthread.h system header is not found, this test will fail.
XPCFLAGS=""
CFLAGS="$CFLAGS -pthread"
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM(
  [#include <pthread.h>
   void *g(void *d) { return NULL; }],
  [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
 [XPCFLAGS=" -Wc,-pthread"],
 [CFLAGS="$save_CFLAGS" LIBS="-lpthread $LIBS"
  AC_LINK_IFELSE(
   [AC_LANG_PROGRAM(
    [#include <pthread.h>
     void *g(void *d) { return NULL; }],
    [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
   [],
   [AC_MSG_ERROR([Pthreads are required to build libitm])])])

# Check for functions needed.
AC_CHECK_FUNCS(strtoull memalign posix_memalign getauxval)

# Check for broken semaphore implementation on darwin.
# sem_init returns: sem_init error: Function not implemented.
case "$host" in
  *-darwin*)
    AC_DEFINE(HAVE_BROKEN_POSIX_SEMAPHORES, 1,
	Define if the POSIX Semaphores do not work on your system.)
    ;;
esac

GCC_LINUX_FUTEX(:)

# See if we support thread-local storage.
GCC_CHECK_TLS

# See what sort of export controls are available.
LIBITM_CHECK_ATTRIBUTE_VISIBILITY
LIBITM_CHECK_ATTRIBUTE_DLLEXPORT
LIBITM_CHECK_ATTRIBUTE_ALIAS

# Check linker hardware capability support.
LIBITM_CHECK_LINKER_HWCAP
# If defaulting to -mavx, don't clear hwcaps.
AC_CHECK_DECL([__AVX__], [HWCAP_LDFLAGS=''])

LIBITM_ENABLE_SYMVERS
if test $enable_symvers = gnu; then
  AC_DEFINE(LIBITM_GNU_SYMBOL_VERSIONING, 1,
	    [Define to 1 if GNU symbol versioning is used for libitm.])
fi

# See if we can emit unwind info in the sjlj stub.
GCC_AS_CFI_PSEUDO_OP

# Determine the proper ABI type for size_t.
LIBITM_CHECK_SIZE_T_MANGLING

# Get target configury.
. ${srcdir}/configure.tgt
if test -n "$UNSUPPORTED"; then
  AC_MSG_ERROR([Configuration ${target} is unsupported.])
fi

CFLAGS="$save_CFLAGS $XCFLAGS"

# Check for __sync_val_compare_and_swap, but only after the target has
# had a chance to set XCFLAGS.
LIBITM_CHECK_SYNC_BUILTINS
LIBITM_CHECK_64BIT_SYNC_BUILTINS
LIBITM_CHECK_AS_AVX
LIBITM_CHECK_AS_RTM
LIBITM_CHECK_AS_HTM

GCC_CHECK_ELF_STYLE_WEAKREF

# Cleanup and exit.
CFLAGS="$save_CFLAGS"
AC_CACHE_SAVE

# Add -Wall -Werror if we are using GCC.
if test "x$GCC" = "xyes"; then
  XCFLAGS="$XCFLAGS -Wall -Werror"
fi

XCFLAGS="$XCFLAGS $XPCFLAGS"

AC_SUBST(config_path)
AC_SUBST(XCFLAGS)
AC_SUBST(XLDFLAGS)

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Set up the set of libraries that we need to link against for libitm.
# Note that the GTM_SELF_SPECS in gcc.c will force -pthread for -fgnu-tm,
# which will force linkage against -lpthread (or equivalent for the system).
# That's not 100% ideal, but about the best we can do easily.
if test $enable_shared = yes; then
  link_itm="-litm %{static: $LIBS}"
else
  link_itm="-litm $LIBS"
fi
AC_SUBST(link_itm)

AM_CONDITIONAL([ARCH_ARM], [test "$ARCH" = arm])
AM_CONDITIONAL([ARCH_X86], [test "$ARCH" = x86])
AM_CONDITIONAL([ARCH_X86_AVX], [test "$libitm_cv_as_avx" = yes])
AM_CONDITIONAL([ARCH_FUTEX], [test $enable_linux_futex = yes])

AC_CONFIG_FILES(Makefile testsuite/Makefile libitm.spec)
AC_OUTPUT
