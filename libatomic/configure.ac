# Process this file with autoreconf to produce a configure script.
#  Copyright (C) 2012-2024 Free Software Foundation, Inc.
#
#  This file is part of the GNU Atomic Library (libatomic).
#
#  Libatomic is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 3 of the License, or
#  (at your option) any later version.
#
#  Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
#  more details.
#
#  Under Section 7 of GPL version 3, you are granted additional
#  permissions described in the GCC Runtime Library Exception, version
#  3.1, as published by the Free Software Foundation.
#
#  You should have received a copy of the GNU General Public License and
#  a copy of the GCC Runtime Library Exception along with this program;
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
#  <http://www.gnu.org/licenses/>.

AC_INIT([GNU Atomic Library], 1.0,,[libatomic])
AC_CONFIG_HEADER(auto-config.h)

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
LIBAT_ENABLE(version-specific-runtime-libs, no, ,
   [Specify that runtime libraries should be installed in a compiler-specific directory],
   permit yes|no)
AC_MSG_RESULT($enable_version_specific_runtime_libs)

# We would like our source tree to be readonly. However when releases or
# pre-releases are generated, the flex/bison generated files as well as the
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do
# just that.
AC_MSG_CHECKING([for --enable-generated-files-in-srcdir])
LIBAT_ENABLE(generated-files-in-srcdir, no, ,
   [put copies of generated files in source dir intended for creating source
    tarballs for users without texinfo bison or flex.],
   permit yes|no)
AC_MSG_RESULT($enable_generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test "$enable_generated_files_in_srcdir" = yes)


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

GCC_WITH_TOOLEXECLIBDIR

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
      case ${with_toolexeclibdir} in
	no)
	  toolexeclibdir='$(toolexecdir)/lib'
	  ;;
	*)
	  toolexeclibdir=${with_toolexeclibdir}
	  ;;
      esac
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
AC_PROG_INSTALL

# Configure libtool
AM_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AC_SUBST(enable_shared)
AC_SUBST(enable_static)
AM_MAINTAINER_MODE

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=3:0:2
AC_SUBST(libtool_VERSION)

# Check for used threading-model
AC_MSG_CHECKING([for thread model used by GCC])
target_thread_file=`$CC -v 2>&1 | sed -n 's/^Thread model: //p'`
AC_MSG_RESULT([$target_thread_file])

case "$target" in
 *aarch64*)
    ACX_PROG_CC_WARNING_OPTS([-march=armv8-a+lse],[enable_aarch64_lse])
    ;;
esac

AC_SUBST(SYSROOT_CFLAGS_FOR_TARGET)

# Get target configury.
. ${srcdir}/configure.tgt
if test -n "$UNSUPPORTED"; then
  AC_MSG_ERROR([Configuration ${target} is unsupported.])
fi

# Write out the ifunc resolver arg type.
AC_DEFINE_UNQUOTED(IFUNC_RESOLVER_ARGS, $IFUNC_RESOLVER_ARGS,
	[Define ifunc resolver function argument.])

# Disable fallbacks to __sync routines from libgcc.  Otherwise we'll
# make silly decisions about what the cpu can do.
CFLAGS="$save_CFLAGS -fno-sync-libcalls $XCFLAGS"

# Check header files.
AC_STDC_HEADERS
ACX_HEADER_STRING
GCC_HEADER_STDINT(gstdint.h)
AC_CHECK_HEADERS([fenv.h])

# Check for common type sizes
LIBAT_FORALL_MODES([LIBAT_HAVE_INT_MODE])

# Check for compiler builtins of atomic operations.
LIBAT_TEST_ATOMIC_INIT
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_LOADSTORE])
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_TAS])
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_EXCHANGE])
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_CAS])
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_FETCH_ADD])
LIBAT_FORALL_MODES([LIBAT_HAVE_ATOMIC_FETCH_OP])

# Check for target-specific assembly-level support for atomic operations.
LIBAT_TEST_FEAT_AARCH64_LSE128()

AC_C_BIGENDIAN
# I don't like the default behaviour of WORDS_BIGENDIAN undefined for LE.
AH_BOTTOM(
[#ifndef WORDS_BIGENDIAN
#define WORDS_BIGENDIAN 0
#endif])

LIBAT_WORDSIZE

# Check to see if -pthread or -lpthread is needed.  Prefer the former.
# In case the pthread.h system header is not found, this test will fail.
case " $config_path " in
 *" posix "*)
  XPCFLAGS=""
  CFLAGS="$CFLAGS -pthread"
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
     [#include <pthread.h>
      void *g(void *d) { return NULL; }],
     [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
    [XPCFLAGS=" -pthread"],
    [CFLAGS="$save_CFLAGS $XCFLAGS" LIBS="-lpthread $LIBS"
     AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
       [#include <pthread.h>
        void *g(void *d) { return NULL; }],
       [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
      [],
      [AC_MSG_ERROR([Pthreads are required to build libatomic])])])
  CFLAGS="$save_CFLAGS $XPCFLAGS"
  ;;
esac

# See what sort of export controls are available.
LIBAT_CHECK_ATTRIBUTE_VISIBILITY
LIBAT_CHECK_ATTRIBUTE_DLLEXPORT
LIBAT_CHECK_ATTRIBUTE_ALIAS
if test x$try_ifunc = xyes; then
  LIBAT_CHECK_IFUNC
fi

# Check linker support.
LIBAT_ENABLE_SYMVERS

# Cleanup and exit.
CFLAGS="$save_CFLAGS"
AC_CACHE_SAVE

AC_ARG_ENABLE([werror],
  [AS_HELP_STRING([--disable-werror], [disable building with -Werror])])
# Add -Wall -Werror if we are using GCC.
AS_IF([test "x$GCC" = "xyes"],
  [XCFLAGS="$XCFLAGS -Wall"])
AS_IF([test "x$enable_werror" != "xno" && test "x$GCC" = "xyes"],
  [XCFLAGS="$XCFLAGS -Werror"])

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
XCFLAGS="$XCFLAGS $CET_FLAGS"

XCFLAGS="$XCFLAGS $XPCFLAGS"

AC_SUBST(config_path)
AC_SUBST(XCFLAGS)
AC_SUBST(XLDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(SIZES)

# Conditionalize the makefile for this target machine.
tmake_file_=
for f in ${tmake_file}
do
	if test -f ${srcdir}/config/$f
	then
		tmake_file_="${tmake_file_} \$(srcdir)/config/$f"
	fi
done
tmake_file="${tmake_file_}"
AC_SUBST(tmake_file)


AM_CONDITIONAL(HAVE_IFUNC, test x$libat_cv_have_ifunc = xyes)
AM_CONDITIONAL(ARCH_AARCH64_LINUX,
	       [expr "$config_path" : ".* linux/aarch64 .*" > /dev/null])
AM_CONDITIONAL(ARCH_ARM_LINUX,
	       [expr "$config_path" : ".* linux/arm .*" > /dev/null])
AM_CONDITIONAL(ARCH_I386,
	       [test "$ARCH" = x86 && test x$libat_cv_wordsize = x4])
AM_CONDITIONAL(ARCH_X86_64,
	       [test "$ARCH" = x86 && test x$libat_cv_wordsize = x8])

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES(Makefile testsuite/Makefile)
AC_CONFIG_FILES(testsuite/libatomic-site-extra.exp)
AC_OUTPUT
