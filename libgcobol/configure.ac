# Configure script for libgcobol.
# Adapted by James K. Lowden from configure script for libalg68.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Configure looks for the existence of this file to auto-config each language.
# We define several parameters used by configure:

# Process this file with autoreconf to produce a configure script.

AC_INIT(package-unused, version-unused,,libgcobol)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_HEADER(config.h)

AC_CONFIG_MACRO_DIRS([m4])

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
ACX_NONCANONICAL_TARGET

AM_ENABLE_MULTILIB(, ..)

target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_MAINTAINER_MODE

AM_INIT_AUTOMAKE([1.15.1 no-define foreign no-dist -Wall -Wno-portability])

AC_MSG_CHECKING([for --enable-libgcobol])
AC_ARG_ENABLE(libgcobol,
  [AS_HELP_STRING([--enable-libgcobol], [Enable libgcobol])])
AC_MSG_RESULT($enable_libgcobol)

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
AS_HELP_STRING([--enable-version-specific-runtime-libs],
[Specify that runtime libraries should be installed in a compiler-specific directory]),
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

GCC_NO_EXECUTABLES

AC_USE_SYSTEM_EXTENSIONS

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

LT_INIT

AC_LIBTOOL_DLOPEN
LT_LIB_M

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

# This library is written in C++ and needs to link with the C++ runtime.
AC_LANG([C++])

# We must force CC/CXX to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CXX
AC_PROG_COBOL

m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CXXFLAGS)
AC_SUBST(COBC)

GCC_WITH_TOOLEXECLIBDIR

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_noncanonical)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
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

# For iconv support.
AM_ICONV

# Checks for header files.
AC_CHECK_HEADERS(malloc.h)

AC_CHECK_SIZEOF([void *])

# Get target configury.
unset LIBGCOBOL_SUPPORTED
. ${srcdir}/configure.tgt

# Decide if it's usable.
case $LIBGCOBOL_SUPPORTED:$enable_libgcobol in
*:no)  use_libgcobol=no  ;;
*:yes) use_libgcobol=yes ;;
yes:*) use_libgcobol=yes ;;
*:*)   use_libgcobol=no  ;;
esac

# -----------------
# __int128 support
# -----------------

AC_CACHE_CHECK([whether __int128 is supported], [libgcobol_cv_have_int128],
  [GCC_TRY_COMPILE_OR_LINK([
    __int128 foo (__int128 )
    {
    __int128 aaa;
     return (__int128) aaa;
    }

    __int128 bar (__int128 )
    {
    __int128 aaa;
     return (__int128) aaa;
    }
  ],[
    foo (1);
    bar (1);
  ],[
    libgcobol_cv_have_int128=yes
  ],[
    libgcobol_cv_have_int128=no
])])

AM_CONDITIONAL(BUILD_LIBGCOBOL, [test "x$use_libgcobol" = xyes && test "x$libgcobol_cv_have_int128" = xyes])

# Check if functions are available in libc before adding extra libs.
AC_SEARCH_LIBS([malloc], [c])
AC_SEARCH_LIBS([clock_gettime], [c rt])

# libgcobol soname version
LIBGCOBOL_VERSION=2:0:0
AC_SUBST(LIBGCOBOL_VERSION)

## added, currently unused.
# VERSION_SUFFIX=$(echo $LIBGCOBOL_VERSION | tr  ':' '.' )
# AC_SUBST(VERSION_SUFFIX)
## end added

extra_ldflags_libgcobol=
case $host in
  *-*-darwin*)
    extra_ldflags_libgcobol=-Wl,-U,___cobol_main ;;
  *) ;;
esac
AC_SUBST(extra_ldflags_libgcobol)

# Do not add ELF linker options on other targets, or embed build-host paths
# in programs produced by a cross compiler.  Native Linux installations can
# find their runtime without changing the system-wide loader configuration.
AC_ARG_ENABLE([libgcobol-rpath],
  [AS_HELP_STRING([--disable-libgcobol-rpath],
    [do not embed the native Linux libgcobol installation directory in RUNPATH])],
  [case $enableval in
     yes|no) ;;
     *) AC_MSG_ERROR([--enable-libgcobol-rpath expects yes or no]) ;;
   esac],
  [enable_libgcobol_rpath=yes])
LIBGCOBOL_RPATH_SPEC=
case $host in
  *-*-linux*)
    if test x$enable_libgcobol_rpath = xyes \
       && test x$enable_shared = xyes \
       && { test -z "$with_cross_host" || test x$with_cross_host = xno; }; then
      LIBGCOBOL_RPATH_SPEC='%{!static:%{!static-pie:--enable-new-dtags -rpath @LIBGCOBOL_RUNTIME_DIR@}}'
      # RUNPATH is not transitive.  Each runtime DSO needs its own entry
      # for dependencies installed alongside it (for example libquadmath).
      extra_ldflags_libgcobol="$extra_ldflags_libgcobol"' -Wl,--enable-new-dtags -Wl,-rpath,$(toolexeclibdir)'
    fi
    ;;
esac
AC_SUBST(LIBGCOBOL_RPATH_SPEC)

AC_CHECK_HEADERS_ONCE(floatingpoint.h ieeefp.h fenv.h fptrap.h \
complex.h stdlib.h errno.h)

# Look for a way to represent the program name
# First, check the GLIBC case
AC_CHECK_DECLS([program_invocation_short_name], , ,[
#if HAVE_ERRNO_H
# define _GNU_SOURCE
# include <errno.h>
#endif
])

# Check an alternate
AC_CHECK_FUNCS_ONCE(getprogname)

# These are GLIBC
AC_CHECK_FUNCS_ONCE(random_r srandom_r initstate_r setstate_r)

# Some functions we check to figure out if the libc Float128 support
# is adequate.

# These are C23.
AC_CHECK_FUNCS_ONCE(strfromf32 strfromf64)

# These are GLIBC.
AC_CHECK_FUNCS_ONCE(strtof128 strfromf128)
# We need to make sure to check libc before adding libm.
libgcobol_have_sinf128=no
AC_SEARCH_LIBS([sinf128], [c m], libgcobol_have_sinf128=yes)
libgcobol_have_cacosf128=no
AC_SEARCH_LIBS([cacosf128], [c m], libgcobol_have_cacosf128=yes)

# These are libxml2.
AC_ARG_WITH([target-libxml2],
[AS_HELP_STRING([--with-target-libxml2=PATHLIST],
		[specify prefix directory for installed libxml2 package.
		 Equivalent to --with-target-libxml2-include=PATH/include
		 plus --with-target-libxml2-lib=PATH/lib])])
AC_ARG_WITH([target-libxml2-include],
[AS_HELP_STRING([--with-target-libxml2-include=PATHLIST],
		[specify directories for installed libxml2 include files])])
AC_ARG_WITH([target-libxml2-lib],
[AS_HELP_STRING([--with-target-libxml2-lib=PATHLIST],
		[specify directories for installed libxml2 library])])

if test "x$use_libgcobol" = xyes && test "x$libgcobol_cv_have_int128" = xyes; then
  AC_MSG_CHECKING([for libxml2])
  if test "x$with_target_libxml2$with_target_libxml2_include$with_target_libxml2_lib" = x; then
    dnl no libxml2 options, assuming libxml2 in default locations
    LIBXML2_CPPFLAGS="-I /usr/include/libxml2"
    LIBXML2_LIBS="-lxml2"
  else
    dnl libxml2 options passed by configure flags
    if test "x$with_target_libxml2_include" = x && test "x$with_target_libxml2_lib" != x; then
      AC_MSG_ERROR([found --with-target-libxml2-lib but --with-target-libxml2-include missing])
    elif test "x$with_target_libxml2_include" != x && test "x$with_target_libxml2_lib" = x; then
      AC_MSG_ERROR([found --with-target-libxml2-include but --with-target-libxml2-lib missing])
    else
      AC_MSG_RESULT([using paths configured with --with-target-libxml2 options])
    fi
    mldir=`${CC-gcc} --print-multi-directory 2>/dev/null`
    libxml2_val=
    if test "x$with_target_libxml2" != x; then
      for i in `echo $with_target_libxml2 | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          libxml2_val=$d
        fi
      done
      if test "x$libxml2_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-libxml2])
      fi
      libxml2_inc_dir="$libxml2_val/include"
      libxml2_lib_dir="$libxml2_val/lib"
    fi
    libxml2_val=
    if test "x$with_target_libxml2_include" != x; then
      for i in `echo $with_target_libxml2_include | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i; fallback=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          libxml2_val=$d
        fi
      done
      if test "x$libxml2_val" = x && test "x$libxml2_inc_dir" = x && test "x$fallback" != x; then
        libxml2_inc_dir="$fallback"
      elif test "x$libxml2_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-libxml2-include])
      else
        libxml2_inc_dir="$libxml2_val"
      fi
    fi
    libxml2_val=
    if test "x$with_target_libxml2_lib" != x; then
      for i in `echo $with_target_libxml2_lib | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          libxml2_val=$d
        fi
      done
      if test "x$libxml2_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-libxml2-lib])
      fi
      libxml2_lib_dir="$libxml2_val"
    fi
    if test "x$libxml2_inc_dir" = x; then
      AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-libxml2-include])
    fi
    if test "x$libxml2_lib_dir" = x; then
      AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-libxml2-lib])
    fi
    LIBXML2_CPPFLAGS="-I$libxml2_inc_dir"
    LIBXML2_LIBS="-L$libxml2_lib_dir -lxml2"
    AC_MSG_RESULT([found])
  fi
else
  LIBXML2_CPPFLAGS=
  LIBXML2_LIBS=
fi
AC_SUBST(LIBXML2_CPPFLAGS)
AC_SUBST(LIBXML2_LIBS)

# Copied from gcc/configure.ac. 2025-06-05 R.J.Dubner
# At least for glibc, clock_gettime is in librt.  But don't pull that
# in if it still doesn't give us the function we want.
ac_cv_func_clock_gettime=no
if test $ac_cv_func_clock_gettime = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [LIBS="-lrt $LIBS"
     AC_DEFINE(HAVE_CLOCK_GETTIME, 1,
	       [Define to 1 if you have the `clock_gettime' function.])])
fi

have_iec_60559_libc_support=no
if test "x$ac_cv_func_strtof128$ac_cv_func_strfromf128" = xyesyes \
   && test "x$libgcobol_have_sinf128$libgcobol_have_cacosf128" = xyesyes; then
  have_iec_60559_libc_support=yes
fi

# Check whether libquadmath should be used
AC_ARG_ENABLE(libquadmath,
AS_HELP_STRING([--disable-libquadmath],
  [disable libquadmath support for libgcobol]),
ENABLE_LIBQUADMATH_SUPPORT=$enableval,
if test "x$have_iec_60559_libc_support" = xyes; then
  ENABLE_LIBQUADMATH_SUPPORT=default
else
  ENABLE_LIBQUADMATH_SUPPORT=yes
fi)
enable_libquadmath_support=
if test "${ENABLE_LIBQUADMATH_SUPPORT}" = "no" ; then
  enable_libquadmath_support=no
elif test "${ENABLE_LIBQUADMATH_SUPPORT}" = "default" ; then
  enable_libquadmath_support=default
fi
LIBGCOBOL_CHECK_FLOAT128

# struct tm tm_zone is a POSIX.1-2024 addition.
AC_CHECK_MEMBERS([struct tm.tm_zone],,,[#include <time.h>])

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Add dependencies for libgcobol.spec file, this might be excessive - since
# the spec file might not use all of them.
SPEC_LIBGCOBOL_DEPS="$LIBS"
AC_SUBST(SPEC_LIBGCOBOL_DEPS)

AC_CONFIG_FILES([
Makefile
libgcobol.spec.conf:libgcobol.spec.in
])

AC_MSG_NOTICE([libgcobol has been configured.])

AC_OUTPUT
