# Process this file with autoconf to produce a configure script.
# Copyright (C) 2006-2020 Free Software Foundation, Inc.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# This requires that you have your environment set-up to use explicit
# versions of automake and autoconf.
#
#    export ACLOCAL=/usr/bin/aclocal-1.15
#    export AUTOMAKE=/usr/bin/automake-1.15
#    export AUTOM4TE=/usr/bin/autom4te2.69
#    export AUTOCONF=/usr/bin/autoconf2.69
#
#    autoreconf2.69
#

AC_INIT(package-unused, version-unused,, libphobos)
AC_CONFIG_SRCDIR(libdruntime/gcc/attribute.d)
AC_CONFIG_HEADERS(config.h)

AM_ENABLE_MULTILIB(, ..)
AC_CANONICAL_SYSTEM
AC_USE_SYSTEM_EXTENSIONS

target_alias=${target_alias-$target}
AC_SUBST(target_alias)

# 1.11.1: Require that version of automake.
# foreign: Don't require README, INSTALL, NEWS, etc.
# no-define: Don't define PACKAGE and VERSION.
# no-dependencies: Don't generate automatic dependencies.
#    (because it breaks when using bootstrap-lean, since some of the
#    headers are gone at "make install" time).
# subdir-objects: Build objects in sub-directories.
# -Wall: Issue all automake warnings.
# -Wno-portability: Don't warn about constructs supported by GNU make.
#    (because GCC requires GNU make anyhow).
#  -Wno-override: Overrides used in testsuite.
AM_INIT_AUTOMAKE([1.11.1 foreign no-dist no-define no-dependencies subdir-objects -Wall -Wno-portability -Wno-override])

m4_rename([_AC_ARG_VAR_PRECIOUS],[glibd_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AM_PROG_AS
AC_PROG_CC
AC_PROG_GDC
WITH_LOCAL_DRUNTIME([GDC_CHECK_COMPILE], [])

m4_rename_force([glibd_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)
AM_MAINTAINER_MODE
AC_PROG_RANLIB
AC_PROG_INSTALL
AC_PROG_MAKE_SET

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
AC_SUBST(CET_FLAGS)

# This should be inherited in the recursive make, but ensure it is defined.
test "$AR" || AR=ar
AC_SUBST(AR)

CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)
AC_SUBST(CFLAGS_FOR_BUILD)

# Enable libtool
LT_INIT(dlopen)
AM_PROG_LIBTOOL
WITH_LOCAL_DRUNTIME([LT_LANG([D])], [])

# libtool variables for Phobos shared and position-independent compiles.
#
# Use phobos_compiler_shared_flag to designate the compile-time flags for
# creating shared objects. Default: -fversion=Shared.
#
# Use phobos_compiler_pic_flag to designate the compile-time flags for
# creating position-independent objects. This varies with the target
# hardware and operating system, but is often: -fPIC.
#
# The distinction between pic and shared compilation flags is not present in
# libtool, and so we make it here.  How it is handled is that in shared
# compilations the `lt_prog_compiler_pic_D' variable is used to instead
# ensure that conditional compilation of shared runtime code is compiled in.
# The original PIC flags are then used in the compilation of every object.
#
# Why are objects destined for libgphobos.a compiled with -fPIC?
# Because -fPIC is not harmful to use for objects destined for static
# libraries. In addition, using -fPIC will allow the use of static
# libgphobos.a in the creation of other D shared libraries.
if test "$enable_shared" = yes; then
  phobos_compiler_pic_flag="$lt_prog_compiler_pic_D"
  phobos_compiler_shared_flag="-fversion=Shared"
else
  phobos_compiler_pic_flag=
  phobos_compiler_shared_flag=
fi
AC_SUBST(phobos_compiler_pic_flag)
AC_SUBST(phobos_compiler_shared_flag)

# Override the libtool's pic_flag and pic_mode.
# Do this step after AM_PROG_LIBTOOL, but before AC_OUTPUT.
# NB: this impacts --with-pic and --without-pic.
lt_prog_compiler_pic_D="$phobos_compiler_shared_flag"
pic_mode='default'

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# libphobos/libdruntime specific options and feature detection
DRUNTIME_CONFIGURE
DRUNTIME_MULTILIB
DRUNTIME_WERROR
DRUNTIME_CPU_SOURCES
DRUNTIME_OS_SOURCES
DRUNTIME_OS_THREAD_MODEL
DRUNTIME_OS_ARM_EABI_UNWINDER
DRUNTIME_OS_MINFO_BRACKETING
DRUNTIME_OS_DLPI_TLS_MODID
DRUNTIME_OS_LINK_SPEC
DRUNTIME_LIBRARIES_CLIB

WITH_LOCAL_DRUNTIME([
  AC_LANG_PUSH([D])
  AC_SEARCH_LIBS([malloc], [c])
  AC_SEARCH_LIBS([pthread_create], [pthread])
  AC_SEARCH_LIBS([cosf], [m])
  AC_SEARCH_LIBS([clock_gettime], [rt])
  DRUNTIME_ENABLE_ATOMIC_BUILTINS
  AC_LANG_POP([D])
], [-nophoboslib])

DRUNTIME_LIBRARIES_ATOMIC
DRUNTIME_LIBRARIES_BACKTRACE
DRUNTIME_LIBRARIES_DLOPEN
DRUNTIME_LIBRARIES_NET
DRUNTIME_LIBRARIES_UCONTEXT
DRUNTIME_LIBRARIES_ZLIB
DRUNTIME_INSTALL_DIRECTORIES

AC_MSG_CHECKING([for --enable-libphobos])
AC_ARG_ENABLE(libphobos,
  [AS_HELP_STRING([--enable-libphobos], [Enable libphobos])])
AC_MSG_RESULT($enable_libphobos)

AC_MSG_CHECKING([for --with-libphobos-druntime-only])
AC_ARG_WITH(libphobos-druntime-only,
  AS_HELP_STRING([--with-libphobos-druntime-only={yes,no,auto}],
		 [build only the druntime library (default: auto)]),,
	    [with_libphobos_druntime_only=auto])
AC_MSG_RESULT($with_libphobos_druntime_only)

case "$with_libphobos_druntime_only" in
  yes|no|auto) ;;
  *) AC_MSG_ERROR([Invalid argument for --with-libphobos-druntime-only]) ;;
esac

# See if supported.
unset LIBPHOBOS_SUPPORTED
unset LIBDRUNTIME_ONLY
AC_MSG_CHECKING([for host support for libphobos])
. ${srcdir}/configure.tgt
case ${host} in
  x86_64-*-solaris2.* | i?86-*-solaris2.*)
    # libphobos doesn't compile with the Solaris/x86 assembler due to a
    # relatively low linelength limit.
    as_prog=`$CC -print-prog-name=as`
    if test -n "$as_prog" && $as_prog -v /dev/null 2>&1 | grep GNU > /dev/null 2>&1; then
      druntime_cv_use_gas=yes;
    else
      druntime_cv_use_gas=no;
    fi
    rm -f a.out
    if test x$druntime_cv_use_gas = xno; then
      LIBPHOBOS_SUPPORTED=no
    fi
    # 64-bit D execution fails with Solaris ld without -z relax=transtls support.
    if test "$druntime_ld_gld" = "no" && test "$druntime_ld_relax_transtls" = "no"; then
      LIBPHOBOS_SUPPORTED=no
    fi
    ;;
esac
AC_MSG_RESULT($LIBPHOBOS_SUPPORTED)

# Decide if it's usable.
case $LIBPHOBOS_SUPPORTED:$enable_libphobos in
*:no)  use_libphobos=no  ;;
*:yes) use_libphobos=yes ;;
yes:*) use_libphobos=yes ;;
*:*)   use_libphobos=no  ;;
esac
AM_CONDITIONAL(ENABLE_LIBPHOBOS, test x$use_libphobos = xyes)

# Decide if only libdruntime should be built.
case $LIBDRUNTIME_ONLY:$with_libphobos_druntime_only in
*:no)  only_libdruntime=no  ;;
*:yes) only_libdruntime=yes ;;
yes:*) only_libdruntime=yes ;;
*:*)   only_libdruntime=no  ;;
esac
AM_CONDITIONAL(ENABLE_LIBDRUNTIME_ONLY, test x$only_libdruntime = xyes)

# Enable expensive internal checks
AC_ARG_ENABLE(libphobos-checking,
[AS_HELP_STRING([[--enable-libphobos-checking[=LIST]]],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: yes,no,all,none,release.
		 Flags are: assert or other strings])],
[ac_checking_flags="${enableval}"],[ac_checking_flags=release])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	# These set all the flags to specific states
	yes|all) RELEASE_FLAG="-fno-release" ; ASSERT_FLAG= ;;
	no|none|release) RELEASE_FLAG="-frelease" ; ASSERT_FLAG= ;;
	# These enable particular checks
	assert) ASSERT_FLAG="-fassert" ;;
	# Accept
	*) ;;
	esac
done
IFS="$ac_save_IFS"
CHECKING_DFLAGS="$RELEASE_FLAG $ASSERT_FLAG"
AC_SUBST(CHECKING_DFLAGS)

# Add drtbegin.o/drtend.o to startfile/endfile specs in libgphobos.spec
if test "$DCFG_MINFO_BRACKETING" = "false"; then
    DRTSTUFF_SPEC=$srcdir/src/drtstuff.spec
else
    DRTSTUFF_SPEC=/dev/null
fi
AC_SUBST_FILE(DRTSTUFF_SPEC)

# Add dependencies for libgphobos.spec file
SPEC_PHOBOS_DEPS="$LIBS"
AC_SUBST(SPEC_PHOBOS_DEPS)

# Libdruntime / phobos soname version
libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

# Set default flags (after DRUNTIME_WERROR!)
if test -z "$GDCFLAGS"; then
    GDCFLAGS="-g -O2"
fi
AC_SUBST(GDCFLAGS)

WARN_DFLAGS="-Wall $WERROR_FLAG"
AC_SUBST(WARN_DFLAGS)

# Sanity check for the cross-compilation case:
AC_CHECK_HEADER(stdio.h,:,
  [AC_MSG_ERROR([cannot find stdio.h.])])

AC_CONFIG_FILES(Makefile)

AC_CONFIG_FILES(libdruntime/gcc/config.d libdruntime/gcc/libbacktrace.d)
AC_CONFIG_FILES(src/libgphobos.spec)
AC_CONFIG_FILES([testsuite/testsuite_flags],[chmod +x testsuite/testsuite_flags])

# Multilibs need MULTISUBDIR defined correctly in certain makefiles so
# that multilib installs will end up installed in the correct place.
# The testsuite needs it for multilib-aware ABI baseline files.
# To work around this not being passed down from config-ml.in ->
# srcdir/Makefile.am -> srcdir/{src,libdruntime,...}/Makefile.am, manually
# append it here.  Only modify Makefiles that have just been created.
#
# Also, get rid of this simulated-VPATH thing that automake does.
AC_CONFIG_FILES(AC_FOREACH([DIR], [libdruntime src testsuite], [DIR/Makefile ]),
  [cat > vpsed$$ << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   sed -f vpsed$$ $ac_file > tmp$$
   mv tmp$$ $ac_file
   rm vpsed$$
   echo 'MULTISUBDIR =' >> $ac_file
   ml_norecursion=yes
   . ${multi_basedir}/config-ml.in
   AS_UNSET([ml_norecursion])
])

AC_OUTPUT
