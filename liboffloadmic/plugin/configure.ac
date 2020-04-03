# Plugin for offload execution on Intel MIC devices.
#
# Copyright (C) 2014 Free Software Foundation, Inc.
#
# Contributed by Andrey Turetskiy <andrey.turetskiy@intel.com>.
#
# This file is part of the GNU Offloading and Multi Processing Library
# (libgomp).
#
# Libgomp is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
# more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

# Process this file with autoconf to produce a configure script, like so:
# aclocal -I ../.. -I ../../config && autoconf && automake

AC_INIT([Intel MIC Offload Plugin], [1.0], ,[libgomp-plugin-intelmic])

AC_CONFIG_AUX_DIR(../..)

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_INIT_AUTOMAKE([1.9.0 foreign no-dist])

AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CXX
AC_CONFIG_FILES([Makefile])
AM_ENABLE_MULTILIB(, ../..)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Make sure liboffloadmic is enabled
case "$enable_liboffloadmic" in
  host | target)
    ;;
  *)
    AC_MSG_ERROR([Liboffloadmic is disabled]) ;;
esac
AM_CONDITIONAL(PLUGIN_HOST, [test x"$enable_liboffloadmic" = xhost])

# Get accel target and path to build or install tree of accel compiler
accel_search_dir=
accel_target=
if test x"$enable_liboffloadmic" = xhost; then
  for accel in `echo $enable_offload_targets | sed -e 's#,# #g'`; do
    accel_name=`echo $accel | sed 's/=.*//'`
    accel_dir=`echo $accel | grep '=' | sed 's/.*=//'`
    case "$accel_name" in
      *-intelmic-* | *-intelmicemul-*)
	accel_target=$accel_name
	accel_search_dir=$accel_dir
	;;
    esac
  done
  if test x"$accel_target" = x; then
    AC_MSG_ERROR([--enable-offload-targets does not contain intelmic target])
  fi
fi
AC_SUBST(accel_search_dir)
AC_SUBST(accel_target)

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE([version-specific-runtime-libs],
  AC_HELP_STRING([--enable-version-specific-runtime-libs],
		 [Specify that runtime libraries should be installed in a compiler-specific directory]),
  [case "$enableval" in
    yes) enable_version_specific_runtime_libs=yes ;;
    no)  enable_version_specific_runtime_libs=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
   esac],
  [enable_version_specific_runtime_libs=no])
AC_MSG_RESULT($enable_version_specific_runtime_libs)


GCC_WITH_TOOLEXECLIBDIR

# Calculate toolexeclibdir.
# Also toolexecdir, though it's only used in toolexeclibdir.
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

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
# Forbid libtool to hardcode RPATH, because we want to be able to specify
# library search directory using LD_LIBRARY_PATH
hardcode_into_libs=no
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Must be last
AC_OUTPUT
