dnl   Copyright (C) 2014-2024 Free Software Foundation, Inc.
dnl
dnl This file is part of GCC.
dnl
dnl GCC is free software; you can redistribute it and/or modify it under
dnl the terms of the GNU General Public License as published by the Free
dnl Software Foundation; either version 3, or (at your option) any later
dnl version.
dnl
dnl GCC is distributed in the hope that it will be useful, but WITHOUT ANY
dnl WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GCC; see the file COPYING3.  If not see
dnl <http://www.gnu.org/licenses/>.

AC_INIT([libcc1], [version-unused])
AC_CONFIG_SRCDIR([libcc1.cc])
AC_CONFIG_HEADER(cc1plugin-config.h)

AC_CANONICAL_SYSTEM
AC_USE_SYSTEM_EXTENSIONS
# Determine the noncanonical target name, for directory use.
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

# 1.11.1: Require that version of automake.
# foreign: Don't require README, INSTALL, NEWS, etc.
# no-define: Don't define PACKAGE and VERSION.
# -Wall: Issue all automake warnings.
# -Wno-portability: Don't warn about constructs supported by GNU make.
#    (because GCC requires GNU make anyhow).
AM_INIT_AUTOMAKE([1.11.1 foreign no-dist no-define -Wall -Wno-portability])
AM_MAINTAINER_MODE

LT_INIT([disable-static])
AM_PROG_LIBTOOL
AC_PROG_CXX

visibility=
if test "$GXX" = yes; then
  visibility=-fvisibility=hidden
fi
AC_SUBST(visibility)

GCC_CET_HOST_FLAGS(CET_HOST_FLAGS)
AC_SUBST(CET_HOST_FLAGS)

AC_CHECK_DECLS([basename])

GCC_BASE_VER

if test x$with_gcc_major_version_only = xyes ; then
  changequote(,)dnl
  gcc_version=`sed -e 's/^\([0-9]*\).*$/\1/' $srcdir/../gcc/BASE-VER`
  changequote([,])dnl
else
  gcc_version=`cat $srcdir/../gcc/BASE-VER`
fi
AC_SUBST(gcc_version)
AC_SUBST([CONFIG_STATUS_DEPENDENCIES], ['$(top_srcdir)/../gcc/BASE-VER'])

ACX_PROG_CC_WARNING_OPTS([-W -Wall], [WARN_FLAGS])
AC_SUBST(WARN_FLAGS)
ACX_PROG_CC_WARNINGS_ARE_ERRORS([manual])

AC_ARG_VAR(GMPINC,[How to find GMP include files])

libsuffix=
if test "$GXX" = yes; then
  libsuffix=`$CXX -print-multi-os-directory`
fi
AC_SUBST(libsuffix)

dnl Test for -lsocket and -lnsl.  Copied from libgo/configure.ac.
AC_CACHE_CHECK([for socket libraries], libcc1_cv_lib_sockets,
  [libcc1_cv_lib_sockets=
   libcc1_check_both=no
   AC_CHECK_FUNC(connect, libcc1_check_socket=no, libcc1_check_socket=yes)
   if test "$libcc1_check_socket" = "yes"; then
     unset ac_cv_func_connect
     AC_CHECK_LIB(socket, main, libcc1_cv_lib_sockets="-lsocket",
     		  libcc1_check_both=yes)
   fi
   if test "$libcc1_check_both" = "yes"; then
     libcc1_old_libs=$LIBS
     LIBS="$LIBS -lsocket -lnsl"
     unset ac_cv_func_accept
     AC_CHECK_FUNC(accept,
		   [libcc1_check_nsl=no
		    libcc1_cv_lib_sockets="-lsocket -lnsl"])
     unset ac_cv_func_accept
     LIBS=$libcc1_old_libs
   fi
   unset ac_cv_func_gethostbyname
   libcc1_old_libs="$LIBS"
   AC_CHECK_FUNC(gethostbyname, ,
		 [AC_CHECK_LIB(nsl, main,
		 	[libcc1_cv_lib_sockets="$libcc1_cv_lib_sockets -lnsl"])])
   unset ac_cv_func_gethostbyname
   LIBS=$libcc1_old_libs
])
LIBS="$LIBS $libcc1_cv_lib_sockets"

case "$host" in
  *-*-darwin*) darwin_dynamic_lookup=yes ;;
  *) darwin_dynamic_lookup=no ;;
esac
AM_CONDITIONAL(DARWIN_DYNAMIC_LOOKUP, test $darwin_dynamic_lookup = yes)

# If any of these functions are missing, simply don't bother building
# this plugin.
GCC_ENABLE_PLUGINS
AC_CHECK_FUNC(socketpair, , enable_plugin=no)
AC_CHECK_FUNC(select, , enable_plugin=no)
AC_CHECK_FUNC(fork, , enable_plugin=no)
AM_CONDITIONAL(ENABLE_PLUGIN, test $enable_plugin = yes)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
