dnl   Copyright (C) 2014 Free Software Foundation, Inc.
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

AC_PREREQ(2.64)
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

AC_CHECK_DECLS([basename])

gcc_version=`cat $srcdir/../gcc/BASE-VER`
AC_SUBST(gcc_version)

ACX_PROG_CC_WARNING_OPTS([-W -Wall], [WARN_FLAGS])
WARN_FLAGS="$WARN_FLAGS -Werror"
AC_SUBST(WARN_FLAGS)

libsuffix=
if test "$GXX" = yes; then
  libsuffix=`$CXX -print-multi-os-directory`
fi
AC_SUBST(libsuffix)

# If any of these functions are missing, simply don't bother building
# this plugin.
GCC_ENABLE_PLUGINS
AC_CHECK_FUNC(socketpair, , enable_plugin=no)
AC_CHECK_FUNC(select, , enable_plugin=no)
AC_CHECK_FUNC(fork, , enable_plugin=no)
AM_CONDITIONAL(ENABLE_PLUGIN, test $enable_plugin = yes)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
