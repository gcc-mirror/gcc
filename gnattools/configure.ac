# Configure script for gnattools
#   Copyright 2003, 2004, 2009, 2012 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

sinclude(../config/acx.m4)
sinclude(../config/override.m4)

AC_INIT

AC_CONFIG_SRCDIR([Makefile.in])

# Command-line options.
# Very limited version of AC_MAINTAINER_MODE.
AC_ARG_ENABLE([maintainer-mode],
  [AC_HELP_STRING([--enable-maintainer-mode],
                 [enable make rules and dependencies not useful (and
                  sometimes confusing) to the casual installer])],
  [case ${enable_maintainer_mode} in
     yes) MAINT='' ;;
     no) MAINT='#' ;;
     *) AC_MSG_ERROR([--enable-maintainer-mode must be yes or no]) ;;
   esac
   maintainer_mode=${enableval}],
  [MAINT='#'])
AC_SUBST([MAINT])dnl

# Start of actual configure tests

AC_PROG_INSTALL

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET

# Need to pass this down for now :-P
AC_PROG_LN_S

# Target-specific stuff (defaults)
TOOLS_TARGET_PAIRS=
EXTRA_GNATTOOLS=

# Per-target case statement
# -------------------------
case "${target}" in
  *-*-aix*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-aix.adb"
    ;;
  *-*-darwin*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-darwin.adb"
    ;;
  *-*-dragonfly*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-freebsd*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-linux*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-vxworks*)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-cygwin32* | *-*-mingw32* | *-*-pe)
    TOOLS_TARGET_PAIRS="indepsw.adb<indepsw-gnu.adb"
    EXTRA_GNATTOOLS='../../gnatdll$(exeext)'
    ;;
esac

AC_SUBST(TOOLS_TARGET_PAIRS)
AC_SUBST(EXTRA_GNATTOOLS)

# From user or toplevel makefile.
AC_SUBST(ADA_CFLAGS)

# This is testing the CC passed from the toplevel Makefile, not the
# one we will select below.
AC_PROG_CC
warn_cflags=
if test "x$GCC" = "xyes"; then
  warn_cflags='$(GCC_WARN_CFLAGS)'
fi
AC_SUBST(warn_cflags)

# Enable --enable-host-pie
AC_ARG_ENABLE(host-pie,
[AS_HELP_STRING([--enable-host-pie],
		[build host code as PIE])],
[PICFLAG=-fPIE; LD_PICFLAG=-pie], [PICFLAG=-fno-PIE; LD_PICFLAG=-no-pie])
AC_SUBST(PICFLAG)

# Enable --enable-host-bind-now
AC_ARG_ENABLE(host-bind-now,
[AS_HELP_STRING([--enable-host-bind-now],
       [link host code as BIND_NOW])],
[LD_PICFLAG="$LD_PICFLAG -Wl,-z,now"], [])
AC_SUBST(LD_PICFLAG)

# Determine what to build for 'gnattools'.  Test after the above,
# because testing for CC sets the final value of cross_compiling, even
# if we end up using a different CC.  We want to build
# gnattools-native when: (a) this is a native build, i.e.,
# cross_compiling=no, otherwise we know we cannot run binaries
# produced by the toolchain used for the build, not even the binaries
# created within ../gcc/; (b) build and host are the same, otherwise
# this is to be regarded as a cross build environment even if it seems
# that we can run host binaries; (c) host and target are the same,
# otherwise the tools in ../gcc/ generate code for a different
# platform.  If you change this test, be sure to adjust
# ../gcc/ada/gcc-interface/config-lang.in as well.
if test "x$cross_compiling/$build/$host" = "xno/$host/$target" ; then
  default_gnattools_target="gnattools-native"
else
  default_gnattools_target="gnattools-cross"
fi
AC_SUBST([default_gnattools_target])

# Output: create a Makefile.
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
