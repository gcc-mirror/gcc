# Configure script for libada.
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
AC_PREREQ([2.64])

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

ACX_NONCANONICAL_TARGET

# Need to pass this down for now :-P
AC_PROG_LN_S

# Determine what to build for 'gnattools'
if test $build = $target ; then
  # Note that build=target is almost certainly the wrong test; FIXME
  default_gnattools_target="gnattools-native"
else
  default_gnattools_target="gnattools-cross"
fi
AC_SUBST([default_gnattools_target])

# Target-specific stuff (defaults)
TOOLS_TARGET_PAIRS=
AC_SUBST(TOOLS_TARGET_PAIRS)
EXTRA_GNATTOOLS=
AC_SUBST(EXTRA_GNATTOOLS)

# Per-target case statement
# -------------------------
case "${target}" in
  *-*-aix*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-aix.adb \
    indepsw.adb<indepsw-aix.adb"
    ;;
  *-*-darwin*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-darwin.adb \
    indepsw.adb<indepsw-darwin.adb"
    ;;
  *-*-freebsd*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-linux.adb \
    indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-linux*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-linux.adb \
    indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-lynxos*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-lynxos.adb \
    indepsw.adb<indepsw-gnu.adb"
    ;;
  *-*-solaris*)
    TOOLS_TARGET_PAIRS="mlib-tgt-specific.adb<mlib-tgt-specific-solaris.adb"
    ;;
  *-*-vxworks*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-vxworks.adb \
    indepsw.adb<indepsw-gnu.adb"
    ;;
  hppa*-hp-hpux10*)
    ;;
  hppa*-hp-hpux11*)
    TOOLS_TARGET_PAIRS="mlib-tgt-specific.adb<mlib-tgt-specific-hpux.adb"
    ;;
  ia64-hp-hpux11*)
    TOOLS_TARGET_PAIRS="mlib-tgt-specific.adb<mlib-tgt-specific-ia64-hpux.adb"
    ;;
  alpha*-*-vms* | alpha*-*-openvms*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-vms-alpha.adb \
    symbols.adb<symbols-vms.adb \
    symbols-processing.adb<symbols-processing-vms-alpha.adb"
    EXTRA_GNATTOOLS='../../gnatlbr$(exeext) ../../gnatsym$(exeext)'
    ;;
  ia64-*-vms* | ia64-*-openvms*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-vms-ia64.adb \
    symbols.adb<symbols-vms.adb \
    symbols-processing.adb<symbols-processing-vms-ia64.adb"
    EXTRA_GNATTOOLS='../../gnatlbr$(exeext) ../../gnatsym$(exeext)'
    ;;
  *-*-cygwin32* | *-*-mingw32* | *-*-pe)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt-specific.adb<mlib-tgt-specific-mingw.adb \
    indepsw.adb<indepsw-mingw.adb"
    EXTRA_GNATTOOLS='../../gnatdll$(exeext)'
    ;;
esac

# From user or toplevel makefile.
AC_SUBST(ADA_CFLAGS)

AC_PROG_CC
warn_cflags=
if test "x$GCC" = "xyes"; then
  warn_cflags='$(GCC_WARN_CFLAGS)'
fi
AC_SUBST(warn_cflags)

# Output: create a Makefile.
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
