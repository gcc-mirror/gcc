# Configure script for libada.
#   Copyright 2003, 2004 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

AC_INIT
AC_PREREQ([2.59])

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

sinclude(../config/acx.m4)
ACX_NONCANONICAL_TARGET

# Need to pass this down for now :-P
AC_PROG_LN_S

# Determine x_ada_cflags
case $host in
  hppa*) x_ada_cflags=-mdisable-indexing ;;
  *) x_ada_cflags= ;;
esac
AC_SUBST([x_ada_cflags])

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
  alpha*-dec-vx*) # Unlike all other Vxworks
    ;;
  m68k*-wrs-vx* \
  | powerpc*-wrs-vxworks \
  | sparc*-wrs-vx* \
  | *86-wrs-vxworks \
  | xscale*-wrs-vx* \
  | xscale*-wrs-coff \
  | mips*-wrs-vx*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-vxworks.adb"
    ;;
  sparc-sun-solaris*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-solaris.adb"
    ;;
  *86-*-solaris2*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-solaris.adb"
    ;;
  *86-*-linux* \
  | powerpc*-*-linux*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-linux.adb \
    indepsw.adb<indepsw-linux.adb"
    ;;
  *ia64-*-linux* \
  | alpha*-*-linux )
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-linux.adb \
    indepsw.adb<indepsw-linux.adb"
    ;;
  *x86_64-*-linux* \
  | s390*-*-linux*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-linux.adb"
    ;;
  *86-*-freebsd*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-linux.adb"
    ;;
  mips-sgi-irix*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-irix.adb"
    ;;
  hppa*-hp-hpux10*) # Unlike hpux11
    ;;
  hppa*-hp-hpux11*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-hpux.adb"
    ;;
  *-ibm-aix*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-aix.adb \
    indepsw.adb<indepsw-aix.adb"
    ;;
  alpha*-dec-osf*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-tru64.adb"
    ;;
  alpha*-dec-vms* | alpha*-dec-openvms* | alpha*-dec-vms* \
  | alpha*-hp-vms* | alpha*-hp-openvms* | alpha*-hp-vms*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-vms-alpha.adb \
    symbols.adb<symbols-vms-alpha.adb"

    EXTRA_GNATTOOLS='../../gnatlbr$(exeext) ../../gnatsym$(exeext)'
    ;;
  ia64*-dec-vms* | ia64*-dec-openvms* | ia64*-dec-vms* \
  | ia64*-hp-vms* | ia64*-hp-openvms* | ia64*-hp-vms*)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-vms-ia64.adb \
    symbols.adb<symbols-vms-ia64.adb"

    EXTRA_GNATTOOLS='../../gnatlbr$(exeext) ../../gnatsym$(exeext)'
    ;;
  *-*-cygwin32* | *-*-mingw32* | *-*-pe)
    TOOLS_TARGET_PAIRS="\
    mlib-tgt.adb<mlib-tgt-mingw.adb \
    indepsw.adb<indepsw-mingw.adb"
    EXTRA_GNATTOOLS='../../gnatdll$(exeext)'
    ;;
  powerpc-*-darwin*)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-darwin.adb"
    ;;
  *-*-lynxos)
    TOOLS_TARGET_PAIRS="mlib-tgt.adb<mlib-tgt-lynxos.adb"
    ;;
esac

# Output: create a Makefile.
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
