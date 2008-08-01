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

sinclude(../config/acx.m4)
sinclude(../config/override.m4)

AC_INIT
AC_PREREQ([2.59])

AC_CONFIG_SRCDIR([Makefile.in])

# Determine the host, build, and target systems
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Determine the noncanonical target name, for directory use.
ACX_NONCANONICAL_TARGET

# Determine the target- and build-specific subdirectories
GCC_TOPLEV_SUBDIRS

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

AC_ARG_ENABLE([shared],
[AC_HELP_STRING([--disable-shared],
                [don't provide a shared libgnat])],
[
case $enable_shared in
  yes | no) ;;
  *)
    enable_shared=no
    IFS="${IFS=         }"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
    for pkg in $enableval; do
      case $pkg in
        ada | libada)
          enable_shared=yes ;;
      esac
    done
    IFS="$ac_save_ifs"
    ;;
esac
], [enable_shared=yes])
AC_SUBST([enable_shared])

# Need to pass this down for now :-P
AC_PROG_LN_S

# Determine what to build for 'gnatlib'
if test $build = $target \
   && test ${enable_shared} = yes ; then
  # Note that build=target is almost certainly the wrong test; FIXME
  default_gnatlib_target="gnatlib-shared"
else
  default_gnatlib_target="gnatlib-plain"
fi
AC_SUBST([default_gnatlib_target])

AC_PROG_CC
warn_cflags=
if test "x$GCC" = "xyes"; then
  warn_cflags='$(GCC_WARN_CFLAGS)'
fi
AC_SUBST(warn_cflags)

# Output: create a Makefile.
AC_CONFIG_FILES([Makefile])

AC_OUTPUT
