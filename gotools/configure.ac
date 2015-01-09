# Configure script for gotools.
#   Copyright 2015 Free Software Foundation, Inc.
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

AC_INIT(package-unused, version-unused,, gotools)
AC_PREREQ(2.64)
AC_CONFIG_SRCDIR(Makefile.am)

m4_include([config/go.m4])

# Determine the noncanonical names used for directories.
ACX_NONCANONICAL_BUILD
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET

dnl Autoconf 2.5x and later will set a default program prefix if
dnl --target was used, even if it was the same as --host.  Disable
dnl that behavior.  This must be done before AC_CANONICAL_SYSTEM
dnl to take effect.
test "$host_noncanonical" = "$target_noncanonical" &&
  test "$program_prefix$program_suffix$program_transform_name" = \
    NONENONEs,x,x, &&
  program_transform_name=s,y,y,

AC_CANONICAL_SYSTEM
AC_ARG_PROGRAM

AM_INIT_AUTOMAKE([1.9.3 no-define foreign no-dist -Wall -Wno-portability])
AM_MAINTAINER_MODE

AC_PROG_INSTALL

AC_PROG_CC
AC_PROG_GO

AM_CONDITIONAL(NATIVE, test "$cross_compiling" = no)

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
