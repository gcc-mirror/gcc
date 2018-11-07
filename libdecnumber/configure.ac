# configure.ac for libdecnumber			-*- Autoconf -*-
# Process this file with autoconf to generate a configuration script.

# Copyright (C) 2005-2018 Free Software Foundation, Inc.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any
# later #version.

# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License #for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

AC_INIT(libdecnumber, [ ], gcc-bugs@gcc.gnu.org, libdecnumber)
AC_CONFIG_SRCDIR(decNumber.h)
AC_CONFIG_MACRO_DIR(../config)
AC_CONFIG_AUX_DIR(..)

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_CC
AC_PROG_RANLIB

MISSING=`cd $ac_aux_dir && ${PWDCMD-pwd}`/missing
AC_CHECK_PROGS([ACLOCAL], [aclocal], [$MISSING aclocal])
AC_CHECK_PROGS([AUTOCONF], [autoconf], [$MISSING autoconf])
AC_CHECK_PROGS([AUTOHEADER], [autoheader], [$MISSING autoheader])

# Figure out what compiler warnings we can enable.
# See config/warnings.m4 for details.

ACX_PROG_CC_WARNING_OPTS([-W -Wall -Wwrite-strings -Wstrict-prototypes \
			  -Wmissing-prototypes -Wold-style-definition \
			  -Wmissing-format-attribute -Wcast-qual])
ACX_PROG_CC_WARNING_ALMOST_PEDANTIC([-Wno-long-long])

ACX_PROG_CC_WARNING_OPTS([-fno-lto], [nolto_flags])

# Only enable with --enable-werror-always until existing warnings are
# corrected.
ACX_PROG_CC_WARNINGS_ARE_ERRORS([manual])

# Checks for header files.
AC_CHECK_HEADERS(ctype.h stddef.h string.h stdio.h)
GCC_HEADER_STDINT(gstdint.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_OFF_T
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)

# Checks for library functions.
AC_HEADER_STDC

AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable rules only needed by maintainers],,
enable_maintainer_mode=no)

if test "x$enable_maintainer_mode" = xno; then
  MAINT='#'
else
  MAINT=
fi
AC_SUBST(MAINT)

AC_CANONICAL_TARGET

# Default decimal format
# If you change the defaults here, be sure to change them in the GCC directory also
AC_MSG_CHECKING([for decimal floating point])

GCC_AC_ENABLE_DECIMAL_FLOAT([$target])

# Use default_decimal_float for dependency.
enable_decimal_float=$default_decimal_float

# If BID is being used, additional objects should be linked in.
if test x$enable_decimal_float = xbid; then
  ADDITIONAL_OBJS="$ADDITIONAL_OBJS \$(bid_OBJS)"
else
  ADDITIONAL_OBJS=
fi

AC_MSG_RESULT($enable_decimal_float)
AC_SUBST(enable_decimal_float)
AC_SUBST(ADDITIONAL_OBJS)

AC_C_BIGENDIAN

# Enable --enable-host-shared.
AC_ARG_ENABLE(host-shared,
[AS_HELP_STRING([--enable-host-shared],
		[build host code as shared libraries])],
[PICFLAG=-fPIC], [PICFLAG=])
AC_SUBST(PICFLAG)

# Output.

AC_CONFIG_HEADERS(config.h:config.in, [echo timestamp > stamp-h1])
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
