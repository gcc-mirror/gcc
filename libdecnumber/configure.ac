#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(libdecnumber, [ ], gcc-bugs@gcc.gnu.org, libdecnumber)
AC_CONFIG_SRCDIR(decNumber.h)
AC_CONFIG_MACRO_DIR(../config)

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
			  -Wmissing-format-attribute])
ACX_PROG_CC_WARNING_ALMOST_PEDANTIC([-Wno-long-long])

# Only enable with --enable-werror-always until existing warnings are
# corrected.
ACX_PROG_CC_WARNINGS_ARE_ERRORS([manual])

# Checks for header files.
AC_CHECK_HEADERS(ctype.h stddef.h string.h stdio.h)

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

# Output.

AC_CONFIG_HEADERS(config.h:config.in, [echo timestamp > stamp-h1])
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
