#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT(cpplib, [ ], gcc-bugs@gcc.gnu.org, cpplib)
AC_CONFIG_SRCDIR(ucnid.h)
AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE(foreign)
AM_MAINTAINER_MODE

# Checks for programs.
AC_PROG_CC
AC_PROG_RANLIB

# Checks for header files.
AC_HEADER_TIME
ACX_HEADER_STRING
AC_CHECK_HEADERS(iconv.h locale.h fcntl.h limits.h stddef.h \
	stdlib.h strings.h string.h sys/file.h unistd.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_FUNC_OBSTACK
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_STRUCT_TM
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_FUNCS(putc_unlocked fputc_unlocked fputs_unlocked \
        fwrite_unlocked fprintf_unlocked)
AC_CHECK_DECLS([abort, errno, putc_unlocked, fputc_unlocked,
        fputs_unlocked, fwrite_unlocked, fprintf_unlocked])

# Checks for library functions.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AM_LANGINFO_CODESET
ZW_GNU_GETTEXT_SISTER_DIR

AC_CACHE_CHECK(for uchar, gcc_cv_type_uchar,
[AC_TRY_COMPILE([
#include "ansidecl.h"
#include "system.h"
],
[if ((uchar *)0) return 0;
 if (sizeof(uchar)) return 0;],
ac_cv_type_uchar=yes, ac_cv_type_uchar=no)])
if test $ac_cv_type_uchar = yes; then
  AC_DEFINE(HAVE_UCHAR, 1,
  [Define if <sys/types.h> defines \`uchar'.])
fi

AM_ICONV
AM_CONDITIONAL(ENABLE_NLS, [test "x$enable_nls" != xno])
AC_ARG_ENABLE(checking,
[  --enable-checking      enable expensive run-time checks],,
enable_checking=no)

if test $enable_checking != no ; then
  AC_DEFINE(ENABLE_CHECKING, 1,
[Define if you want more run-time sanity checks.])
fi

m4_changequote(,)
case $target in
	alpha*-*-* | \
	x86_64-*-* | \
	ia64-*-* | \
	hppa*64*-*-* | parisc*64*-*-* | \
	mips*-*-* | \
	mmix-*-* | \
	powerpc*-*-* | \
	rs6000*-*-* | \
	s390*-*-* | \
	sparc64*-*-* | ultrasparc-*-freebsd* | \
	sparcv9-*-solaris2* | \
	sparc-*-solaris2.[789] | sparc-*-solaris2.1[0-9] | \
	sh[123456789l]*-*-*)
		need_64bit_hwint=yes ;;
	*)
		need_64bit_hwint=no ;;
esac

case $need_64bit_hwint:$ac_cv_sizeof_long in
	*:8 | no:*) host_wide_int=long ;;
	*) host_wide_int='long long' ;;
esac
m4_changequote([,])

AC_DEFINE_UNQUOTED(HOST_WIDE_INT, $host_wide_int,
[Define to the widest efficient host integer type at least
   as wide as the target's size_t type.])

AC_CONFIG_HEADERS(config.h:config.in)
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
