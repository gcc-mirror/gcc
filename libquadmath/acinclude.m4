dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libgfortran.

sinclude(../libtool.m4)
sinclude(../config/enable.m4)
sinclude(../config/cet.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
AC_DEFUN([AC_LIBTOOL_DLOPEN])
AC_DEFUN([AC_PROG_LD])
])

dnl Check whether POSIX's signgam is defined in math.h.
AC_DEFUN([LIBQUAD_CHECK_MATH_H_SIGNGAM], [
  AC_CACHE_CHECK([whether the math.h includes POSIX's signgam],
                 libgfor_cv_have_math_h_signgam, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <math.h>
void foo(void) { signgam = 1; }]], [])],
                    libgfor_cv_have_math_h_signgam=yes,
                    libgfor_cv_have_math_h_signgam=no)
  CFLAGS="$save_CFLAGS"])
  if test $libgfor_cv_have_math_h_signgam = yes; then
    AC_DEFINE(HAVE_MATH_H_SIGNGAM, 1,
      [Define to 1 if the math.h includes POSIX's signgam.])
  fi])
