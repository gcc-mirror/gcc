dnl Check:
dnl * If we have gettimeofday;
dnl * If we have struct timezone for use in calling it;
dnl * If calling it with a timezone pointer actually works -- this is deemed
dnl   obsolete or undefined on some systems which say you should use a null
dnl   pointer -- and undefine HAVE_TIMEZONE if so;
dnl * Whether it only takes one arg.
AC_DEFUN([LIBGFOR_GETTIMEOFDAY], [
  AC_CHECK_FUNCS(gettimeofday)
  if test "$ac_cv_func_gettimeofday" = yes; then
    AC_CACHE_CHECK([for struct timezone], gfor_cv_struct_timezone,
      [AC_TRY_COMPILE([#include <sys/time.h>],
      [struct timezone tz;],
      gfor_cv_struct_timezone=yes, gfor_cv_struct_timezone=no)])
    if test $gfor_cv_struct_timezone = yes; then
      dnl It may be that we can't call gettimeofday with a non-null pointer.
      dnl In that case we'll lie about struct timezone.
      AC_TRY_RUN([
#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
main ()
{
  struct timeval time;
  struct timezone dummy;
  if (gettimeofday (&time, &dummy))
    exit (1);
  else
    exit (0);
}],
        [gfor_have_struct_timezone=yes], [gfor_have_struct_timezone=no],
        [gfor_have_struct_timezone=yes])
      if test $gfor_have_struct_timezone = yes; then
        AC_DEFINE(HAVE_TIMEZONE, 1, [Do we have struct timezone])
      fi
    fi
    AC_REQUIRE([AC_HEADER_TIME])
    AC_CACHE_CHECK(whether gettimeofday can accept two arguments,
      emacs_cv_gettimeofday_two_arguments,
      AC_TRY_LINK([
#ifdef TIME_WITH_SYS_TIME
#include <sys/time.h>
#include <time.h>
#else
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>
#endif
#endif
      ],
      [
      struct timeval time;
#ifdef HAVE_TIMEZONE
      struct timezone dummy;
#define DUMMY &dummy
#else
#define DUMMY NULL
#endif
      gettimeofday (&time, DUMMY);],
      emacs_cv_gettimeofday_two_arguments=yes,
      emacs_cv_gettimeofday_two_arguments=no))
    if test $emacs_cv_gettimeofday_two_arguments = no; then
      AC_DEFINE(GETTIMEOFDAY_ONE_ARGUMENT, 1,
        [Does gettimeofday take a single argument])
    fi
  fi])

sinclude(../libtool.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
AC_DEFUN([AC_LIBTOOL_DLOPEN])
AC_DEFUN([AC_PROG_LD])
])

