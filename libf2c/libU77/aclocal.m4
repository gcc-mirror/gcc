dnl Check:
dnl * If we have gettimeofday;
dnl * If we have struct timezone for use in calling it;
dnl * If calling it with a timezone pointer actually works -- this is deemed
dnl   obsolete or undefined on some systems which say you should use a null
dnl   pointer -- and undefine HAVE_TIMEZONE if so;
dnl * Whether it only takes one arg.
AC_DEFUN(LIBU77_GETTIMEOFDAY, [
  AC_CHECK_FUNCS(gettimeofday)
  if test "$ac_cv_func_gettimeofday" = yes; then
    AC_CACHE_CHECK([for struct timezone], g77_cv_struct_timezone,
      [AC_TRY_COMPILE([#include <sys/time.h>],
      [struct timezone tz;],
      g77_cv_struct_timezone=yes, g77_cv_struct_timezone=no)])
    if test $g77_cv_struct_timezone = yes; then
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
      [AC_DEFINE(HAVE_TIMEZONE)], ,[AC_DEFINE(HAVE_TIMEZONE)])
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
      AC_DEFINE(GETTIMEOFDAY_ONE_ARGUMENT)
    fi
  fi])
