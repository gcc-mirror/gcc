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
    AC_CACHE_CHECK([whether gettimeofday can accept two arguments],
      emacs_cv_gettimeofday_two_arguments,
      [AC_TRY_LINK([
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
      emacs_cv_gettimeofday_two_arguments=no)])
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

dnl Check whether the target is ILP32.
AC_DEFUN([LIBGFOR_TARGET_ILP32], [
  AC_CACHE_CHECK([whether the target is ILP32], target_ilp32, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="-O2"
  AC_TRY_LINK(,[
if (sizeof(int) == 4 && sizeof(long) == 4 && sizeof(void *) == 4)
  ;
else
  undefined_function ();
               ],
               target_ilp32=yes,
               target_ilp32=no)
  CFLAGS="$save_CFLAGS"])
  if test $target_ilp32 = yes; then
    AC_DEFINE(TARGET_ILP32, 1,
      [Define to 1 if the target is ILP32.])
  fi
  ])

dnl Check whether the target supports hidden visibility.
AC_DEFUN([LIBGFOR_CHECK_ATTRIBUTE_VISIBILITY], [
  AC_CACHE_CHECK([whether the target supports hidden visibility],
		 have_attribute_visibility, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((visibility("hidden"))) foo(void) { }],
		 [], have_attribute_visibility=yes,
		 have_attribute_visibility=no)
  CFLAGS="$save_CFLAGS"])
  if test $have_attribute_visibility = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_VISIBILITY, 1,
      [Define to 1 if the target supports __attribute__((visibility(...))).])
  fi])

dnl Check whether the target supports dllexport
AC_DEFUN([LIBGFOR_CHECK_ATTRIBUTE_DLLEXPORT], [
  AC_CACHE_CHECK([whether the target supports dllexport],
		 have_attribute_dllexport, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((dllexport)) foo(void) { }],
		 [], have_attribute_dllexport=yes,
		 have_attribute_dllexport=no)
  CFLAGS="$save_CFLAGS"])
  if test $have_attribute_dllexport = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_DLLEXPORT, 1,
      [Define to 1 if the target supports __attribute__((dllexport)).])
  fi])

dnl Check whether the target supports symbol aliases.
AC_DEFUN([LIBGFOR_CHECK_ATTRIBUTE_ALIAS], [
  AC_CACHE_CHECK([whether the target supports symbol aliases],
		 have_attribute_alias, [
  AC_TRY_LINK([
#define ULP	STR1(__USER_LABEL_PREFIX__)
#define STR1(x)	STR2(x)
#define STR2(x)	#x
void foo(void) { }
extern void bar(void) __attribute__((alias(ULP "foo")));],
    [bar();], have_attribute_alias=yes, have_attribute_alias=no)])
  if test $have_attribute_alias = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_ALIAS, 1,
      [Define to 1 if the target supports __attribute__((alias(...))).])
  fi])
