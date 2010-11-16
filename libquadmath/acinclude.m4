dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libgfortran.

dnl Check whether the target supports __sync_*_compare_and_swap.
AC_DEFUN([LIBGOMP_CHECK_SYNC_BUILTINS], [
  AC_CACHE_CHECK([whether the target supports __sync_*_compare_and_swap],
                 libgomp_cv_have_sync_builtins, [
  AC_TRY_LINK([], [int foo; __sync_val_compare_and_swap(&foo, 0, 1);],
              libgomp_cv_have_sync_builtins=yes, libgomp_cv_have_sync_builtins=no)])
  if test $libgomp_cv_have_sync_builtins = yes; then
    AC_DEFINE(HAVE_SYNC_BUILTINS, 1,
              [Define to 1 if the target supports __sync_*_compare_and_swap])
  fi])

dnl Check whether the target supports hidden visibility.
AC_DEFUN([LIBGOMP_CHECK_ATTRIBUTE_VISIBILITY], [
  AC_CACHE_CHECK([whether the target supports hidden visibility],
                 libgomp_cv_have_attribute_visibility, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((visibility("hidden"))) foo(void) { }],
                 [], libgomp_cv_have_attribute_visibility=yes,
                 libgomp_cv_have_attribute_visibility=no)
  CFLAGS="$save_CFLAGS"])
  if test $libgomp_cv_have_attribute_visibility = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_VISIBILITY, 1,
      [Define to 1 if the target supports __attribute__((visibility(...))).])
  fi])

dnl Check whether the target supports dllexport
AC_DEFUN([LIBGOMP_CHECK_ATTRIBUTE_DLLEXPORT], [
  AC_CACHE_CHECK([whether the target supports dllexport],
                 libgomp_cv_have_attribute_dllexport, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((dllexport)) foo(void) { }],
                 [], libgomp_cv_have_attribute_dllexport=yes,
                 libgomp_cv_have_attribute_dllexport=no)
  CFLAGS="$save_CFLAGS"])
  if test $libgomp_cv_have_attribute_dllexport = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_DLLEXPORT, 1,
      [Define to 1 if the target supports __attribute__((dllexport)).])
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
