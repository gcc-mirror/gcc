dnl Check whether the target supports hidden visibility.
AC_DEFUN([LIBGA68_CHECK_ATTRIBUTE_VISIBILITY], [
  AC_CACHE_CHECK([whether the target supports hidden visibility],
		 libga68_cv_have_attribute_visibility, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[void __attribute__((visibility("hidden"))) foo(void) { }]], [])],
		    [libga68_cv_have_attribute_visibility=yes],
		    [libga68_cv_have_attribute_visibility=no])
  CFLAGS="$save_CFLAGS"])
  if test $libga68_cv_have_attribute_visibility = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_VISIBILITY, 1,
      [Define to 1 if the target supports __attribute__((visibility(...))).])
  fi])
