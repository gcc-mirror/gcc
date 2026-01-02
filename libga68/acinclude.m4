dnl Copyright (C) 2025-2026 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

m4_include([m4/autoconf.m4])

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
