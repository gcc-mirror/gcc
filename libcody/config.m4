# Nathan's Common Config -*- mode:autoconf -*-
# Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
# License: Apache v2.0

# Note: VAR+=... is not dashing, despite its looks

AC_DEFUN([NMS_MAINTAINER_MODE],
[AC_ARG_ENABLE([maintainer-mode],
AS_HELP_STRING([--enable-maintainer-mode],
[enable maintainer mode.  Add rules to rebuild configurey bits]),,
[enable_maintainer_mode=no])
AS_CASE([$enable_maintainer_mode],
  [yes], [maintainer_mode=yes],
  [no], [maintainer=no],
  [AC_MSG_ERROR([unknown maintainer mode $enable_maintainer_mode])])
AC_MSG_CHECKING([maintainer-mode])
AC_MSG_RESULT([$maintainer_mode])
test "$maintainer_mode" = yes && MAINTAINER=yes
AC_SUBST(MAINTAINER)])

AC_DEFUN([NMS_CXX_COMPILER],
[AC_ARG_WITH([compiler],
AS_HELP_STRING([--with-compiler=NAME],[which compiler to use]),
AC_MSG_CHECKING([C++ compiler])
if test "$withval" = "yes" ; then
  AC_MSG_ERROR([NAME not specified])
elif test "$withval" = "no" ; then
  AC_MSG_ERROR([Gonna need a C++ compiler!])
else
  CXX="${withval}"
  AC_MSG_RESULT([$CXX])
fi)])

AC_DEFUN([NMS_CXX_11],
[AC_MSG_CHECKING([whether $CXX is for C++11])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
[#if __cplusplus != 201103
#error "C++11 is required"
#endif
]])],
[AC_MSG_RESULT([yes])],
[CXX_ORIG="$CXX"
CXX="$CXX -std=c++11"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
[#if __cplusplus != 201103
#error "C++11 is required"
#endif
]])],
AC_MSG_RESULT([adding -std=c++11]),
[CXX="$CXX_ORIG"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
[#if __cplusplus > 201103
#error "C++11 is required"
#endif
]])],
AC_MSG_RESULT([> C++11]),
AC_MSG_RESULT([no])
AC_MSG_ERROR([C++11 is required])]))
unset CXX_ORIG])])

AC_DEFUN([NMS_ENABLE_EXCEPTIONS],
[AC_ARG_ENABLE([exceptions],
AS_HELP_STRING([--enable-exceptions],
[enable exceptions & rtti]),,
[enable_exceptions="no"])
AS_CASE([$enable_exceptions],
  [yes], [nms_exceptions=yes],
  [no], [nms_exceptions=no],
  [AC_MSG_ERROR([unknown exceptions $enable_exceptions])])
AC_MSG_CHECKING([exceptions])
AC_MSG_RESULT([$nms_exceptions])
if test "$nms_exceptions" != no ; then
  EXCEPTIONS=yes
fi
AC_SUBST(EXCEPTIONS)])

AC_DEFUN([NMS_LINK_OPT],
[AC_MSG_CHECKING([adding $1 to linker])
ORIG_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS $1"
AC_LINK_IFELSE([AC_LANG_PROGRAM([])],
[AC_MSG_RESULT([ok])],
[LDFLAGS="$ORIG_LDFLAGS"
AC_MSG_RESULT([no])])
unset ORIG_LDFLAGS])

AC_DEFUN([NMS_ENABLE_CHECKING],
[AC_ARG_ENABLE([checking],
AS_HELP_STRING([--enable-checking],
[enable run-time checking]),,
[enable_checking="yes"])
AS_CASE([$enable_checking],
  [yes|all|yes,*], [nms_checking=yes],
  [no|none|release], [nms_checking=],
  [AC_MSG_ERROR([unknown check "$enable_checking"])])
AC_MSG_CHECKING([checking])
AC_MSG_RESULT([${nms_checking:-no}])
if test "$nms_checking" = yes ; then
  AC_DEFINE_UNQUOTED([NMS_CHECKING], [0${nms_checking:+1}], [Enable checking])
fi])
