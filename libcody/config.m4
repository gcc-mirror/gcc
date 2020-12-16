# Nathan's Common Config -*- mode:autoconf -*-
# Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
# License: Apache v2.0

# Note: VAR+=... is not dashing, despite its looks

AC_DEFUN([NMS_NOT_IN_SOURCE],
[if test -e configure ; then
AC_MSG_ERROR([Do not build in the source tree.  Reasons])
fi])

# thanks to Zack Weinberg for fixing this!
AC_DEFUN([NMS_TOOLS],
[AC_SUBST([tools], [])
AC_ARG_WITH([tools],
  AS_HELP_STRING([--with-tools=DIR],[tool directory]),
  [AS_CASE([$withval],
    [yes], [AC_MSG_ERROR([--with-tools requires an argument])],
    [no], [:],
    [tools="${withval%/bin}"])])

if test -n "$tools" ; then
  if test -d "$tools/bin"; then
    PATH="$tools/bin:$PATH"
    AC_MSG_NOTICE([Using tools in $tools])
  else
    AC_MSG_ERROR([tool location does not exist])
  fi
fi])

AC_DEFUN([NMS_TOOL_DIRS],
[if test "$tools" && test -d "$tools/include" ; then
  CXX="$CXX -I$tools/include"
fi
if test "$tools" && test -d "$tools/lib" ; then
  toollib="$tools/lib"
  if os=$(CXX -print-multi-os-directory 2>/dev/null) ; then
    toollib="$toollib/${os}"
  fi
  LDFLAGS="$LDFLAGS -L $toollib"
  unset toollib
fi])

AC_DEFUN([NMS_NUM_CPUS],
[AC_MSG_CHECKING([number of CPUs])
AS_CASE([$build],
[*-*-darwin*], [NUM_CPUS=$(sysctl -n hw.ncpu 2>/dev/null)],
[NUM_CPUS=$(grep -c '^processor' /proc/cpuinfo 2>/dev/null)])
test "$NUM_CPUS" = 0 && NUM_CPUS=
AC_MSG_RESULT([${NUM_CPUS:-unknown}])
test "$NUM_CPUS" = 1 && NUM_CPUS=
AC_SUBST(NUM_CPUS)])

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

AC_DEFUN([NMS_CXX_20],
[AC_MSG_CHECKING([whether $CXX is for C++20])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
[#if __cplusplus <= 201703
#error "C++20 is required"
#endif
]])],
[AC_MSG_RESULT([yes])],
[CXX="$CXX -std=c++20"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
[#if __cplusplus <= 201703
#error "C++20 is required"
#endif
]])],
AC_MSG_RESULT([adding -std=c++20]),
AC_MSG_RESULT([no])
AC_MSG_ERROR([C++20 is required])]))

AC_MSG_CHECKING([whether C++20 support is sufficiently advanced])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <version>
// There doesn't seem to be a feature macro for __VA_OPT__ :(
#define VARIADIC(X,...) X __VA_OPT__((__VA_ARGS__))
#define X(Y,Z) 1
int ary[VARIADIC(X,Y,Z)];
#if  __cpp_constinit < 201907
#error "C++20 constinit required"
cpp_constinit is __cpp_constinit
#endif
#if  __cpp_if_constexpr < 201606
#error "C++20 constexpr required"
cpp_constexpr is __cpp_if_constexpr
#endif
#if  __cpp_concepts < 201907
#error "C++20 concepts required"
cpp_concepts is __cpp_concepts
#endif
#if __cpp_structured_bindings < 201606
#error "C++20 structured bindings required"
cpp_structured_bindings is __cpp_structured_bindings
#endif
#if __cpp_lib_int_pow2 < 202002
#error "std::has_single_bit required"
cpp_lib_int_pow2 is __cpp_lib_int_pow2
#endif
]])],
AC_MSG_RESULT([yes 🙂]),
AC_MSG_RESULT([no 🙁])
AC_MSG_ERROR([C++20 support is too immature]))])

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

AC_DEFUN([NMS_BUGURL],
[AC_MSG_CHECKING([bugurl])
AC_ARG_WITH(bugurl,
AS_HELP_STRING([--with-bugurl=URL],[where to report bugs]),
AS_CASE(["$withval"],
  [yes], [AC_MSG_ERROR([--with-bugurl requires an argument])],
  [no], [BUGURL=""],
  [BUGURL="${withval}"]),
[BUGURL="${PACKAGE_BUGREPORT}"])
AC_MSG_RESULT($BUGURL)
AC_DEFINE_UNQUOTED(BUGURL,"$BUGURL",[Bug reporting location])])

AC_DEFUN([NMS_DISTRIBUTION],
[AC_ARG_ENABLE([distribution],
AS_HELP_STRING([--enable-distribution],
[enable distribution.  Inhibit components that prevent distribution]),,
[enable_distribution="no"])
AS_CASE([$enable_distribution],
  [yes], [nms_distribution=yes],
  [no], [nms_distribution=no],
  [AC_MSG_ERROR([unknown distribution $enable_distribution])])
AC_MSG_CHECKING([distribution])
AC_MSG_RESULT([$nms_distribution])])

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

AC_DEFUN([NMS_WITH_BINUTILS],
[AC_MSG_CHECKING([binutils])
AC_ARG_WITH(bfd,
AS_HELP_STRING([--with-bfd=DIR], [location of libbfd]),
if test "$withval" = "yes" ; then
  AC_MSG_ERROR([DIR not specified])
elif test "$withval" = "no" ; then
  AC_MSG_RESULT(installed)
else
  AC_MSG_RESULT(${withval})
  CPPFLAGS="$CPPFLAGS -I${withval}/include"
  LDFLAGS="$LDFLAGS -L${withval}/lib"
fi,
AC_MSG_RESULT(installed))])

AC_DEFUN([NMS_ENABLE_BACKTRACE],
[AC_REQUIRE([NMS_DISTRIBUTION])
AC_ARG_ENABLE([backtrace],
AS_HELP_STRING([--enable-backtrace],[provide backtrace on fatality.]),,
[enable_backtrace="maybe"])
if test "${enable_backtrace:-maybe}" != no ; then
  AC_CHECK_HEADERS(execinfo.h)
  AC_CHECK_FUNCS(backtrace)
  if test "$nms_distribution" = no ; then
    AC_DEFINE([HAVE_DECL_BASENAME], [1], [Needed for demangle.h])
    # libiberty prevents distribution because of licensing
    AC_CHECK_HEADERS([demangle.h libiberty/demangle.h],[break])
    # libbfd prevents distribution because of licensing
    AC_CHECK_HEADERS([bfd.h])
    AC_SEARCH_LIBS([bfd_openr],[bfd],[LIBS="$LIBS -lz -liberty -ldl"],,[-lz -liberty -ldl])
  fi
  if test "$ac_cv_func_backtrace" = yes ; then
    nms_backtrace=yes
    ldbacktrace=-rdynamic
    AC_DEFINE([NMS_BACKTRACE], [1], [Enable backtrace])
  elif test "$enable_backtrace" = yes ; then
    AC_MSG_ERROR([Backtrace unavailable])
  fi
  AC_SUBST([ldbacktrace])
fi
AC_MSG_CHECKING([backtrace])
AC_MSG_RESULT([${nms_backtrace:-no}])])

AC_DEFUN([NMS_CONFIG_FILES],
[CONFIG_FILES="Makefile $1"
SUBDIRS="$2"
for generated in config.h.in configure ; do
  if test $srcdir/configure.ac -nt $srcdir/$generated ; then
    touch $srcdir/$generated
  fi
done
for dir in . $SUBDIRS
do
  CONFIG_FILES="$CONFIG_FILES $dir/Makesub"
  test -f ${srcdir}/$dir/tests/Makesub.in && CONFIG_FILES="$CONFIG_FILES $dir/tests/Makesub"
done
AC_CONFIG_FILES([$CONFIG_FILES])
AC_SUBST(configure_args,[$ac_configure_args])
AC_SUBST(SUBDIRS)
AC_SUBST(CONFIG_FILES)])
