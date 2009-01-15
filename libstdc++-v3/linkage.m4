dnl
dnl This file contains macros for testing linkage.
dnl

dnl
dnl Check to see if the (math function) argument passed is
dnl declared when using the c++ compiler
dnl ASSUMES argument is a math function with ONE parameter
dnl
dnl GLIBCXX_CHECK_MATH_DECL_1
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_1], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <math.h>
		      #ifdef HAVE_IEEEFP_H
		      #include <ieeefp.h>
		      #endif
		     ],
                     [ $1(0);],
                      [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
])


dnl 
dnl Define autoheader template for using the underscore functions
dnl For each parameter, create a macro where if func doesn't exist,
dnl but _func does, then it will "#define func _func".
dnl
dnl GLIBCXX_MAYBE_UNDERSCORED_FUNCS
AC_DEFUN([GLIBCXX_MAYBE_UNDERSCORED_FUNCS], 
[AC_FOREACH([glibcxx_ufunc], [$1],
  [AH_VERBATIM(_[]glibcxx_ufunc,
[#if defined (]AS_TR_CPP(HAVE__[]glibcxx_ufunc)[) && ! defined (]AS_TR_CPP(HAVE_[]glibcxx_ufunc)[)
# define ]AS_TR_CPP(HAVE_[]glibcxx_ufunc)[ 1
# define ]glibcxx_ufunc[ _]glibcxx_ufunc[
#endif])])
])


dnl
dnl Check to see if the (math function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl 3) if not, see if 1) and 2) for argument prepended with '_'
dnl
dnl Define HAVE_CARGF etc if "cargf" is declared and links
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a math function with ONE parameter
dnl
dnl GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1], [
  GLIBCXX_CHECK_MATH_DECL_1($1)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  else
    GLIBCXX_CHECK_MATH_DECL_1(_$1)
    if test x$glibcxx_cv_func__$1_use = x"yes"; then
      AC_CHECK_FUNCS(_$1)
    fi
  fi
  GLIBCXX_MAYBE_UNDERSCORED_FUNCS($1)
])


dnl
dnl Like GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1, but does a bunch of
dnl of functions at once.  It's an all-or-nothing check -- either
dnl HAVE_XYZ is defined for each of the functions, or for none of them.
dnl Doing it this way saves significant configure time.
AC_DEFUN([GLIBCXX_CHECK_MATH_DECLS_AND_LINKAGES_1], [
  define([funclist],patsubst($3,\(\w+\)\(\W*\),\1 ))dnl
  AC_MSG_CHECKING([for $1 functions])
  AC_CACHE_VAL(glibcxx_cv_func_$2_use, [
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AC_TRY_COMPILE([#include <math.h>],
                   patsubst(funclist,[\w+],[\& (0);]),
                   [glibcxx_cv_func_$2_use=yes],
                   [glibcxx_cv_func_$2_use=no])
    AC_LANG_RESTORE])
  AC_MSG_RESULT($glibcxx_cv_func_$2_use)
  if test x$glibcxx_cv_func_$2_use = x"yes"; then
    AC_CHECK_FUNCS(funclist)
  else
    AC_MSG_CHECKING([for _$1 functions])
    AC_CACHE_VAL(glibcxx_cv_func__$2_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <math.h>],
                     patsubst(funclist,[\w+],[_\& (0);]),
                     [glibcxx_cv_func__$2_use=yes],
                     [glibcxx_cv_func__$2_use=no])
      AC_LANG_RESTORE])
    AC_MSG_RESULT($glibcxx_cv_func__$2_use)
    if test x$glibcxx_cv_func__$2_use = x"yes"; then
      AC_CHECK_FUNCS(patsubst(funclist,[\w+],[_\&]))
    fi
  fi
  GLIBCXX_MAYBE_UNDERSCORED_FUNCS(funclist)
  undefine([funclist])
])

dnl
dnl Check to see if the (math function) argument passed is
dnl declared when using the c++ compiler
dnl ASSUMES argument is a math function with TWO parameters
dnl
dnl GLIBCXX_CHECK_MATH_DECL_2
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_2], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <math.h>],
                     [ $1(0, 0);],
                     [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
])

dnl
dnl Check to see if the (math function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl
dnl Define HAVE_CARGF etc if "cargf" is declared and links
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a math function with TWO parameters
dnl
dnl GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2], [
  GLIBCXX_CHECK_MATH_DECL_2($1)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  else
    GLIBCXX_CHECK_MATH_DECL_2(_$1)
    if test x$glibcxx_cv_func__$1_use = x"yes"; then
      AC_CHECK_FUNCS(_$1)
    fi
  fi
  GLIBCXX_MAYBE_UNDERSCORED_FUNCS($1)
])


dnl
dnl Check to see if the (math function) argument passed is
dnl declared when using the c++ compiler
dnl ASSUMES argument is a math function with THREE parameters
dnl
dnl GLIBCXX_CHECK_MATH_DECL_3
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_3], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <math.h>],
                     [ $1(0, 0, 0);],
                     [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
])

dnl
dnl Check to see if the (math function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl
dnl Define HAVE_CARGF etc if "cargf" is declared and links
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a math function with THREE parameters
dnl
dnl GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_3
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_3], [
  GLIBCXX_CHECK_MATH_DECL_3($1)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  else
    GLIBCXX_CHECK_MATH_DECL_3(_$1)
    if test x$glibcxx_cv_func__$1_use = x"yes"; then
      AC_CHECK_FUNCS(_$1)
    fi
  fi
  GLIBCXX_MAYBE_UNDERSCORED_FUNCS($1)
])


dnl
dnl Check to see if the (stdlib function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a stdlib function without parameters
dnl
dnl GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_0
AC_DEFUN([GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_0], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <stdlib.h>],
                     [ $1();],
                     [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  fi
])


dnl
dnl Check to see if the (stdlib function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a stdlib function with TWO parameters
dnl
dnl GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_2
AC_DEFUN([GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_2], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <stdlib.h>],
                     [ $1(0, 0);],
                     [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  fi
  GLIBCXX_MAYBE_UNDERSCORED_FUNCS($1)
])


dnl
dnl Check to see if the (stdlib function) argument passed is
dnl 1) declared when using the c++ compiler
dnl 2) has "C" linkage
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a stdlib function with THREE parameters
dnl
dnl GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_3
AC_DEFUN([GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_3], [
  AC_MSG_CHECKING([for $1 declaration])
  if test x${glibcxx_cv_func_$1_use+set} != xset; then
    AC_CACHE_VAL(glibcxx_cv_func_$1_use, [
      AC_LANG_SAVE
      AC_LANG_CPLUSPLUS
      AC_TRY_COMPILE([#include <stdlib.h>],
                     [ $1(0, 0, 0);],
                     [glibcxx_cv_func_$1_use=yes], [glibcxx_cv_func_$1_use=no])
      AC_LANG_RESTORE
    ])
  fi
  AC_MSG_RESULT($glibcxx_cv_func_$1_use)
  if test x$glibcxx_cv_func_$1_use = x"yes"; then
    AC_CHECK_FUNCS($1)
  fi
])

dnl
dnl Check to see what the underlying c library is like
dnl These checks need to do two things:
dnl 1) make sure the name is declared when using the c++ compiler
dnl 2) make sure the name has "C" linkage
dnl This might seem like overkill but experience has shown that it's not...
dnl
dnl Define HAVE_STRTOLD if "strtold" is declared and links
dnl Define HAVE_STRTOF if "strtof" is declared and links
dnl
dnl GLIBCXX_CHECK_STDLIB_SUPPORT
AC_DEFUN([GLIBCXX_CHECK_STDLIB_SUPPORT], [
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS='-fno-builtin -D_GNU_SOURCE'

  GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_2(strtold)
  GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_2(strtof)

  CXXFLAGS="$ac_save_CXXFLAGS"
])

dnl
dnl Check to see what the underlying c library or math library is like.
dnl These checks need to do two things:
dnl 1) make sure the name is declared when using the c++ compiler
dnl 2) make sure the name has "C" linkage
dnl This might seem like overkill but experience has shown that it's not...
dnl
dnl Define HAVE_CARGF etc if "cargf" is found.
dnl
dnl GLIBCXX_CHECK_MATH_SUPPORT
AC_DEFUN([GLIBCXX_CHECK_MATH_SUPPORT], [
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS='-fno-builtin -D_GNU_SOURCE'

  dnl Check libm
  AC_CHECK_LIB(m, sin, libm="-lm")
  ac_save_LIBS="$LIBS"
  LIBS="$LIBS $libm"

  dnl Check to see if certain C math functions exist.
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isinf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isnan)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(finite)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_3(sincos)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(fpclass)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(qfpclass)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(hypot)

  dnl Check to see if basic C math functions have float versions.
  GLIBCXX_CHECK_MATH_DECLS_AND_LINKAGES_1(float trig,
                                          float_trig,
                                          acosf asinf atanf \
                                          cosf sinf tanf \
                                          coshf sinhf tanhf)
  GLIBCXX_CHECK_MATH_DECLS_AND_LINKAGES_1(float round,
                                          float_round,
                                          ceilf floorf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(expf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isnanf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isinff)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(atan2f)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(fabsf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(fmodf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(frexpf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(hypotf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(ldexpf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(logf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(log10f)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(modff)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(modf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(powf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(sqrtf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_3(sincosf)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(finitef)

  dnl Check to see if basic C math functions have long double versions.
  GLIBCXX_CHECK_MATH_DECLS_AND_LINKAGES_1(long double trig,
                                          long_double_trig,
                                          acosl asinl atanl \
                                          cosl sinl tanl \
                                          coshl sinhl tanhl)
  GLIBCXX_CHECK_MATH_DECLS_AND_LINKAGES_1(long double round,
                                          long_double_round,
                                          ceill floorl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isnanl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(isinfl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(atan2l)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(expl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(fabsl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(fmodl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(frexpl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(hypotl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(ldexpl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(logl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(log10l)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(modfl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_2(powl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(sqrtl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_3(sincosl)
  GLIBCXX_CHECK_MATH_DECL_AND_LINKAGE_1(finitel)

  LIBS="$ac_save_LIBS"
  CXXFLAGS="$ac_save_CXXFLAGS"
])
