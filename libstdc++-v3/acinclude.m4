dnl
dnl Initialize configure bits.
dnl
dnl Define OPTLEVEL='-O2' if new inlining code present.
dnl
dnl GLIBCPP_CONFIGURE
AC_DEFUN(GLIBCPP_CONFIGURE, [
  dnl Default to --enable-multilib
  AC_ARG_ENABLE(multilib,
  [  --enable-multilib       build hella library versions (default)],
  [case "${enableval}" in
    yes) multilib=yes ;;
    no)  multilib=no ;;
    *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
   esac], [multilib=yes])dnl

  dnl We may get other options which we dont document:
  dnl --with-target-subdir, --with-multisrctop, --with-multisubdir
  if test "[$]{srcdir}" = "."; then
    if test "[$]{with_target_subdir}" != "."; then
      glibcpp_basedir="[$]{srcdir}/[$]{with_multisrctop}../$1"
    else
      glibcpp_basedir="[$]{srcdir}/[$]{with_multisrctop}$1"
    fi
  else
    glibcpp_basedir="[$]{srcdir}/$1"
  fi
  AC_SUBST(glibcpp_basedir)

  AC_CANONICAL_HOST

  AM_INIT_AUTOMAKE(libstdc++, 2.90.8)

# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(LIB_AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])dnl
dnl Fool anybody using AC_PROG_CC.
AC_PROVIDE([AC_PROG_CC])
AC_CHECK_PROG(CC, gcc, gcc)
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
  test -z "$CC" && AC_MSG_ERROR([no acceptable cc found in \$PATH])
fi

AC_PROG_CC_GNU

if test $ac_cv_prog_gcc = yes; then
  GCC=yes
dnl Check whether -g works, even if CFLAGS is set, in case the package
dnl plays around with CFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CFLAGS="${CFLAGS+set}"
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=
  AC_PROG_CC_G
  if test "$ac_test_CFLAGS" = set; then
    CFLAGS="$ac_save_CFLAGS"
  elif test $ac_cv_prog_cc_g = yes; then
    CFLAGS="-g -O2"
  else
    CFLAGS="-O2"
  fi
else
  GCC=
  test "${CFLAGS+set}" = set || CFLAGS="-g"
fi
])

LIB_AC_PROG_CC

# Likewise for AC_PROG_CXX.
AC_DEFUN(LIB_AC_PROG_CXX,
[AC_BEFORE([$0], [AC_PROG_CXXCPP])dnl
dnl Fool anybody using AC_PROG_CXX.
AC_PROVIDE([AC_PROG_CXX])
AC_CHECK_PROGS(CXX, $CCC c++ g++ gcc CC cxx cc++, gcc)
test -z "$CXX" && AC_MSG_ERROR([no acceptable c++ found in \$PATH])

AC_PROG_CXX_GNU

if test $ac_cv_prog_gxx = yes; then
  GXX=yes
dnl Check whether -g works, even if CXXFLAGS is set, in case the package
dnl plays around with CXXFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS=
  AC_PROG_CXX_G
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  elif test $ac_cv_prog_cxx_g = yes; then
    CXXFLAGS="-g -O2"
  else
    CXXFLAGS="-O2"
  fi
else
  GXX=
  test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
fi
])

LIB_AC_PROG_CXX

# AC_CHECK_TOOL does AC_REQUIRE (AC_CANONICAL_BUILD).  If we dont
# run it explicitly here, it will be run implicitly before
# LIBGCJ_CONFIGURE, which doesn't work because that means that it will
# be run before AC_CANONICAL_HOST.
AC_CANONICAL_BUILD

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

# We need AC_EXEEXT to keep automake happy in cygnus mode.  However,
# at least currently, we never actually build a program, so we never
# need to use $(EXEEXT).  Moreover, the test for EXEEXT normally
# fails, because we are probably configuring with a cross compiler
# which cant create executables.  So we include AC_EXEEXT to keep
# automake happy, but we dont execute it, since we dont care about
# the result.
if false; then
  AC_EXEEXT
fi

# configure.host sets the following important variables
#	glibcpp_cflags    - host specific C compiler flags
#	glibcpp_cxxflags  - host specific C++ compiler flags

glibcpp_cflags=
glibcpp_cxxflags=

. [$]{glibcpp_basedir}/configure.host

case [$]{glibcpp_basedir} in
/* | [A-Za-z]:[/\\]*) libgcj_flagbasedir=[$]{glibcpp_basedir} ;;
*) glibcpp_flagbasedir='[$](top_builddir)/'[$]{glibcpp_basedir} ;;
esac

GLIBCPP_CFLAGS="[$]{glibcpp_cflags}"
GLIBCPP_CXXFLAGS="[$]{glibcpp_cxxflags}"
AC_SUBST(GLIBCPP_CFLAGS)
AC_SUBST(GLIBCPP_CXXFLAGS)
])


dnl
dnl Check to see if g++ can compile this library, and if so, if any version-
dnl specific precautions need to be taken. In particular, test for
dnl newer compiler features, or features that are present in newer
dnl compiler version but not older compiler versions should be placed
dnl here.
dnl
dnl Define FMTFLAGS='-fdiagnostics-show-location=once' if possible
dnl Define WERROR='-Werror' if possible; g++'s that lack the new inlining
dnl    code or the new system_header pragma will die.  Other options dealing
dnl    with warnings, errors, and compiler complaints may be folded into
dnl    the WERROR variable.
dnl
dnl GLIBCPP_CHECK_COMPILER_VERSION
AC_DEFUN(GLIBCPP_CHECK_COMPILER_VERSION, [
  # All these tests are for C++; save the language and the compiler flags.
  # The CXXFLAGS thing is suspicious, but based on similar bits 
  # found in GLIBCPP_CONFIGURE.
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  WERROR='-Werror'

  # Sanity check that g++ is capable of dealing with v-3.
  AC_MSG_CHECKING([for g++ that will successfully compile this code])
  AC_EGREP_CPP([ok], [
  #if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95) 
    ok
  #endif
  ], gpp_satisfactory=yes, AC_MSG_ERROR("please upgrade to gcc-2.95 or above"))
  AC_MSG_RESULT($gpp_satisfactory)

  # Check for pragma system_header.
  AC_MSG_CHECKING([for g++ that supports pragma system_header])
  CXXFLAGS='-Wunknown-pragmas -Werror'
  AC_TRY_COMPILE([#pragma system_header], [int foo;
  ], [ac_newpragma=yes], [ac_newpragma=no])
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  else
    # this is the suspicious part
    CXXFLAGS=''
  fi
  if test "$ac_newpragma" = "no"; then
    WERROR="$WERROR -Wno-unknown-pragmas"
  fi
  AC_MSG_RESULT($ac_newpragma)

  # Check for more sophisticated diagnostic control.
  AC_MSG_CHECKING([for g++ that supports -fdiagnostics-show-location=once])
  CXXFLAGS='-fdiagnostics-show-location=once'
  AC_TRY_COMPILE(, [int foo;
  ], [ac_gabydiags=yes], [ac_gabydiags=no])
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  else
    # this is the suspicious part
    CXXFLAGS=''
  fi
  if test "$ac_gabydiags" = "yes"; then
    FMTFLAGS='-fdiagnostics-show-location=once'
  fi
  AC_MSG_RESULT($ac_gabydiags)

  AC_LANG_RESTORE
  AC_SUBST(WERROR)
  AC_SUBST(FMTFLAGS)
])

dnl
dnl Check to see what builtin math functions are supported
dnl
dnl check for __builtin_acos
dnl check for __builtin_acosf
dnl check for __builtin_acosl
dnl check for __builtin_asin
dnl check for __builtin_asinf
dnl check for __builtin_asinl
dnl check for __builtin_atan
dnl check for __builtin_atanf
dnl check for __builtin_atanl
dnl check for __builtin_atan2
dnl check for __builtin_atan2f
dnl check for __builtin_atan2l
dnl check for __builtin_ceil
dnl check for __builtin_ceilf
dnl check for __builtin_ceill
dnl check for __builtin_cos
dnl check for __builtin_cosf
dnl check for __builtin_cosl
dnl check for __builtin_cosh
dnl check for __builtin_coshf
dnl check for __builtin_coshl
dnl check for __builtin_exp
dnl check for __builtin_expf
dnl check for __builtin_expl
dnl check for __builtin_fabs
dnl check for __builtin_fabsf
dnl check for __builtin_fabsl
dnl check for __builtin_floor
dnl check for __builtin_floorf
dnl check for __builtin_floorl
dnl check for __builtin_frexp
dnl check for __builtin_frexpf
dnl check for __builtin_frexpl
dnl check for __builtin_ldexp
dnl check for __builtin_ldexpf
dnl check for __builtin_ldexpl
dnl check for __builtin_log
dnl check for __builtin_logf
dnl check for __builtin_logl
dnl check for __builtin_log10
dnl check for __builtin_log10f
dnl check for __builtin_log10l
dnl check for __builtin_modf
dnl check for __builtin_modff
dnl check for __builtin_modfl
dnl check for __builtin_pow
dnl check for __builtin_powf
dnl check for __builtin_powl
dnl check for __builtin_sin
dnl check for __builtin_sinf
dnl check for __builtin_sinl
dnl check for __builtin_sinh
dnl check for __builtin_sinhf
dnl check for __builtin_sinhl
dnl check for __builtin_sqrt
dnl check for __builtin_sqrtf
dnl check for __builtin_sqrtl
dnl check for __builtin_tan
dnl check for __builtin_tanf
dnl check for __builtin_tanl
dnl check for __builtin_tanh
dnl check for __builtin_tanhf
dnl check for __builtin_tanhl
dnl GLIBCPP_CHECK_BUILTIN_MATH_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_BUILTIN_MATH_SUPPORT, [
  dnl Test for builtin math functions.
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_MSG_CHECKING([for __builtin_acos])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_acos(0.0);], 
  use_builtin_acos=yes, use_builtin_acos=no)
  AC_MSG_RESULT($use_builtin_acos)
  if test $use_builtin_acos = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ACOS)
  fi
  AC_MSG_CHECKING([for __builtin_acosf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_acosf(0.0);], 
  use_builtin_acosf=yes, use_builtin_acosf=no)
  AC_MSG_RESULT($use_builtin_acosf)
  if test $use_builtin_acosf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ACOSF)
  fi
  AC_MSG_CHECKING([for __builtin_acosl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_acosl(0.0);], 
  use_builtin_acosl=yes, use_builtin_acosl=no)
  AC_MSG_RESULT($use_builtin_acosl)
  if test $use_builtin_acosl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ACOSL)
  fi
  AC_MSG_CHECKING([for __builtin_asin])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_asin(0.0);], 
  use_builtin_asin=yes, use_builtin_asin=no)
  AC_MSG_RESULT($use_builtin_asin)
  if test $use_builtin_asin = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ASIN)
  fi
  AC_MSG_CHECKING([for __builtin_asinf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_asinf(0.0);], 
  use_builtin_asinf=yes, use_builtin_asinf=no)
  AC_MSG_RESULT($use_builtin_asinf)
  if test $use_builtin_asinf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ASINF)
  fi
  AC_MSG_CHECKING([for __builtin_asinl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_asinl(0.0);], 
  use_builtin_asinl=yes, use_builtin_asinl=no)
  AC_MSG_RESULT($use_builtin_asinl)
  if test $use_builtin_asinl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ASINL)
  fi
  AC_MSG_CHECKING([for __builtin_atan])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atan(0.0);], 
  use_builtin_atan=yes, use_builtin_atan=no)
  AC_MSG_RESULT($use_builtin_atan)
  if test $use_builtin_atan = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATAN)
  fi
  AC_MSG_CHECKING([for __builtin_atanf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atanf(0.0);], 
  use_builtin_atanf=yes, use_builtin_atanf=no)
  AC_MSG_RESULT($use_builtin_atanf)
  if test $use_builtin_atanf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATANF)
  fi
  AC_MSG_CHECKING([for __builtin_atanl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atanl(0.0);], 
  use_builtin_atanl=yes, use_builtin_atanl=no)
  AC_MSG_RESULT($use_builtin_atanl)
  if test $use_builtin_atanl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATANL)
  fi
  AC_MSG_CHECKING([for __builtin_atan2])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atan2(0.0);], 
  use_builtin_atan2=yes, use_builtin_atan2=no)
  AC_MSG_RESULT($use_builtin_atan2)
  if test $use_builtin_atan2 = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATAN2)
  fi
  AC_MSG_CHECKING([for __builtin_atan2f])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atan2f(0.0);], 
  use_builtin_atan2f=yes, use_builtin_atan2f=no)
  AC_MSG_RESULT($use_builtin_atan2f)
  if test $use_builtin_atan2f = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATAN2F)
  fi
  AC_MSG_CHECKING([for __builtin_atan2l])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_atan2l(0.0);], 
  use_builtin_atan2l=yes, use_builtin_atan2l=no)
  AC_MSG_RESULT($use_builtin_atan2l)
  if test $use_builtin_atan2l = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_ATAN2L)
  fi
  AC_MSG_CHECKING([for __builtin_ceil])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_fceil(0.0);], 
  use_builtin_ceil=yes, use_builtin_ceil=no)
  AC_MSG_RESULT($use_builtin_ceil)
  if test $use_builtin_ceil = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_CEIL)
  fi
  AC_MSG_CHECKING([for __builtin_ceilf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ceilf(0.0);], 
  use_builtin_ceilf=yes, use_builtin_ceilf=no)
  AC_MSG_RESULT($use_builtin_ceilf)
  if test $use_builtin_ceilf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_CEILF)
  fi
  AC_MSG_CHECKING([for __builtin_ceill])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ceill(0.0);], 
  use_builtin_ceill=yes, use_builtin_ceill=no)
  AC_MSG_RESULT($use_builtin_ceill)
  if test $use_builtin_ceill = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_CEILL)
  fi
  AC_MSG_CHECKING([for __builtin_cos])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_cos(0.0);], 
  use_builtin_cos=yes, use_builtin_cos=no)
  AC_MSG_RESULT($use_builtin_cos)
  if test $use_builtin_cos = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COS)
  fi
  AC_MSG_CHECKING([for __builtin_cosf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_cosf(0.0);], 
  use_builtin_cosf=yes, use_builtin_cosf=no)
  AC_MSG_RESULT($use_builtin_cosf)
  if test $use_builtin_cosf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COSF)
  fi
  AC_MSG_CHECKING([for __builtin_cosl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_cosl(0.0);], 
  use_builtin_cosl=yes, use_builtin_cosl=no)
  AC_MSG_RESULT($use_builtin_cosl)
  if test $use_builtin_cosl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COSL)
  fi
  AC_MSG_CHECKING([for __builtin_cosh])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_cosh(0.0);], 
  use_builtin_cosh=yes, use_builtin_cosh=no)
  AC_MSG_RESULT($use_builtin_cosh)
  if test $use_builtin_cosh = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COSH)
  fi
  AC_MSG_CHECKING([for __builtin_coshf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_coshf(0.0);], 
  use_builtin_coshf=yes, use_builtin_coshf=no)
  AC_MSG_RESULT($use_builtin_coshf)
  if test $use_builtin_coshf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COSHF)
  fi
  AC_MSG_CHECKING([for __builtin_coshl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_coshl(0.0);], 
  use_builtin_coshl=yes, use_builtin_coshl=no)
  AC_MSG_RESULT($use_builtin_coshl)
  if test $use_builtin_coshl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_COSHL)
  fi
  AC_MSG_CHECKING([for __builtin_exp])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_exp(0.0);], 
  use_builtin_exp=yes, use_builtin_exp=no)
  AC_MSG_RESULT($use_builtin_exp)
  if test $use_builtin_exp = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_EXP)
  fi
  AC_MSG_CHECKING([for __builtin_expf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_expf(0.0);], 
  use_builtin_expf=yes, use_builtin_expf=no)
  AC_MSG_RESULT($use_builtin_expf)
  if test $use_builtin_expf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_EXPF)
  fi
  AC_MSG_CHECKING([for __builtin_expl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_expl(0.0);], 
  use_builtin_expl=yes, use_builtin_expl=no)
  AC_MSG_RESULT($use_builtin_expl)
  if test $use_builtin_expl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_EXPL)
  fi
  AC_MSG_CHECKING([for __builtin_fabs])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_fabs(0.0);], 
  use_builtin_fabs=yes, use_builtin_fabs=no)
  AC_MSG_RESULT($use_builtin_fabs)
  if test $use_builtin_fabs = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FABS)
  fi
  AC_MSG_CHECKING([for __builtin_fabsf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_fabsf(0.0);], 
  use_builtin_fabsf=yes, use_builtin_fabsf=no)
  AC_MSG_RESULT($use_builtin_fabsf)
  if test $use_builtin_fabsf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FABSF)
  fi
  AC_MSG_CHECKING([for __builtin_fabsl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_fabsl(0.0);], 
  use_builtin_fabsl=yes, use_builtin_fabsl=no)
  AC_MSG_RESULT($use_builtin_fabsl)
  if test $use_builtin_fabsl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FABSL)
  fi
  AC_MSG_CHECKING([for __builtin_floor])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ffloor(0.0);], 
  use_builtin_floor=yes, use_builtin_floor=no)
  AC_MSG_RESULT($use_builtin_floor)
  if test $use_builtin_floor = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FLOOR)
  fi
  AC_MSG_CHECKING([for __builtin_floorf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_floorf(0.0);], 
  use_builtin_floorf=yes, use_builtin_floorf=no)
  AC_MSG_RESULT($use_builtin_floorf)
  if test $use_builtin_floorf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FLOORF)
  fi
  AC_MSG_CHECKING([for __builtin_floorl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_floorl(0.0);], 
  use_builtin_floorl=yes, use_builtin_floorl=no)
  AC_MSG_RESULT($use_builtin_floorl)
  if test $use_builtin_floorl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FLOORL)
  fi
  AC_MSG_CHECKING([for __builtin_frexp])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_frexp(0.0);], 
  use_builtin_frexp=yes, use_builtin_frexp=no)
  AC_MSG_RESULT($use_builtin_frexp)
  if test $use_builtin_frexp = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FREXP)
  fi
  AC_MSG_CHECKING([for __builtin_frexpf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_frexpf(0.0);], 
  use_builtin_frexpf=yes, use_builtin_frexpf=no)
  AC_MSG_RESULT($use_builtin_frexpf)
  if test $use_builtin_frexpf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FREXPF)
  fi
  AC_MSG_CHECKING([for __builtin_frexpl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_frexpl(0.0);], 
  use_builtin_frexpl=yes, use_builtin_frexpl=no)
  AC_MSG_RESULT($use_builtin_frexpl)
  if test $use_builtin_frexpl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_FREXPL)
  fi
  AC_MSG_CHECKING([for __builtin_ldexp])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ldexp(0.0);], 
  use_builtin_ldexp=yes, use_builtin_ldexp=no)
  AC_MSG_RESULT($use_builtin_ldexp)
  if test $use_builtin_ldexp = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LDEXP)
  fi
  AC_MSG_CHECKING([for __builtin_ldexpf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ldexpf(0.0);], 
  use_builtin_ldexpf=yes, use_builtin_ldexpf=no)
  AC_MSG_RESULT($use_builtin_ldexpf)
  if test $use_builtin_ldexpf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LDEXPF)
  fi
  AC_MSG_CHECKING([for __builtin_ldexpl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_ldexpl(0.0);], 
  use_builtin_ldexpl=yes, use_builtin_ldexpl=no)
  AC_MSG_RESULT($use_builtin_ldexpl)
  if test $use_builtin_ldexpl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LDEXPL)
  fi
  AC_MSG_CHECKING([for __builtin_log])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_log(0.0);], 
  use_builtin_log=yes, use_builtin_log=no)
  AC_MSG_RESULT($use_builtin_log)
  if test $use_builtin_log = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOG)
  fi
  AC_MSG_CHECKING([for __builtin_logf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_logf(0.0);], 
  use_builtin_logf=yes, use_builtin_logf=no)
  AC_MSG_RESULT($use_builtin_logf)
  if test $use_builtin_logf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOGF)
  fi
  AC_MSG_CHECKING([for __builtin_logl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_logl(0.0);], 
  use_builtin_logl=yes, use_builtin_logl=no)
  AC_MSG_RESULT($use_builtin_logl)
  if test $use_builtin_logl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOGL)
  fi
  AC_MSG_CHECKING([for __builtin_log10])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_log10(0.0);], 
  use_builtin_log10=yes, use_builtin_log10=no)
  AC_MSG_RESULT($use_builtin_log10)
  if test $use_builtin_log10 = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOG10)
  fi
  AC_MSG_CHECKING([for __builtin_log10f])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_log10f(0.0);], 
  use_builtin_log10f=yes, use_builtin_log10f=no)
  AC_MSG_RESULT($use_builtin_log10f)
  if test $use_builtin_log10f = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOG10F)
  fi
  AC_MSG_CHECKING([for __builtin_log10l])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_log10l(0.0);], 
  use_builtin_log10l=yes, use_builtin_log10l=no)
  AC_MSG_RESULT($use_builtin_log10l)
  if test $use_builtin_log10l = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_LOG10L)
  fi
  AC_MSG_CHECKING([for __builtin_modf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_modf(0.0);], 
  use_builtin_modf=yes, use_builtin_modf=no)
  AC_MSG_RESULT($use_builtin_modf)
  if test $use_builtin_modf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_MODF)
  fi
  AC_MSG_CHECKING([for __builtin_modff])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_modff(0.0);], 
  use_builtin_modff=yes, use_builtin_modff=no)
  AC_MSG_RESULT($use_builtin_modff)
  if test $use_builtin_modff = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_MODFF)
  fi
  AC_MSG_CHECKING([for __builtin_modfl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_modfl(0.0);], 
  use_builtin_modfl=yes, use_builtin_modfl=no)
  AC_MSG_RESULT($use_builtin_modfl)
  if test $use_builtin_modfl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_MODFL)
  fi
  AC_MSG_CHECKING([for __builtin_pow])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_pow(0.0);], 
  use_builtin_pow=yes, use_builtin_pow=no)
  AC_MSG_RESULT($use_builtin_pow)
  if test $use_builtin_pow = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_POW)
  fi
  AC_MSG_CHECKING([for __builtin_powf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_powf(0.0);], 
  use_builtin_powf=yes, use_builtin_powf=no)
  AC_MSG_RESULT($use_builtin_powf)
  if test $use_builtin_powf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_POWF)
  fi
  AC_MSG_CHECKING([for __builtin_powl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_powl(0.0);], 
  use_builtin_powl=yes, use_builtin_powl=no)
  AC_MSG_RESULT($use_builtin_powl)
  if test $use_builtin_powl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_POWL)
  fi
  AC_MSG_CHECKING([for __builtin_sin])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sin(0.0);], 
  use_builtin_sin=yes, use_builtin_sin=no)
  AC_MSG_RESULT($use_builtin_sin)
  if test $use_builtin_sin = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SIN)
  fi
  AC_MSG_CHECKING([for __builtin_sinf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sinf(0.0);], 
  use_builtin_sinf=yes, use_builtin_sinf=no)
  AC_MSG_RESULT($use_builtin_sinf)
  if test $use_builtin_sinf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SINF)
  fi
  AC_MSG_CHECKING([for __builtin_sinl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sinl(0.0);], 
  use_builtin_sinl=yes, use_builtin_sinl=no)
  AC_MSG_RESULT($use_builtin_sinl)
  if test $use_builtin_sinl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SINL)
  fi
  AC_MSG_CHECKING([for __builtin_sinh])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sinh(0.0);], 
  use_builtin_sinh=yes, use_builtin_sinh=no)
  AC_MSG_RESULT($use_builtin_sinh)
  if test $use_builtin_sinh = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SINH)
  fi
  AC_MSG_CHECKING([for __builtin_sinhf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sinhf(0.0);], 
  use_builtin_sinhf=yes, use_builtin_sinhf=no)
  AC_MSG_RESULT($use_builtin_sinhf)
  if test $use_builtin_sinhf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SINHF)
  fi
  AC_MSG_CHECKING([for __builtin_sinhl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sinhl(0.0);], 
  use_builtin_sinhl=yes, use_builtin_sinhl=no)
  AC_MSG_RESULT($use_builtin_sinhl)
  if test $use_builtin_sinhl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SINHL)
  fi
  AC_MSG_CHECKING([for __builtin_sqrt])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_fsqrt(0.0);], 
  use_builtin_sqrt=yes, use_builtin_sqrt=no)
  AC_MSG_RESULT($use_builtin_sqrt)
  if test $use_builtin_sqrt = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SQRT)
  fi
  AC_MSG_CHECKING([for __builtin_sqrtf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sqrtf(0.0);], 
  use_builtin_sqrtf=yes, use_builtin_sqrtf=no)
  AC_MSG_RESULT($use_builtin_sqrtf)
  if test $use_builtin_sqrtf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SQRTF)
  fi
  AC_MSG_CHECKING([for __builtin_sqrtl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_sqrtl(0.0);], 
  use_builtin_sqrtl=yes, use_builtin_sqrtl=no)
  AC_MSG_RESULT($use_builtin_sqrtl)
  if test $use_builtin_sqrtl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_SQRTL)
  fi
  AC_MSG_CHECKING([for __builtin_tan])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tan(0.0);], 
  use_builtin_tan=yes, use_builtin_tan=no)
  AC_MSG_RESULT($use_builtin_tan)
  if test $use_builtin_tan = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TAN)
  fi
  AC_MSG_CHECKING([for __builtin_tanf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tanf(0.0);], 
  use_builtin_tanf=yes, use_builtin_tanf=no)
  AC_MSG_RESULT($use_builtin_tanf)
  if test $use_builtin_tanf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TANF)
  fi
  AC_MSG_CHECKING([for __builtin_tanl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tanl(0.0);], 
  use_builtin_tanl=yes, use_builtin_tanl=no)
  AC_MSG_RESULT($use_builtin_tanl)
  if test $use_builtin_tanl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TANL)
  fi
  AC_MSG_CHECKING([for __builtin_tanh])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tanh(0.0);], 
  use_builtin_tanh=yes, use_builtin_tanh=no)
  AC_MSG_RESULT($use_builtin_tanh)
  if test $use_builtin_tanh = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TANH)
  fi
  AC_MSG_CHECKING([for __builtin_tanhf])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tanhf(0.0);], 
  use_builtin_tanhf=yes, use_builtin_tanhf=no)
  AC_MSG_RESULT($use_builtin_tanhf)
  if test $use_builtin_tanhf = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TANHF)
  fi
  AC_MSG_CHECKING([for __builtin_tanhl])
  AC_TRY_COMPILE([#include <math.h>], 
  [ __builtin_tanhl(0.0);], 
  use_builtin_tanhl=yes, use_builtin_tanhl=no)
  AC_MSG_RESULT($use_builtin_tanhl)
  if test $use_builtin_tanhl = "yes"; then
    AC_DEFINE(HAVE_BUILTIN_TANHL)
  fi
  AC_LANG_RESTORE
])



dnl Check to see what architecture we are compiling for. If it's
dnl supported, use special hand-crafted routines to provide thread
dnl primitives. Also, if architecture-specific flags are required for 
dnl compilation, add them here.
dnl 
dnl Depending on what is found, select configure/cpu/*/bits/atomicity.h 
dnl If not found, select configure/cpu/generic/bits/atomicity.h
dnl
dnl GLIBCPP_CHECK_CPU
AC_DEFUN(GLIBCPP_CHECK_CPU, [
    AC_MSG_CHECKING([for cpu primitives directory])
    CPUFLAGS=			
    case "$target_cpu" in
      alpha*)
	cpu_include_dir="config/cpu/alpha"
        ;;
      arm*)
	cpu_include_dir="config/cpu/arm"
        ;;
      i386)
	cpu_include_dir="config/cpu/i386"
	;;
      i486 | i586 | i686 | i786)
	cpu_include_dir="config/cpu/i486"
        ;;
      powerpc | rs6000)
	cpu_include_dir="config/cpu/powerpc"
    	CPUFLAGS='-mcpu=powerpc'
        ;;
      sparc64 | ultrasparc)
	cpu_include_dir="config/cpu/sparc/sparc64"
        ;;
      sparc*)
	cpu_include_dir="config/cpu/sparc/sparc32"
        ;;
      *)
	cpu_include_dir="config/cpu/generic"
        ;;
    esac
    AC_MSG_RESULT($cpu_include_dir)
    AC_SUBST(cpu_include_dir)
    AC_SUBST(CPUFLAGS)
])

 
dnl
dnl Check to see what the underlying c library's interface to ctype looks
dnl like. Bits of locale rely on things like isspace, toupper, etc. This
dnl stuff makes sure the right bits from the clibrary get called.
dnl 
dnl Depending on what is found, select various configure/*/bits/ctype_base.h 
dnl Depending on what is found, select various configure/*/ctype.cc
dnl
dnl GLIBCPP_CHECK_CTYPE
AC_DEFUN(GLIBCPP_CHECK_CTYPE, [
  AC_CHECK_HEADER(ctype.h, [
    
    dnl If doesn't match any specified, go with defaults.
    ctype_default=yes

    dnl Test for <ctype> functionality -- gnu-linux
    AC_MSG_CHECKING([<ctype> for gnu-linux ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _ISspace + _ISprint + _IScntrl + _ISupper + _ISlower + _ISalpha \
	+ _ISdigit + _ISpunct + _ISxdigit + _ISalnum + _ISgraph \
	+ __ctype_tolower[a] + __ctype_toupper[a] + __ctype_b[a];}], \
    ctype_linux=yes, ctype_linux=no)
    AC_MSG_RESULT($ctype_linux)
    if test $ctype_linux = "yes"; then
      ctype_include_dir="config/gnu-linux"
      ctype_default=no
    fi

    dnl Test for <ctype> functionality -- FreeBSD 4.0
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for freebsd 4.0 ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _CTYPE_S + _CTYPE_R + _CTYPE_C + _CTYPE_U + _CTYPE_L + _CTYPE_A \
	+ _CTYPE_D + _CTYPE_P + _CTYPE_X + _CTYPE_G ;}], \
    ctype_bsd=yes, ctype_bsd=no)
    AC_MSG_RESULT($ctype_bsd)
    if test $ctype_bsd = "yes"; then
      ctype_include_dir="config/bsd"
      ctype_default=no
    fi
    fi

    dnl Test for <ctype> functionality -- FreeBSD 3.4
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for freebsd 3.4 ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _S + _R + _C + _U + _L + _A \
      + _D + _P + _X + _G + __istype (a, 0);}], \
    ctype_freebsd34=yes, ctype_freebsd34=no)
    AC_MSG_RESULT($ctype_freebsd34)
    if test $ctype_freebsd34 = "yes"; then
      ctype_include_dir="config/bsd"
      ctype_default=no
    fi
    fi

    dnl Test for <ctype> functionality -- solaris 2.6 and 2.7
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for solaris 2.[6,7,8] ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _ISSPACE + _ISPRINT + _ISCNTRL + _ISUPPER + _ISLOWER + _ISALPHA \
	+ _ISDIGIT + _ISPUNCT + _ISXDIGIT + _ISALNUM + _ISGRAPH \
	+ __trans_lower[a] + __trans_upper[a] + __ctype_mask[a];}], \
    ctype_solaris=yes, ctype_solaris=no)
    AC_MSG_RESULT($ctype_solaris)

    if test $ctype_solaris = "yes"; then
      AC_MSG_CHECKING([  for version])
      AC_LANG_CPLUSPLUS 
      AC_TRY_COMPILE([#include <ctype.h>],
      [typedef long* __to_type; __to_type const& _M_toupper = __trans_upper;],\
      ctype_solaris26=yes, ctype_solaris26=no)
      AC_LANG_C
      if test $ctype_solaris26 = "yes"; then
        ctype_include_dir="config/solaris/solaris2.6"
        AC_MSG_RESULT("solaris2.6")
        ctype_default=no
      else
        ctype_include_dir="config/solaris/solaris2.7"
        AC_MSG_RESULT("solaris2.[7,8]")
        ctype_default=no
      fi
    fi
    fi  

    dnl Test for <ctype> functionality -- solaris 2.5.1
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for solaris 2.5.1 ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _U + _L + _N + _S + _P + _C + _X + _B \
	+ __ctype[a];}], \
    ctype_solaris25=yes, ctype_solaris25=no)
    AC_MSG_RESULT($ctype_solaris25)
    if test $ctype_solaris25 = "yes"; then
      ctype_include_dir="config/solaris/solaris2.5"
      ctype_default=no
    fi
    fi

    dnl Test for <ctype> functionality -- aix
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for aix ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _ISSPACE + _ISPRINT + _ISCNTRL + _ISUPPER + _ISLOWER + _ISALPHA \
	+ _ISDIGIT + _ISPUNCT + _ISXDIGIT + _ISALNUM + _ISGRAPH \
	+ _VALC('a') + _IS('c', 0);}], \
    ctype_aix=yes, ctype_aix=no)
    AC_MSG_RESULT($ctype_aix)
    if test $ctype_aix = "yes"; then
      ctype_include_dir="config/aix"
      ctype_default=no
    fi
    fi

    dnl Test for <ctype> functionality -- newlib
    if test $ctype_default = "yes"; then
    AC_MSG_CHECKING([<ctype> for newlib ])
    AC_TRY_COMPILE([#include <ctype.h>],
    [int
    foo (int a)
    { return _U + _L + _N + _S + _P + _C + _X + _B \
	+ _ctype_[a];}], \
    ctype_newlib=yes, ctype_newlib=no)
    AC_MSG_RESULT($ctype_newlib)
    if test $ctype_newlib = "yes"; then
      ctype_include_dir="config/newlib"
      ctype_default=no
    fi
    fi

    if test $ctype_default = "yes"; then
      ctype_include_dir="config/generic"
      AC_MSG_WARN("Using default ctype headers.")
    fi
    AC_SUBST(ctype_include_dir)
  ])
])


dnl
dnl Check to see what the underlying c library or math library is like.
dnl
dnl Define HAVE_CARGF etc if "cargf" is found.
dnl
dnl GLIBCPP_CHECK_MATH_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_MATH_SUPPORT, [
  AC_CHECK_LIB(m, sin, libm="-lm")
  save_LIBS="$LIBS"
  LIBS="$LIBS $libm"

  dnl Check for complex versions of math functions of platform.
  AC_CHECK_HEADERS([complex.h])
  AC_REPLACE_MATHFUNCS(ccos ccosf ccosh ccoshf cexp cexpf c_log c_logf \
  clog10 clog10f cpow cpowf csin csinf csinh csinhf csqrt csqrtf \
  ctan ctanf ctanh ctanhf \
  carg cargf nan hypot hypotf atan2f expf copysignf)

  dnl We compile the long double complex functions only if the function 
  dnl provides the non-complex long double functions.
  USE_LONG_DOUBLE=no
  AC_CHECK_FUNC(copysignl,
  USE_LONG_DOUBLE=yes
  AC_REPLACE_MATHFUNCS(ccoshl ccosl cexpl cpowl csinhl csinl \
  csqrtl ctanhl ctanl cargl hypotl signbitl c_logl clog10l))
  AC_SUBST(USE_LONG_DOUBLE)

  dnl Check to see if basic C math functions have float, long double versions.
  AC_REPLACE_MATHFUNCS(cosf fabsf sinf sqrtf)
  AC_CHECK_FUNCS(isnan isnanf isnanl isinf isinff isinfl copysign copysignl \
  acosf acosl asinf asinl atanf atanl atan2f atan2l ceilf ceill cosl \
  coshf coshl expf expl fabsl floorf floorl frexpf frexpl ldexpf \
  ldexpl logf logl log10f log10l modf modff modfl powf powl sinl sinhf \
  sinhl sqrtl tanf tanl tanhf tanhl strtof strtold sincos sincosf \
  sincosl finite finitef finitel fqfinite fpclass qfpclass)

#Some runtimes have these functions with a preceding underscore. Please
# keep this sync'd with the one above. And if you add any new symbol,
# please add the corresponding block in the @BOTTOM@ section of
# acconfig.h.

  AC_CHECK_FUNCS(_isnan _isnanf _isnanl _isinf _isinff _isinfl _copysign \
  _copysignl _acosf _acosl _asinf _asinl _atanf _atanl _atan2f _atan2l \
  _ceilf _ceill _cosf _cosl _coshf _coshl _expf _expl _fabsf _fabsl \
  _floorf _floorl _frexpf _frexpl _ldexpf _ldexpl _logf _logl _log10f \
  _log10l _modf _modff _modfl _powf _powl _sinf _sinl _sinhf _sinhl _sqrtf \
  _sqrtl _tanf _tanl _tanhf _tanhl _strtof _strtold _sincos _sincosf _sincosl \
  _finite _finitef _finitel _fqfinite _fpclass _qfpclass)

LIBS="$save_LIBS"
])


dnl
dnl Check to see if this target can enable the wchar_t parts of libstdc++.
dnl
dnl Define _GLIBCPP_USE_WCHAR_T if all the bits are found 
dnl Define _GLIBCPP_NEED_MBSTATE_T if mbstate_t is not in wchar.h
dnl
dnl GLIBCPP_CHECK_WCHAR_T_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_WCHAR_T_SUPPORT, [

  dnl Sanity check for existence of ISO C9X headers for extended encoding.
  AC_CHECK_HEADER(wchar.h, ac_has_wchar_h=yes, ac_has_wchar_h=no)
  AC_CHECK_HEADER(wctype.h, ac_has_wctype_h=yes, ac_has_wctype_h=no)
	
  dnl Only continue checking if the ISO C9X headers exist.
  if test x"$ac_has_wchar_h" = xyes && test x"$ac_has_wctype_h" = xyes; then

    dnl Test wchar.h for mbstate_t, which is needed for char_traits and others.
    AC_MSG_CHECKING([for mbstate_t])
    AC_TRY_COMPILE([#include <wchar.h>],
    [mbstate_t teststate;], 
    use_native_mbstatet=yes, use_native_mbstatet=no)
    AC_MSG_RESULT($use_native_mbstatet)
    if test x"$use_native_mbstatet" = xno; then
      AC_DEFINE(_GLIBCPP_NEED_MBSTATE_T)
    fi
  
    dnl Test wchar.h for WCHAR_MIN, WCHAR_MAX, which is needed before
    dnl numeric_limits can instantiate type_traits<wchar_t>
    AC_MSG_CHECKING([for WCHAR_MIN and WCHAR_MAX])
    AC_TRY_COMPILE([#include <wchar.h>],
    [int i = WCHAR_MIN; int j = WCHAR_MAX;], 
    has_wchar_minmax=yes, has_wchar_minmax=no)
    AC_MSG_RESULT($has_wchar_minmax)
  
    dnl Test wchar.h for WEOF, which is what we use to determine whether
    dnl to specialize for char_traits<wchar_t> or not.
    AC_MSG_CHECKING([for WEOF])
    AC_TRY_COMPILE([
      #include <wchar.h>
      #include <stddef.h>],
    [wint_t i = WEOF;],
    has_weof=yes, has_weof=no)
    AC_MSG_RESULT($has_weof)

    dnl Tests for wide character functions used in char_traits<wchar_t>.
    AC_CHECK_FUNCS(wcslen wmemchr wmemcmp wmemcpy wmemmove wmemset, ac_wfuncs=yes, ac_wfuncs=no)

    AC_MSG_CHECKING([for ISO C9X wchar_t support])
    if test x"$has_weof" = xyes && test x"$has_wchar_minmax" = xyes && test x"$ac_wfuncs" = xyes; then
      ac_isoC9X_wchar_t=yes
    else
      ac_isoC9X_wchar_t=no
    fi
    AC_MSG_RESULT($ac_isoC9X_wchar_t)

    dnl Use iconv for wchar_t to char conversions. As such, check for 
    dnl X/Open Portability Guide, version 2 features (XPG2).
    AC_CHECK_HEADER(iconv.h, ac_has_iconv_h=yes, ac_has_iconv_h=no)
    AC_CHECK_FUNCS(iconv_open iconv_close iconv, ac_XPG2funcs=yes, ac_XPG2funcs=no)

    AC_MSG_CHECKING([for XPG2 wchar_t support])
    if test x"$ac_has_iconv_h" = xyes && test x"$ac_XPG2funcs" = xyes; then
      ac_XPG2_wchar_t=yes
    else
      ac_XPG2_wchar_t=no
    fi
    AC_MSG_RESULT($ac_XPG2_wchar_t)

    dnl At the moment, only enable wchar_t specializations if all the
    dnl above support is present.
    AC_MSG_CHECKING([for enabled wchar_t specializations])
    if test x"$ac_isoC9X_wchar_t" = xyes && test x"$ac_XPG2_wchar_t" = xyes; then
      libinst_wstring_la="libinst-wstring.la"
      AC_DEFINE(_GLIBCPP_USE_WCHAR_T)
      AC_MSG_RESULT("yes")
    else
      libinst_wstring_la=""
      AC_MSG_RESULT("no")
    fi
    AC_SUBST(libinst_wstring_la)

  else
    AC_MSG_WARN([<wchar.h> not found])
    AC_DEFINE(_GLIBCPP_NEED_MBSTATE_T)
  fi
])


dnl
dnl Check to see if this version of GNU C++ is afflicted by bugs in
dnl __complex__ float support.
dnl
dnl Define _GLIBCPP_BUGGY_FLOAT_COMPLEX if buggy.
dnl
dnl GLIBCPP_CHECK_COMPLEX_FLOAT_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_COMPLEX_FLOAT_SUPPORT, [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([for GNU C++ __complex__ float support])
  AC_CACHE_VAL(glibcpp_cv_float_complex, [
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    rm -f conftest.h
    cat > conftest.h <<EOB
      //
      // Check for buggy __complex__ that causes ICE in most versions of egcs
      // and gcc-2.95.x on certain platforms (eg., x86-win32).
      //
      // See http://egcs.cygnus.com/ml/gcc-bugs/1999-07/msg00845.html for
      // more info on the bug itself.
      //
      struct
      float_complex
      {
       __complex__ float m_value;
       float_complex (float = 0.0f, float = 0.0f);
       float_complex (__complex__ float val) : m_value (val) {}
       float_complex foo (const float_complex &val)
         { return float_complex (~val.m_value); }
      };
EOB
    AC_TRY_COMPILE([#include "conftest.h"], ,
      glibcpp_cv_float_complex=ok,
      glibcpp_cv_float_complex=buggy
    )
    AC_LANG_RESTORE
  ])
  AC_MSG_RESULT($glibcpp_cv_float_complex)
  if test $glibcpp_cv_float_complex = buggy; then
    AC_DEFINE(_GLIBCPP_BUGGY_FLOAT_COMPLEX)
  fi
])


dnl
dnl 
dnl Check to see if this version of GNU C++ is afflicted by bugs in 
dnl __complex__ support.Check for buggy __complex__ that will cause ICE in
dnl gcc-2.95.x when using the library, unless we define the default copy
dnl ctor in the specializations of complex<>. 
dnl 
dnl Define _GLIBCPP_BUGGY_COMPLEX if buggy.
dnl
dnl GLIBCPP_CHECK_COMPLEX_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_COMPLEX_SUPPORT, [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([for GNU C++ __complex__ support])
  AC_CACHE_VAL(glibcpp_cv_complex, [
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AC_TRY_COMPILE([struct dcomplex { __complex__ double x; }; \
		    dcomplex f(const dcomplex& x) { return dcomplex(x); }], \
		    [ dcomplex x; f(x); ],
      glibcpp_cv_complex=ok,
      glibcpp_cv_complex=buggy
    )
    AC_LANG_RESTORE
  ])
  AC_MSG_RESULT($glibcpp_cv_complex)
  if test $glibcpp_cv_complex = buggy; then
    AC_DEFINE(_GLIBCPP_BUGGY_COMPLEX)
  fi
])


dnl
dnl Check for special debugging mode; not for production use.
dnl
dnl GLIBCPP_ENABLE_DEBUG
dnl --enable-debug sets '-ggdb -O0'.
dnl --disable-debug sets '-g' and whatever optimization options the
dnl     compiler can handle.
dnl  +  Perhaps --enable-maintainer-mode should automatically turn this on?
dnl  +  Perhaps -D/-U of NDEBUG, DEBUG, DEBUG_ASSERT, ...?
dnl  +  Usage:  GLIBCPP_ENABLE_DEBUG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.  If ommitted, it
dnl       defaults to `no'.
AC_DEFUN(GLIBCPP_ENABLE_DEBUG, [dnl
define([GLIBCPP_ENABLE_DEBUG_DEFAULT], ifelse($1, yes, yes, no))dnl
AC_ARG_ENABLE(debug,
changequote(<<, >>)dnl
<<  --enable-debug          extra debugging, turn off optimization [default=>>GLIBCPP_ENABLE_DEBUG_DEFAULT],
changequote([, ])dnl
[case "$enableval" in
 yes) enable_debug=yes ;;
 no)  enable_debug=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable extra debugging]) ;;
 esac],
enable_debug=GLIBCPP_ENABLE_DEBUG_DEFAULT)dnl
dnl Option parsed, now set things appropriately
case "$enable_debug" in
    yes) 
	DEBUGFLAGS='-O0 -ggdb'			
	;;
    no)   
	DEBUGFLAGS='-g'
        ;;
esac
AC_SUBST(DEBUGFLAGS)
])


dnl
dnl Check for "unusual" flags to pass to the compiler while building.
dnl
dnl GLIBCPP_ENABLE_CXX_FLAGS
dnl --enable-cxx-flags='-foo -bar -baz' is a general method for passing
dnl     experimental flags such as -fhonor-std, -fsquangle, -Dfloat=char, etc.
dnl     Somehow this same set of flags must be passed when [re]building
dnl     libgcc.
dnl --disable-cxx-flags passes nothing.
dnl  +  See http://sourceware.cygnus.com/ml/libstdc++/2000-q2/msg00131.html
dnl         http://sourceware.cygnus.com/ml/libstdc++/2000-q2/msg00284.html
dnl         http://sourceware.cygnus.com/ml/libstdc++/2000-q1/msg00035.html
dnl  +  Usage:  GLIBCPP_ENABLE_CXX_FLAGS(default flags)
dnl       If "default flags" is an empty string (or "none"), the effect is
dnl       the same as --disable or --enable=no.
AC_DEFUN(GLIBCPP_ENABLE_CXX_FLAGS, [dnl
define([GLIBCPP_ENABLE_CXX_FLAGS_DEFAULT], ifelse($1,,, $1))dnl
AC_ARG_ENABLE(cxx-flags,
changequote(<<, >>)dnl
<<  --enable-cxx-flags=FLAGS      pass compiler FLAGS when building library;
                                [default=>>GLIBCPP_ENABLE_CXX_FLAGS_DEFAULT],
changequote([, ])dnl
[case "x$enableval" in
 xyes)   AC_MSG_ERROR([--enable-cxx-flags needs compiler flags as arguments]) ;;
 xno|x)  enable_cxx_flags='' ;;
 *)      enable_cxx_flags="$enableval" ;;
 esac],
enable_cxx_flags='GLIBCPP_ENABLE_CXX_FLAGS_DEFAULT')dnl
dnl Thinko on my part during design.  This kludge is the workaround.
if test "$enable_cxx_flags" = "none"; then enable_cxx_flags=''; fi
dnl Run through flags (either default or command-line) and set anything
dnl extra (e.g., #defines) that must accompany particular g++ options.
if test -n "$enable_cxx_flags"; then
    for f in $enable_cxx_flags; do
        case "$f" in
            -fhonor-std)  ;;
            -*)  ;;
            *)   # and we're trying to pass /what/ exactly?
                 AC_MSG_ERROR([compiler flags start with a -]) ;;
        esac
    done
fi
EXTRA_CXX_FLAGS="$enable_cxx_flags"
AC_SUBST(EXTRA_CXX_FLAGS)
])


dnl
dnl Check for instructions to automatically rebuild libgcc.a.  Requires,
dnl of course, the location of the gcc objdir.  Note that if --disable-
dnl namespaces is in effect, rebuilding libgcc.a is an expensive no-op.
dnl
dnl GLIBCPP_ENABLE_RELIBGCC
dnl --enable-libgcc-rebuild=/absolute/path/to/gcc/objdir sets GCC_OBJDIR
dnl     (presumably in the top-level Makefile) to /absol.../objdir
dnl --disable-libgcc-rebuild will not touch libgcc.a at all (maybe print
dnl     a warning if this is given along with --enable-namespaces), by
dnl     setting GCC_OBJDIR to `no'.
dnl  +  Doing this by default is going to be interesting.  What default
dnl     "on" value can there be?
dnl  +  Usage:  GLIBCPP_ENABLE_RELIBGCC[(DEFAULT)]
dnl       The default path should be ../.. if bundled with GCC source.
dnl       If ommitted, it defaults to `no'.
dnl
AC_DEFUN(GLIBCPP_ENABLE_RELIBGCC, [dnl
define([GLIBCPP_ENABLE_RELIBGCC_DEFAULT], ifelse($1,, no, $1))dnl
AC_ARG_ENABLE(libgcc-rebuild,
changequote(<<, >>)dnl
<<  --enable-libgcc-rebuild=DIR     also rebuild libgcc.a; DIR is
                                  the GCC objdir; see install.html>>,
changequote([, ])dnl
[case "$enableval" in
 yes) AC_MSG_ERROR([--enable-libgcc-rebuild needs a pathname]) ;;
 no)  enable_libgcc_rebuild=no ;;
 *)   if test -d "$enableval" && test -d "${enableval}/gcc" && \
         test -d "${enableval}/libiberty"
      then
         enable_libgcc_rebuild="$enableval"
      else
         AC_MSG_ERROR(["$enableval" does not appear to be the GCC objdir])
      fi
      ;;
 esac],
enable_libgcc_rebuild=GLIBCPP_ENABLE_RELIBGCC_DEFAULT)dnl
GCC_OBJDIR="$enable_libgcc_rebuild"
AC_SUBST(GCC_OBJDIR)
])


dnl
dnl Check for which I/O library to use:  libio, or something specific.
dnl
dnl GLIBCPP_ENABLE_CSTDIO
dnl --enable-cstdio=libio sets config/c_io_libio.h and friends
dnl 
dnl default is libio
dnl
AC_DEFUN(GLIBCPP_ENABLE_CSTDIO, [
  AC_MSG_CHECKING([for cstdio to use])
  AC_ARG_ENABLE(cstdio,
  [  --enable-cstdio         enable GNU libio for target io package. (default)
  --enable-cstdio=LIB     use LIB target-speific io package.], 
  if test x$enable_cstdio = xno; then
    enable_cstdio=libio
  fi,
     enable_cstdio=libio)

  enable_cstdio_flag=$enable_cstdio

  dnl Check if a valid thread package
  case x${enable_cstdio_flag} in
	xlibio | x | xno | xnone | xyes)
		# default
		CSTDIO_H=c_io_libio.h
		CSTDIO_CC=c_io_libio.cc
 		AC_MSG_RESULT(libio)

		# see if we are on a system with libio native (ie, linux)
  		AC_CHECK_HEADER(libio.h,  has_libio=yes, has_libio=no)
  		if test $has_libio = "yes"; then
   		  BUILD_LIBIO_INCLUDE=
		  need_libio=no
  		else
   		  BUILD_LIBIO_INCLUDE='-I../libio'
		  need_libio=yes
  		fi
  		AC_SUBST(BUILD_LIBIO_INCLUDE)

		# see if the _G_config.h header needs to be built. 
		# NB: This replaces the _G_CONFIG_H machinery in libio-v2
		AC_CHECK_HEADER(_G_config.h,  has_gconf_h=yes, has_gconf_h=no)
  		AM_CONDITIONAL(GLIBCPP_NEED_LIBIO_CONFIG_H, test "$has_gconf_h" = no)
		;;
        xwince)
                CSTDIO_H=c_io_wince.h
                CSTDIO_CC=c_io_wince.cc
                AC_MSG_RESULT(wince)

                need_libio=no
                BUILD_LIBIO_INCLUDE=
                AC_SUBST(BUILD_LIBIO_INCLUDE)
                ;;
	*)
		echo "$enable_cstdio is an unknown io package" 1>&2
		exit 1
		;;
  esac
  AC_SUBST(CSTDIO_H)
  AC_SUBST(CSTDIO_CC)
  AM_CONDITIONAL(GLIBCPP_NEED_LIBIO, test "$need_libio" = yes)
])


dnl
dnl Check for which threading library to use.
dnl
dnl GLIBCPP_ENABLE_THREADS
dnl --enable-threads=posix sets config/threads-posix.h et. al.
dnl 
dnl default is no threads
dnl
AC_DEFUN(GLIBCPP_ENABLE_THREADS, [
  dnl Note this comes from the gcc/config.in and libjava/config.in
  dnl Efforts should be made to keep this in sync.
  AC_MSG_CHECKING([for threads package to use])
  AC_ARG_ENABLE(threads,
  [  --enable-threads        enable thread usage for target GCC.
  --enable-threads=LIB    use LIB thread package for target GCC.],
  if test x$enable_threads = xno; then
    enable_threads=''
  fi,
    enable_threads='')

  enable_threads_flag=$enable_threads

  dnl Check if a valid thread package
  case x${enable_threads_flag} in
	x | xno | xnone)
		# No threads
		target_thread_file='single'
		;;
	xyes)
		# default
		target_thread_file=''
		;;
	xdecosf1 | xirix | xmach | xos2 | xposix | xpthreads | xsingle | \
	xsolaris | xwin32 | xdce | xvxworks)
		target_thread_file=$enable_threads_flag
		;;
	*)
		echo "$enable_threads is an unknown thread package" 1>&2
		exit 1
		;;
  esac

  dnl Check for thread package actually supported in libstdc++ 
  case "$target_thread_file" in
    no | none | single)
      THREADS=none
      ;;
    posix | pthreads)
      THREADS=posix
      case "$host" in
        *-*-linux*)
	;;
      esac
      ;;
    decosf1 | irix | mach | os2 | solaris | win32 | dce | vxworks)
      AC_MSG_ERROR(thread package $THREADS not yet supported)
      ;;
    *)
      AC_MSG_ERROR($THREADS is an unknown thread package)
      ;;
  esac
  AC_MSG_RESULT($THREADS)

  THREADLIBS=
  THREADINCS=
  THREADDEPS=
  THREADOBJS=
  THREADH=
  THREADSPEC=
  case "$THREADS" in
    posix)
      AC_CHECK_HEADER(pthread.h, [have_pthread_h=yes], [have_pthread_h=])
      THREADLIBS=-lpthread
      THREADSPEC=-lpthread
      dnl Not presently used
      dnl THREADOBJS=threads-posix.lo
      THREADH=threads-posix.h
      ;;
    none)
      dnl Not presently used
      dnl THREADOBJS=threads-no.lo
      THREADH=threads-no.h
      ;;
  esac
  AC_SUBST(THREADLIBS)
  AC_SUBST(THREADINCS)
  AC_SUBST(THREADDEPS)
  AC_SUBST(THREADOBJS)
  AC_SUBST(THREADSPEC)
])


dnl
dnl Check for template specializations for the 'long long' type extension.
dnl
dnl GLIBCPP_ENABLE_LONG_LONG
dnl --enable-long-long defines _GLIBCPP_USE_LONG_LONG
dnl --disable-long-long leaves _GLIBCPP_USE_LONG_LONG undefined
dnl  +  Usage:  GLIBCPP_ENABLE_LONG_LONG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.  If ommitted, it
dnl       defaults to `no'.
dnl  +  If 'long long' stuff is not available, ignores DEFAULT and sets `no'.
dnl
dnl GLIBCPP_ENABLE_LONG_LONG
AC_DEFUN(GLIBCPP_ENABLE_LONG_LONG, [dnl
  define([GLIBCPP_ENABLE_LONG_LONG_DEFAULT], ifelse($1, yes, yes, no))dnl

  AC_ARG_ENABLE(long-long,
  changequote(<<, >>)dnl
  <<--enable-long-long      turns on 'long long' [default=>>GLIBCPP_ENABLE_LONG_LONG_DEFAULT],
  changequote([, ])dnl
  [case "$enableval" in
   yes) enable_long_long=yes ;;
   no)  enable_long_long=no ;;
   *)   AC_MSG_ERROR([Unknown argument to enable/disable long long]) ;;
   esac],
  enable_long_long=GLIBCPP_ENABLE_LONG_LONG_DEFAULT)dnl

  # Check for the existance of functions used if long long is enabled.
  AC_CHECK_FUNC(strtoll,,ac_strtoll=no)
  AC_CHECK_FUNC(strtoull,,ac_strtoull=no)

  AC_MSG_CHECKING([for enabled long long])
  if test x"$ac_strtoll" = xno || test x"$ac_strtoull" = xno; then 
    enable_long_long=no; 
  fi; 
  AC_MSG_RESULT($enable_long_long)

  dnl Option parsed, now set things appropriately
  case "$enable_long_long" in
    yes)  AC_DEFINE(_GLIBCPP_USE_LONG_LONG)
          ;;
  esac
])


dnl
dnl Check for whether or not to do shadowed C headers.
dnl
dnl GLIBCPP_ENABLE_SHADOW
dnl --enable-cshadow-headers [does stuff].
dnl --disable-cshadow-headers [does not do stuff].
dnl  +  This will eventually need to be on by default.
dnl  +  Usage:  GLIBCPP_ENABLE_SHADOW[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.  If ommitted, it
dnl       defaults to `no'.
AC_DEFUN(GLIBCPP_ENABLE_SHADOW, [dnl
define([GLIBCPP_ENABLE_SHADOW_DEFAULT], ifelse($1, yes, yes, no))dnl
AC_MSG_CHECKING([for enabled cshadow headers])
AC_ARG_ENABLE(cshadow-headers,
changequote(<<, >>)dnl
<<  --enable-cshadow-headers construct "shadowed" C header files for
                           g++ [default=>>GLIBCPP_ENABLE_SHADOW_DEFAULT],
changequote([, ])dnl
[case "$enableval" in
 yes) enable_cshadow_headers=yes 
	;;
 no)  enable_cshadow_headers=no 
	;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable shadowed C headers]) 
	;;
 esac],
enable_cshadow_headers=GLIBCPP_ENABLE_SHADOW_DEFAULT)dnl
AC_MSG_RESULT($enable_cshadow_headers)
dnl Option parsed, now set things appropriately
case "$enable_cshadow_headers" in
    yes) 
	CSHADOWFLAGS="-D_GNU_SOURCE"
	CSHADOW_INCLUDES=" -I$srcdir/shadow -I$blddir/cshadow"
	;;
    no)   
	CSHADOWFLAGS=""
	CSHADOW_INCLUDES=""
        ;;
esac

AC_SUBST(CSHADOWFLAGS)
AC_SUBST(CSHADOW_INCLUDES)
AM_CONDITIONAL(GLIBCPP_USE_CSHADOW, test "$enable_cshadow_headers" = yes)
])


# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file file be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1

AC_DEFUN(AC_LC_MESSAGES,
  [if test $ac_cv_header_locale_h = yes; then
    AC_CACHE_CHECK([for LC_MESSAGES], ac_cv_val_LC_MESSAGES,
      [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       ac_cv_val_LC_MESSAGES=yes, ac_cv_val_LC_MESSAGES=no)])
    if test $ac_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES)
    fi
  fi])


# Check for functions in math library.
# Ulrich Drepper <drepper@cygnus.com>, 1998.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1

dnl AC_REPLACE_MATHFUNCS(FUNCTION...)
AC_DEFUN(AC_REPLACE_MATHFUNCS,
[AC_CHECK_FUNCS([$1], , [LIBMATHOBJS="$LIBMATHOBJS ${ac_func}.lo"])
AC_SUBST(LIBMATHOBJS)dnl
])


# Check for string functions.
# Ulrich Drepper <drepper@cygnus.com>, 1998.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1

dnl AC_REPLACE_STRINGFUNCS(FUNCTION...)
AC_DEFUN(AC_REPLACE_STRINGFUNCS,
[AC_CHECK_FUNCS([$1], , [LIBSTRINGOBJS="$LIBSTRINGOBJS ${ac_func}.lo"])
AC_SUBST(LIBSTRINGOBJS)dnl
])
