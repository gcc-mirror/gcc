dnl
dnl This file contains details for non-native builds.
dnl

AC_DEFUN([GLIBCXX_CROSSCONFIG],[
# Base decisions on target environment.
case "${host}" in
  arm*-*-symbianelf*)
    # This is a freestanding configuration; there is nothing to do here.
    ;;

  avr*-*-*)
    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_CEILF)
    AC_DEFINE(HAVE_COSF)
    AC_DEFINE(HAVE_COSHF)
    AC_DEFINE(HAVE_EXPF)
    AC_DEFINE(HAVE_FABSF)
    AC_DEFINE(HAVE_FLOORF)
    AC_DEFINE(HAVE_FMODF)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_SQRTF)
    AC_DEFINE(HAVE_HYPOTF)
    AC_DEFINE(HAVE_LDEXPF)
    AC_DEFINE(HAVE_LOG10F)
    AC_DEFINE(HAVE_LOGF)
    AC_DEFINE(HAVE_MODFF)
    AC_DEFINE(HAVE_POWF)
    AC_DEFINE(HAVE_SINF)
    AC_DEFINE(HAVE_SINHF)
    AC_DEFINE(HAVE_TANF)
    AC_DEFINE(HAVE_TANHF)
    ;;

  mips*-sde-elf*)
    # These definitions are for the SDE C library rather than newlib.
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_COMPILER_FEATURES
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT

    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISINF)

    AC_DEFINE(HAVE_LDEXPF)
    AC_DEFINE(HAVE_MODF)
    AC_DEFINE(HAVE_SQRTF)
    ;;

  *-aix*)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_DEFINE(_GLIBCXX_USE_DEV_RANDOM)
    AC_DEFINE(_GLIBCXX_USE_RANDOM_TR1)
    # We don't yet support AIX's TLS ABI.
    #GCC_CHECK_TLS
    AM_ICONV
    ;;

  *-darwin*)
    # Darwin versions vary, but the linker should work in a cross environment,
    # so we just check for all the features here.
    # Check for available headers.

    # Don't call GLIBCXX_CHECK_LINKER_FEATURES, Darwin doesn't have a GNU ld
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    ;;

  *djgpp)
    # GLIBCXX_CHECK_MATH_SUPPORT
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_SINCOS)
    AC_DEFINE(HAVE_HYPOT)
    ;;

  *-freebsd*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    AC_DEFINE(HAVE_SETENV)
    AC_DEFINE(HAVE_FINITEF)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_HYPOTF)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)

    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_CEILF)
    AC_DEFINE(HAVE_COSF)
    AC_DEFINE(HAVE_COSHF)
    AC_DEFINE(HAVE_EXPF)
    AC_DEFINE(HAVE_FABSF)
    AC_DEFINE(HAVE_FLOORF)
    AC_DEFINE(HAVE_FMODF)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_LDEXPF)
    AC_DEFINE(HAVE_LOG10F)
    AC_DEFINE(HAVE_LOGF)
    AC_DEFINE(HAVE_MODFF)
    AC_DEFINE(HAVE_POWF)
    AC_DEFINE(HAVE_SINF)
    AC_DEFINE(HAVE_SINHF)
    AC_DEFINE(HAVE_SQRTF)
    AC_DEFINE(HAVE_TANF)
    AC_DEFINE(HAVE_TANHF)
    if test x"long_double_math_on_this_cpu" = x"yes"; then
      AC_DEFINE(HAVE_FINITEL)
      AC_DEFINE(HAVE_ISINFL)
      AC_DEFINE(HAVE_ISNANL)
    fi
    AC_CHECK_FUNCS(__cxa_thread_atexit)
    AC_CHECK_FUNCS(aligned_alloc posix_memalign memalign _aligned_malloc)
    AC_CHECK_FUNCS(timespec_get)
    AC_CHECK_FUNCS(sockatmark)
    ;;

  *-fuchsia*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    ;;

  *-hpux*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES

    # GLIBCXX_CHECK_MATH_SUPPORT
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_COSF)
    AC_DEFINE(HAVE_COSHF)
    AC_DEFINE(HAVE_SINF)
    AC_DEFINE(HAVE_SINHF)
    AC_DEFINE(HAVE_TANF)
    AC_DEFINE(HAVE_TANHF)
    AC_DEFINE(HAVE_EXPF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_FABSF)
    AC_DEFINE(HAVE_FMODF)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_LOGF)
    AC_DEFINE(HAVE_LOG10F)
    AC_DEFINE(HAVE_MODF)
    AC_DEFINE(HAVE_POWF)
    AC_DEFINE(HAVE_SQRTF)

    # GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_DEFINE(HAVE_STRTOLD)

    GCC_CHECK_TLS
    case "$target" in
      *-hpux10*)
	AC_DEFINE(HAVE_ISINF)
	AC_DEFINE(HAVE_ISINFF)
	AC_DEFINE(HAVE_ISNANF)
	AC_DEFINE(HAVE_FINITE)
	AC_DEFINE(HAVE_FINITEF)
	;;
    esac
    ;;
  *-linux* | *-uclinux* | *-gnu* | *-kfreebsd*-gnu | *-cygwin* | *-solaris*)
    GLIBCXX_CHECK_COMPILER_FEATURES
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_DEFINE(_GLIBCXX_USE_DEV_RANDOM)
    AC_DEFINE(_GLIBCXX_USE_RANDOM_TR1)
    GCC_CHECK_TLS
    AC_CHECK_FUNCS(__cxa_thread_atexit_impl)
    AC_CHECK_FUNCS(aligned_alloc posix_memalign memalign _aligned_malloc)
    AC_CHECK_FUNCS(timespec_get)
    AC_CHECK_FUNCS(sockatmark)
    AM_ICONV
    ;;
  *-mingw32*)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_CHECK_FUNCS(aligned_alloc posix_memalign memalign _aligned_malloc)
    AC_CHECK_FUNCS(_wfopen)
    ;;
  *-netbsd* | *-openbsd*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    AC_DEFINE(HAVE_FINITEF)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_HYPOTF)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISINFF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)
    if test x"long_double_math_on_this_cpu" = x"yes"; then
      AC_DEFINE(HAVE_FINITEL)
      AC_DEFINE(HAVE_ISINFL)
      AC_DEFINE(HAVE_ISNANL)
    fi
    AC_CHECK_FUNCS(aligned_alloc posix_memalign memalign _aligned_malloc)
    AC_CHECK_FUNCS(timespec_get)
    AC_CHECK_FUNCS(sockatmark)
    ;;
  *-qnx6.1* | *-qnx6.2*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    AC_DEFINE(HAVE_COSF)
    AC_DEFINE(HAVE_COSL)
    AC_DEFINE(HAVE_COSHF)
    AC_DEFINE(HAVE_COSHL)
    AC_DEFINE(HAVE_LOGF)
    AC_DEFINE(HAVE_LOGL)
    AC_DEFINE(HAVE_LOG10F)
    AC_DEFINE(HAVE_LOG10L)
    AC_DEFINE(HAVE_SINF)
    AC_DEFINE(HAVE_SINL)
    AC_DEFINE(HAVE_SINHF)
    AC_DEFINE(HAVE_SINHL)
    ;;
  *-rtems*)
    GLIBCXX_CHECK_COMPILER_FEATURES
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    ;;
  *-tpf)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    SECTION_LDFLAGS='-Wl,--gc-sections $SECTION_LDFLAGS'
    AC_SUBST(SECTION_FLAGS)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_FINITEF)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_HYPOTF)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISINFF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)
    AC_DEFINE(HAVE_SINCOS)
    AC_DEFINE(HAVE_SINCOSF)
    if test x"long_double_math_on_this_cpu" = x"yes"; then
      AC_DEFINE(HAVE_FINITEL)
      AC_DEFINE(HAVE_HYPOTL)
      AC_DEFINE(HAVE_ISINFL)
      AC_DEFINE(HAVE_ISNANL)
    fi
    ;;
  *-*vms*)
    # Check for available headers.
    # Don't call GLIBCXX_CHECK_LINKER_FEATURES, VMS doesn't have a GNU ld
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    ;;
  *-vxworks)
    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_CEILF)
    AC_DEFINE(HAVE_COSF)
    AC_DEFINE(HAVE_COSHF)
    AC_DEFINE(HAVE_EXPF)
    AC_DEFINE(HAVE_FABSF)
    AC_DEFINE(HAVE_FLOORF)
    AC_DEFINE(HAVE_FMODF)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_LOG10F)
    AC_DEFINE(HAVE_LOGF)
    AC_DEFINE(HAVE_POWF)
    AC_DEFINE(HAVE_SINF)
    AC_DEFINE(HAVE_SINHF)
    AC_DEFINE(HAVE_SQRTF)
    AC_DEFINE(HAVE_TANF)
    AC_DEFINE(HAVE_TANHF)

dnl # Different versions and execution modes implement different
dnl # subsets of these functions.  Instead of hard-coding, test for C
dnl # declarations in headers.  The C primitives could be defined as
dnl # macros, in which case the tests might fail, and we might have to
dnl # switch to more elaborate tests.
    GLIBCXX_CHECK_MATH_DECLS([
      acosl asinl atan2l atanl ceill cosl coshl expl fabsl floorl fmodl
      frexpl ldexpl log10l logl modfl powl sinl sinhl sqrtl tanl tanhl])
dnl # sincosl is the only one missing here, compared with the *l
dnl # functions in the list guarded by
dnl # long_double_math_on_this_cpu in configure.ac, right after
dnl # the expansion of the present macro.
    ;;
  *)
    AC_MSG_ERROR([No support for this host/target combination.])
   ;;
esac
])


dnl
dnl Check to see if the (math function) argument passed is
dnl declared when using the c compiler
dnl
dnl Define HAVE_CARGF etc if "cargf" is declared
dnl
dnl argument 1 is name of function to check
dnl
dnl ASSUMES argument is a math function
dnl
dnl GLIBCXX_CHECK_MATH_DECL
AC_DEFUN([GLIBCXX_CHECK_MATH_DECL], [
  AC_CACHE_CHECK([for $1 declaration],
    [glibcxx_cv_func_$1_use], [
      AC_LANG_SAVE
      AC_LANG_C
      AC_TRY_COMPILE([
#include <math.h>
#ifdef HAVE_IEEEFP_H
# include <ieeefp.h>
#endif
#undef $1
], [
  void (*f)(void) = (void (*)(void))$1;
], [glibcxx_cv_func_$1_use=yes
], [glibcxx_cv_func_$1_use=no])])
  if test "x$glibcxx_cv_func_$1_use" = xyes; then
    AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_$1]))
  fi
])

dnl
dnl Check to see whether multiple math functions are
dnl declared when using the c compiler
dnl
dnl Define HAVE_CARGF HAVE_POWL etc if "cargf" and "powl"
dnl are declared
dnl
dnl argument 1 is a word list naming function to check
dnl
dnl ASSUMES arguments are math functions
dnl
dnl GLIBCXX_CHECK_MATH_DECLS
AC_DEFUN([GLIBCXX_CHECK_MATH_DECLS], [
  m4_foreach_w([glibcxx_func], [$1], [
    GLIBCXX_CHECK_MATH_DECL(glibcxx_func)
  ])
])
