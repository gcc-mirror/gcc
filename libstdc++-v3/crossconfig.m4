dnl
dnl This file contains stuff.
dnl

# Base decisions on target environment.
case "${host}" in
  *-darwin*)
    # Darwin versions vary, but the linker should work in a cross environment,
    # so we just check for all the features here.
    # Check for available headers.
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h machine/endian.h \
    machine/param.h sys/machine.h fp.h locale.h float.h inttypes.h gconv.h \
    sys/types.h])

    GLIBCXX_CHECK_COMPILER_FEATURES
    # Don't call GLIBCXX_CHECK_LINKER_FEATURES, Darwin doesn't have a GNU ld
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_BUILTIN_MATH_SUPPORT
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT

    # For showmanyc_helper().
    AC_CHECK_HEADERS(sys/ioctl.h sys/filio.h)
    GLIBCXX_CHECK_POLL
    GLIBCXX_CHECK_S_ISREG_OR_S_IFREG

    # For xsputn_2().
    AC_CHECK_HEADERS(sys/uio.h)
    GLIBCXX_CHECK_WRITEV

    AC_DEFINE(HAVE_LC_MESSAGES)

    AC_TRY_COMPILE(
      [#include <setjmp.h>],
      [sigjmp_buf env;
       while (! sigsetjmp (env, 1))
         siglongjmp (env, 1);
    ],
    [AC_DEFINE(HAVE_SIGSETJMP, 1, [Define if sigsetjmp is available.])])

    AC_DEFINE(HAVE_MMAP)
    ;;

  *-freebsd*)
    #os_include_dir="os/bsd/freebsd"
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h sys/resource.h sys/stat.h \
      sys/time.h unistd.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    AC_DEFINE(HAVE_LC_MESSAGES)
    AC_DEFINE(HAVE_DRAND48)
    AC_DEFINE(HAVE_GETPAGESIZE)
    AC_DEFINE(HAVE_SETENV)
    AC_DEFINE(HAVE_SIGSETJMP)
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
    AC_DEFINE(HAVE_FINITEF)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_HYPOTF)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)

    AC_DEFINE(HAVE_MMAP)
    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_CEILF)
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
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
    ;;
  *-hpux*)
    #os_include_dir="os/hpux"
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
    AC_DEFINE(HAVE_FREXPF)
    AC_DEFINE(HAVE_HYPOT)
    case "$target" in
      *-hpux10*)
	AC_DEFINE(HAVE_FINITE)
	AC_DEFINE(HAVE_FINITEF)
	AC_DEFINE(HAVE_ISINF)
	AC_DEFINE(HAVE_ISINFF)
	AC_DEFINE(HAVE_ISNAN)
	AC_DEFINE(HAVE_ISNANF)
	;;
    esac
    ;;
  *-linux*)
    #os_include_dir="os/gnu-linux"
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
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
  *-mingw32*)
    #os_include_dir="os/mingw32"
    AC_CHECK_HEADERS([sys/types.h locale.h float.h])
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    ;;
  *-netbsd*)
    #os_include_dir="os/bsd/netbsd"
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
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
    ;;
  *-qnx6.1* | *-qnx6.2*)
    #os_include_dir="os/qnx/qnx6.1"
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_WCHAR_T_SUPPORT
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
  *-solaris*)
    #case "$target" in
    #  *-solaris2.5)
    #    os_include_dir="os/solaris/solaris2.5"
    #    ;;
    #  *-solaris2.6)
    #    os_include_dir="os/solaris/solaris2.6"
    #    ;;
    #  *-solaris2.7 | *-solaris2.8 | *-solaris2.9)
    #    os_include_dir="os/solaris/solaris2.7"
    #    ;;
    #esac
    AC_DEFINE(HAVE_STRTOF)
    AC_DEFINE(HAVE_STRTOLD)
    AC_DEFINE(HAVE_MMAP) 
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)
    AC_DEFINE(HAVE_MODFF)
    AC_DEFINE(HAVE_HYPOT)
    ;;
  *-windiss*)
    #os_include_dir="os/windiss"
    AC_DEFINE(HAVE_ACOSF)
    AC_DEFINE(HAVE_ASINF)
    AC_DEFINE(HAVE_ATAN2F)
    AC_DEFINE(HAVE_ATANF)
    AC_DEFINE(HAVE_CEILF)
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
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
    ;;
  *)
    AC_MSG_ERROR([No support for this host/target combination.])
   ;;
esac

