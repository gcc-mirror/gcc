dnl
dnl This file contains details for non-native builds.
dnl

AC_DEFUN([GLIBCXX_CROSSCONFIG],[
# Base decisions on target environment.
case "${host}" in
  arm*-*-symbianelf*)
    # This is a freestanding configuration; there is nothing to do here.
    ;;

  mips*-sde-elf*)
    # These definitions are for the SDE C library rather than newlib.
    AC_CHECK_HEADERS([float.h inttypes.h locale.h \
      stdint.h stdlib.h string.h unistd.h wchar.h \
      machine/endian.h sys/ioctl.h sys/resource.h \
      sys/stat.h sys/time.h sys/types.h sys/uio.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_COMPILER_FEATURES
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_BUILTIN_MATH_SUPPORT
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT
    GLIBCXX_CHECK_S_ISREG_OR_S_IFREG
    AC_DEFINE(HAVE_WRITEV)

    AC_DEFINE(HAVE_LIBM)
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISINF)

    AC_DEFINE(HAVE_LDEXPF)
    AC_DEFINE(HAVE_MODF)
    AC_DEFINE(HAVE_SQRTF)
    ;;

  *-darwin*)
    # Darwin versions vary, but the linker should work in a cross environment,
    # so we just check for all the features here.
    # Check for available headers.
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h machine/endian.h \
    machine/param.h sys/machine.h fp.h locale.h float.h inttypes.h \
    sys/types.h])

    # Don't call GLIBCXX_CHECK_LINKER_FEATURES, Darwin doesn't have a GNU ld
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_BUILTIN_MATH_SUPPORT
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT

    # For showmanyc_helper().
    AC_CHECK_HEADERS(sys/ioctl.h sys/filio.h)
    GLIBCXX_CHECK_POLL
    GLIBCXX_CHECK_S_ISREG_OR_S_IFREG

    # For xsputn_2().
    AC_CHECK_HEADERS(sys/uio.h)
    GLIBCXX_CHECK_WRITEV

    AC_DEFINE(HAVE_LC_MESSAGES)
    ;;

  *djgpp)
    AC_CHECK_HEADERS([float.h ieeefp.h inttypes.h locale.h \
      memory.h stdint.h stdlib.h strings.h string.h unistd.h \
      wchar.h wctype.h machine/endian.h sys/ioctl.h sys/param.h \
      sys/resource.h sys/stat.h sys/time.h sys/types.h sys/uio.h])
    # GLIBCXX_CHECK_MATH_SUPPORT
    AC_DEFINE(HAVE_LIBM)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_FINITE)
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_SINCOS)
    AC_DEFINE(HAVE_HYPOT)
    # GLIBCXX_CHECK_BUILTIN_MATH_SUPPORT
    AC_DEFINE(HAVE___BUILTIN_ABS)
    AC_DEFINE(HAVE___BUILTIN_FABSF)
    AC_DEFINE(HAVE___BUILTIN_FABS)
    AC_DEFINE(HAVE___BUILTIN_FABSL)
    AC_DEFINE(HAVE___BUILTIN_LABS)
    AC_DEFINE(HAVE___BUILTIN_SQRTF)
    AC_DEFINE(HAVE___BUILTIN_SQRT)
    AC_DEFINE(HAVE___BUILTIN_SQRTL)
    AC_DEFINE(HAVE___BUILTIN_SINF)
    AC_DEFINE(HAVE___BUILTIN_SIN)
    AC_DEFINE(HAVE___BUILTIN_SINL)
    AC_DEFINE(HAVE___BUILTIN_COSF)
    AC_DEFINE(HAVE___BUILTIN_COS)
    AC_DEFINE(HAVE___BUILTIN_COSL)
    # GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    # GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_COPYSIGNF)
    # GLIBCXX_CHECK_S_ISREG_OR_S_IFREG
    AC_DEFINE(HAVE_S_ISREG)
    AC_DEFINE(HAVE_S_IFREG)
    AC_DEFINE(HAVE_WRITEV)
    ;;

  *-freebsd*)
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h sys/resource.h sys/stat.h \
      sys/time.h unistd.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    AC_DEFINE(HAVE_LC_MESSAGES)
    AC_DEFINE(HAVE_SETENV)
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
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
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
  *-linux* | *-uclinux* | *-gnu* | *-kfreebsd*-gnu | *-knetbsd*-gnu)
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h float.h endian.h inttypes.h locale.h float.h stdint.h \
      sys/ipc.h sys/sem.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_COMPILER_FEATURES
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_MATH_SUPPORT
    GLIBCXX_CHECK_BUILTIN_MATH_SUPPORT
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    GLIBCXX_CHECK_STDLIB_SUPPORT

    # For LFS.
    GLIBCXX_CHECK_LFS

    # For showmanyc_helper().
    AC_CHECK_HEADERS(sys/ioctl.h sys/filio.h)
    GLIBCXX_CHECK_POLL
    GLIBCXX_CHECK_S_ISREG_OR_S_IFREG

    # For xsputn_2().
    AC_CHECK_HEADERS(sys/uio.h)
    GLIBCXX_CHECK_WRITEV

    AC_DEFINE(_GLIBCXX_USE_RANDOM_TR1)

    AC_LC_MESSAGES

    # For iconv support.
    AM_ICONV
    ;;
  *-mingw32*)
    AC_CHECK_HEADERS([sys/types.h locale.h float.h])
    AC_DEFINE(HAVE_STRTOF)
    AC_DEFINE(HAVE_STRTOLD)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    ;;
  *-netbsd*)
    AC_CHECK_HEADERS([nan.h ieeefp.h endian.h sys/isa_defs.h \
      machine/endian.h machine/param.h sys/machine.h sys/types.h \
      fp.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
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
  *-netware)
    AC_CHECK_HEADERS([nan.h ieeefp.h sys/isa_defs.h sys/machine.h \
      sys/types.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
    AC_DEFINE(HAVE_HYPOT)
    AC_DEFINE(HAVE_ISINF)
    AC_DEFINE(HAVE_ISNAN)

    # For showmanyc_helper().
    AC_CHECK_HEADERS(sys/ioctl.h sys/filio.h)
    GLIBCXX_CHECK_POLL
    GLIBCXX_CHECK_S_ISREG_OR_S_IFREG

    # For xsputn_2().
    AC_CHECK_HEADERS(sys/uio.h)
    GLIBCXX_CHECK_WRITEV
    ;;
  *-qnx6.1* | *-qnx6.2*)
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS) 
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
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
    case "$target" in
      *-solaris2.7 | *-solaris2.8 | *-solaris2.9 | *-solaris2.10)
         GLIBCXX_CHECK_LINKER_FEATURES
         AC_DEFINE(HAVE_MBSTATE_T)
         AC_DEFINE(HAVE_POLL)
         AC_DEFINE(HAVE_S_ISREG)
         AC_DEFINE(HAVE_LC_MESSAGES)
         AC_DEFINE(HAVE_FINITE)
         AC_DEFINE(HAVE_FPCLASS)
         # All of the dependencies for wide character support are here, so
         # turn it on. 
         AC_DEFINE(_GLIBCXX_USE_WCHAR_T) 
         # Are these tested for even when cross?
         AC_DEFINE(HAVE_FLOAT_H)
         AC_DEFINE(HAVE_IEEEFP_H)
         AC_DEFINE(HAVE_INTTYPES_H)
         AC_DEFINE(HAVE_LOCALE_H)
         AC_DEFINE(HAVE_NAN_H)
         AC_DEFINE(HAVE_SYS_FILIO_H)
         AC_DEFINE(HAVE_SYS_IOCTL_H)
         AC_DEFINE(HAVE_SYS_ISA_DEFS_H)
         AC_DEFINE(HAVE_SYS_RESOURCE_H)
         AC_DEFINE(HAVE_SYS_TIME_H)
         AC_DEFINE(HAVE_SYS_TYPES_H)
         AC_DEFINE(HAVE_UNISTD_H)
         AC_DEFINE(HAVE_WCHAR_H)
         AC_DEFINE(HAVE_WCTYPE_H)
         AC_DEFINE(HAVE_LIBM)
        ;;
    esac
    case "$target" in
      sparc*-*-solaris2.8 | sparc*-*-solaris2.9 | sparc*-*-solaris2.10)
        # I saw these on sparc-sun-solaris2.8, but not 2.6, and not on i386
        AC_DEFINE(HAVE___BUILTIN_ABS)
        AC_DEFINE(HAVE___BUILTIN_LABS)
        AC_DEFINE(HAVE___BUILTIN_FABS)
        AC_DEFINE(HAVE___BUILTIN_FABSF)
        AC_DEFINE(HAVE___BUILTIN_FABSL)
        AC_DEFINE(HAVE___BUILTIN_COS)
        AC_DEFINE(HAVE___BUILTIN_COSF)
        AC_DEFINE(HAVE___BUILTIN_SIN)
        AC_DEFINE(HAVE___BUILTIN_SINF)
       ;;
    esac
    case "$target" in
      *-*-solaris2.10)
      # These two C99 functions are present only in Solaris >= 10
      AC_DEFINE(HAVE_STRTOF)
      AC_DEFINE(HAVE_STRTOLD)
     ;;
    esac
    AC_DEFINE(HAVE_COPYSIGN)
    AC_DEFINE(HAVE_ISNAN)
    AC_DEFINE(HAVE_ISNANF)
    AC_DEFINE(HAVE_MODFF)
    AC_DEFINE(HAVE_HYPOT)
    ;;
  *-tpf)
    AC_CHECK_HEADERS([nan.h endian.h machine/endian.h  \
      sys/param.h sys/types.h locale.h float.h inttypes.h])
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
    AC_SUBST(SECTION_FLAGS)
    GLIBCXX_CHECK_LINKER_FEATURES
    GLIBCXX_CHECK_COMPLEX_MATH_SUPPORT
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
    ;;
  *)
    AC_MSG_ERROR([No support for this host/target combination.])
   ;;
esac
])
