// acconfig.h symbols and macros for libstdc++ v3 -*- C++ -*-

// If using the namespace std, you need this. Eventually this should
// not be an option. In the meantime, and as things like std_ctype.h
// need to be hacked out, give people the option. If this is set to 1,
// CXXFLAGS should include -fhonor-std. If this is undefined, CXXFLAGS
// should include -fno-honor-std.
#undef _GLIBCPP_USE_NAMESPACES

// Include support for 'long long' and 'unsigned long long'.
#undef _GLIBCPP_USE_LONG_LONG

// Define if the host has a type mbstate_t defined in
// wchar.h, as required by 21.1.3.1. Some systems, namely
// hppa-hp-hpux10.20 do not meet this requirement, and must be worked
// around.
#undef _GLIBCPP_NEED_MBSTATE_T

// Define if WCHAR_MIN and WCHAR_MAX are in the <cwchar_t>
// header. Presently, this is needed so that solaris won't
// instantitate numeric_limits<wchar_t>
#undef _GLIBCPP_HAS_WCHAR_MIN_MAX

// Define if code specialized for wchar_t should be used.
#undef _GLIBCPP_USE_WCHAR_T

// Define if the compiler/host combination has __builtin_sinf defined.
#undef _GLIBCPP_HAS_BUILTIN_SINF

// Define if the compiler/host combination has __builtin_cosf defined.
#undef _GLIBCPP_HAS_BUILTIN_COSF

// Define if the compiler/host combination has __builtin_fabsf defined.
#undef _GLIBCPP_HAS_BUILTIN_FABSF

// Define if the compiler/host combination has __builtin_fabsf defined.
#undef _GLIBCPP_HAS_BUILTIN_SQRTF

// Define if GCC support for __complex__ float is buggy.
#undef _GLIBCPP_BUGGY_FLOAT_COMPLEX

// Define if GCC support for __complex__ is buggy.
#undef _GLIBCPP_BUGGY_COMPLEX

// Define if LC_MESSAGES is available in <locale.h>.
#undef HAVE_LC_MESSAGES

// Define if <float.h> exists.
#undef HAVE_FLOAT_H

// Define if modf is present in <math.h>
#undef HAVE_MODF

// @BOTTOM@
//
// Systems that have certain non-standard functions prefixed with an
// underscore, we'll handle those here. Must come after config.h.in.
//

#if defined (HAVE__ISNAN) && ! defined (HAVE_ISNAN)
# define HAVE_ISNAN 1
# define isnan _isnan
#endif

#if defined (HAVE__ISNANF) && ! defined (HAVE_ISNANF)
# define HAVE_ISNANF 1
# define isnanf _isnanf
#endif

#if defined (HAVE__ISNANL) && ! defined (HAVE_ISNANL)
# define HAVE_ISNANL 1
# define isnanl _isnanl
#endif

#if defined (HAVE__ISINF) && ! defined (HAVE_ISINF)
# define HAVE_ISINF 1
# define isinf _isinf
#endif

#if defined (HAVE__ISINFF) && ! defined (HAVE_ISINFF)
# define HAVE_ISINFF 1
# define isinff _isinff
#endif

#if defined (HAVE__ISINFL) && ! defined (HAVE_ISINFL)
# define HAVE_ISINFL 1
# define isinfl _isinfl
#endif

#if defined (HAVE__COPYSIGN) && ! defined (HAVE_COPYSIGN)
# define HAVE_COPYSIGN 1
# define copysign _copysign
#endif

#if defined (HAVE__COPYSIGNL) && ! defined (HAVE_COPYSIGNL)
# define HAVE_COPYSIGNL 1
# define copysignl _copysignl
#endif

#if defined (HAVE__COSF) && ! defined (HAVE_COSF)
# define HAVE_COSF 1
# define cosf _cosf
#endif

#if defined (HAVE__COSHF) && ! defined (HAVE_COSHF)
# define HAVE_COSHF 1
# define coshf _coshf
#endif

#if defined (HAVE__LOGF) && ! defined (HAVE_LOGF)
# define HAVE_LOGF 1
# define logf _logf
#endif

#if defined (HAVE__LOG10F) && ! defined (HAVE_LOG10F)
# define HAVE_LOG10F 1
# define log10f _log10f
#endif

#if defined (HAVE__POWF) && ! defined (HAVE_POWF)
# define HAVE_POWF 1
# define powf _powf
#endif

#if defined (HAVE__SINF) && ! defined (HAVE_SINF)
# define HAVE_SINF 1
# define sinf _sinf
#endif

#if defined (HAVE__SINHF) && ! defined (HAVE_SINHF)
# define HAVE_SINHF 1
# define sinhf _sinhf
#endif

#if defined (HAVE__SQRTF) && ! defined (HAVE_SQRTF)
# define HAVE_SQRTF 1
# define sqrtf _sqrtf
#endif

#if defined (HAVE__TANF) && ! defined (HAVE_TANF)
# define HAVE_TANF 1
# define tanf _tanf
#endif

#if defined (HAVE__TANHF) && ! defined (HAVE_TANHF)
# define HAVE_TANHF 1
# define tanhf _tanhf
#endif

#if defined (HAVE__STRTOF) && ! defined (HAVE_STRTOF)
# define HAVE_STRTOF 1
# define strtof _strtof
#endif

#if defined (HAVE__STRTOLD) && ! defined (HAVE_STRTOLD)
# define HAVE_STRTOLD 1
# define strtold _strtold
#endif

#if defined (HAVE__FABSF) && ! defined (HAVE_FABSF)
# define HAVE_FABSF 1
# define fabsf _fabsf
#endif

#if defined (HAVE__SINCOS) && ! defined (HAVE_SINCOS)
# define HAVE_SINCOS 1
# define sincos _sincos
#endif

#if defined (HAVE__SINCOSF) && ! defined (HAVE_SINCOSF)
# define HAVE_SINCOSF 1
# define sincosf _sincosf
#endif

#if defined (HAVE__SINCOSL) && ! defined (HAVE_SINCOSL)
# define HAVE_SINCOSL 1
# define sincosl _sincosl
#endif

#if defined (HAVE__FINITE) && ! defined (HAVE_FINITE)
# define HAVE_FINITE 1
# define finite _finite
#endif

#if defined (HAVE__QFINITE) && ! defined (HAVE_QFINITE)
# define HAVE_QFINITE 1
# define qfinite _qfinite
#endif

#if defined (HAVE__FPCLASS) && ! defined (HAVE_FPCLASS)
# define HAVE_FPCLASS 1
# define fpclass _fpclass
#endif

#if defined (HAVE__QFPCLASS) && ! defined (HAVE_QFPCLASS)
# define HAVE_QFPCLASS 1
# define qfpclass _qfpclass
#endif

