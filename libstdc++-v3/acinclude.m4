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
dnl Check to see if g++ can compile this library. 
dnl
dnl Define OPTLEVEL='-O2' if new inlining code present.
dnl
dnl GLIBCPP_CHECK_COMPILER_VERSION
AC_DEFUN(GLIBCPP_CHECK_COMPILER_VERSION, [
  AC_MSG_CHECKING([for g++ that will successfullly compile this code])
  AC_EGREP_CPP([ok], [
  #if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95) 
    ok
  #endif
  ], gpp_satisfactory=yes, AC_MSG_ERROR("please upgrade to gcc-2.95 or above"))
  AC_MSG_RESULT($gpp_satisfactory)

  AC_MSG_CHECKING([for g++ that supports new inlining mechanism])
  AC_EGREP_CPP([ok], [
  #if  __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 95)
    ok
  #endif
  ], [OPTLEVEL='-O2'
      WERRORSUPPRESS=
  ], [OPTLEVEL=
      WERRORSUPPRESS=-Wno-error
  ])
  if test "$OPTLEVEL" = ""; then
    AC_MSG_RESULT(no)
  else
    AC_MSG_RESULT(yes)
  fi
  AC_SUBST(OPTLEVEL)
  AC_SUBST(WERRORSUPPRESS)
])


dnl
dnl Check to see what builtin math functions are supported
dnl
dnl Define _GLIBCPP_HAS_BUILTIN_SINF if __builtin_sinf
dnl Define _GLIBCPP_HAS_BUILTIN_COSF if __builtin_cosf
dnl Define _GLIBCPP_HAS_BUILTIN_FABSF if __builtin_fabsf
dnl Define _GLIBCPP_HAS_BUILTIN_SQRTF if __builtin_sqrtf
dnl
dnl GLIBCPP_CHECK_BUILTIN_MATH_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_BUILTIN_MATH_SUPPORT, [
  dnl Test for builtin math functions.
  AC_MSG_CHECKING([for __builtin_sinf])
  AC_TRY_COMPILE([#include <math.h>], 
  [float foo(void) { __builtin_sinf(0.0); }], 
  use_builtin_sinf=yes, use_builtin_sinf=no)
  AC_MSG_RESULT($use_builtin_sinf)
  if test $use_builtin_sinf = "yes"; then
    AC_DEFINE(_GLIBCPP_HAS_BUILTIN_SINF)
  fi

  AC_MSG_CHECKING([for __builtin_cosf])
  AC_TRY_COMPILE([#include <math.h>], 
  [float foo(void) { __builtin_cosf(0.0); }], 
  use_builtin_cosf=yes, use_builtin_cosf=no)
  AC_MSG_RESULT($use_builtin_cosf)
  if test $use_builtin_cosf = "yes"; then
    AC_DEFINE(_GLIBCPP_HAS_BUILTIN_COSF)
  fi

  AC_MSG_CHECKING([for __builtin_fabsf])
  AC_TRY_COMPILE([#include <math.h>], 
  [float foo(void) { __builtin_fabsf(0.0); }], 
  use_builtin_fabsf=yes, use_builtin_fabsf=no)
  AC_MSG_RESULT($use_builtin_fabsf)
  if test $use_builtin_fabsf = "yes"; then
    AC_DEFINE(_GLIBCPP_HAS_BUILTIN_FABSF)
  fi

  AC_MSG_CHECKING([for __builtin_sqrtf])
  AC_TRY_COMPILE([#include <math.h>], 
  [float foo(void) { __builtin_sqrtf(0.0); }], 
  use_builtin_sqrtf=yes, use_builtin_sqrtf=no)
  AC_MSG_RESULT($use_builtin_sqrtf)
  if test $use_builtin_sqrtf = "yes"; then
    AC_DEFINE(_GLIBCPP_HAS_BUILTIN_SQRTF)
  fi
])


dnl

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
      i486 | i586 | i686 | i786)
	cpu_include_dir="config/cpu/i386"
        ;;
      powerpc | rs6000)
	cpu_include_dir="config/cpu/powerpc"
    	CPUFLAGS='-mnew-mnemonics -Wa,-mppc -mpowerpc'
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
        AC_MSG_RESULT("solaris2.[6,7]")
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

  dnl Check to see if basic C math functions have faster float versions.
  AC_CHECK_FUNCS(modf isnan isnanf isnanl isinf isinff isinfl copysign \
  copysignl cosf coshf logf log10f powf sinf sinhf sqrtf tanf tanhf \
  strtof strtold fabsf sincos sincosf sincosl finite finite fqfinite \
  fpclass qfpclass)

#Some runtimes have these functions with a preceding underscore. Please
# keep this sync'd with the one above. And if you add any new symbol,
# please add the corresponding block in the @BOTTOM@ section of
# acconfig.h.
AC_CHECK_FUNCS(_modf _isnan _isnanf _isnanl _isinf _isinff _isinfl _copysign \
_copysignl _cosf _coshf _logf _log10f _powf _sinf _sinhf _sqrtf _tanf _tanhf \
_strtof _strtold _fabsf _sincos _sincosf _sincosl _finite _finitef _qfinite \
_fpclass _qfpclass)

LIBS="$save_LIBS"
])


dnl
dnl Check to see if this target can enable the wchar_t parts of libstdc++.
dnl
dnl Define _GLIBCPP_USE_WCHAR_T if all the bits are found 
dnl Define _GLIBCPP_NEED_MBSTATE_T if mbstate_t is not in wchar.h
dnl Define _GLIBCPP_HAS_WCHAR_MIN_MAX if WCHAR_MIN, WCHAR_MAX in wchar.h
dnl
dnl GLIBCPP_CHECK_WCHAR_T_SUPPORT
AC_DEFUN(GLIBCPP_CHECK_WCHAR_T_SUPPORT, [
  AC_CHECK_HEADER(wchar.h,[
  dnl Test wchar.h for mbstate_t, which is needed for char_traits and others.
  AC_MSG_CHECKING([for native mbstate_t])
  AC_TRY_COMPILE([#include <wchar.h>],
  [mbstate_t teststate;], 
  use_native_mbstatet=yes, use_native_mbstatet=no)
  AC_MSG_RESULT($use_native_mbstatet)
  if test $use_native_mbstatet = "no"; then
    AC_DEFINE(_GLIBCPP_NEED_MBSTATE_T)
  fi
  
  dnl Test wchar.h for WCHAR_MIN, WCHAR_MAX, which is needed before
  dnl numeric_limits can instantiate type_traits<wchar_t>
  AC_MSG_CHECKING([for WCHAR_MIN and WCHAR_MAX])
  AC_TRY_COMPILE([#include <wchar.h>],
  [int i = WCHAR_MIN; int j = WCHAR_MAX;], 
  has_wchar_minmax=yes, has_wchar_minmax=no)
  AC_MSG_RESULT($has_wchar_minmax)
  if test $has_wchar_minmax = "yes"; then
    AC_DEFINE(_GLIBCPP_HAS_WCHAR_MIN_MAX)
  fi
  
  # Test wchar.h for WEOF, which is what we use to determine whether
  # to specialize for wchar_t or not.
  AC_MSG_CHECKING([for WEOF])
  AC_TRY_COMPILE([
    #include <wchar.h>
    #include <stddef.h>],
  [wint_t i = WEOF;],
  has_weof=yes, has_weof=no)
  AC_MSG_RESULT($has_weof)

  dnl Tests for wide character functions.
  AC_REPLACE_STRINGFUNCS(wcslen wmemchr wmemcmp wmemcpy wmemmove wmemset)
  AC_SUBST(libinst_wstring_la)

  AC_MSG_CHECKING([for wide character support])
  if test $has_weof = "yes" && test $has_wchar_minmax = "yes"; then
    libinst_wstring_la="libinst-wstring.la"
    AC_DEFINE(_GLIBCPP_USE_WCHAR_T)
    AC_MSG_RESULT(ok)
  else
    libinst_wstring_la=""
    AC_MSG_RESULT("not specializing for wchar_t")
  fi
  ],[
  AC_MSG_WARN([<wchar.h> not found])
  AC_DEFINE(_GLIBCPP_NEED_MBSTATE_T)
  ])
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
dnl Check for certain special build configurations.
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
dnl Check for certain special build configurations.
dnl
dnl GLIBCPP_ENABLE_NAMESPACES
dnl --enable-namespaces sets '-fhonor-std' and defines _GLIBCPP_USE_NAMESPACES
dnl --disable-namespaces sets '-fno-honor-std' (the macro should be
dnl     undefined by default in whatever.h.in).
dnl  +  Eventually, this will go away.
dnl  +  Usage:  GLIBCPP_ENABLE_NAMESPACES[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.  If ommitted, it
dnl       defaults to `no'.
AC_DEFUN(GLIBCPP_ENABLE_NAMESPACES, [dnl
define([GLIBCPP_ENABLE_NAMESPACES_DEFAULT], ifelse($1, yes, yes, no))dnl
AC_ARG_ENABLE(namespaces,
changequote(<<, >>)dnl
<<  --enable-namespaces     turns on 'std' [default=>>GLIBCPP_ENABLE_NAMESPACES_DEFAULT],
changequote([, ])dnl
[case "$enableval" in
 yes) enable_namespaces=yes ;;
 no)  enable_namespaces=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable namespaces]) ;;
 esac],
enable_namespaces=GLIBCPP_ENABLE_NAMESPACES_DEFAULT)dnl
dnl Option parsed, now set things appropriately
case "$enable_namespaces" in
    yes)  NAMESPACES='-fhonor-std'
          AC_DEFINE(_GLIBCPP_USE_NAMESPACES)
          ;;
    no)   NAMESPACES='-fno-honor-std'
          ;;
esac
AC_SUBST(NAMESPACES)
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
dnl Check for certain special build configurations.
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
dnl Check for certain special build configurations.
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
dnl Check for certain special build configurations.
dnl
dnl GLIBCPP_ENABLE_LONG_LONG
dnl --enable-long-long defines _GLIBCPP_USE_LONG_LONG
dnl --disable-long-long leaves _GLIBCPP_USE_LONG_LONG undefined
dnl  +  Usage:  GLIBCPP_ENABLE_LONG_LONG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.  If ommitted, it
dnl       defaults to `no'.
dnl
dnl GLIBCPP_ENABLE_LONG_LONG
AC_DEFUN(GLIBCPP_ENABLE_LONG_LONG, [dnl
  define([GLIBCPP_ENABLE_LONG_LONG_DEFAULT], ifelse($1, yes, yes, no))dnl
  AC_ARG_ENABLE(long-long,
  changequote(<<, >>)dnl
  <<--enable-long_long      turns on 'long long' [default=>>GLIBCPP_ENABLE_LONG_LONG_DEFAULT],
  changequote([, ])dnl
  [case "$enableval" in
   yes) enable_long_long=yes ;;
   no)  enable_long_long=no ;;
   *)   AC_MSG_ERROR([Unknown argument to enable/disable long long]) ;;
   esac],
  enable_long_long=GLIBCPP_ENABLE_LONG_LONG_DEFAULT)dnl
  dnl Option parsed, now set things appropriately
  case "$enable_long_long" in
    yes)  AC_DEFINE(_GLIBCPP_USE_LONG_LONG)
          ;;
  esac
])












