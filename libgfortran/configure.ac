# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_PREREQ(2.59)
AC_INIT([GNU Fortran Runtime Library], 0.2,,[libgfortran])
AC_CONFIG_HEADER(config.h)
GCC_TOPLEV_SUBDIRS

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
[  --enable-version-specific-runtime-libs    Specify that runtime libraries should be installed in a compiler-specific directory ],
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)


# Gets build, host, target, *_vendor, *_cpu, *_os, etc.
#
# You will slowly go insane if you do not grok the following fact:  when
# building this library, the top-level /target/ becomes the library's /host/.
#
# configure then causes --target to default to --host, exactly like any
# other package using autoconf.  Therefore, 'target' and 'host' will
# always be the same.  This makes sense both for native and cross compilers
# just think about it for a little while.  :-)
#
# Also, if this library is being configured as part of a cross compiler, the
# top-level configure script will pass the "real" host as $with_cross_host.
#
# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

# Sets up automake.  Must come after AC_CANONICAL_SYSTEM.  Each of the
# following is magically included in AUTOMAKE_OPTIONS in each Makefile.am.
#  1.8.2:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  no-dependencies:  turns off auto dependency generation (just for now)
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is required.
AM_INIT_AUTOMAKE([1.8.2 no-define foreign no-dependencies -Wall -Wno-portability])

AM_MAINTAINER_MODE
AM_ENABLE_MULTILIB(, ..)

# Handy for debugging:
#AC_MSG_NOTICE($build / $host / $target / $host_alias / $target_alias); sleep 5

# Are we being configured with some form of cross compiler?
# NB: We don't actually need to know this just now, but when, say, a test
#     suite is included, we'll have to know.
if test "$build" != "$host"; then
  LIBGFOR_IS_NATIVE=false
  GCC_NO_EXECUTABLES
else
  LIBGFOR_IS_NATIVE=true
fi

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_alias)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_alias)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
      toolexeclibdir='$(libdir)'
    fi
    multi_os_directory=`$CC -print-multi-os-directory`
    case $multi_os_directory in
      .) ;; # Avoid trailing /.
      *) toolexeclibdir=$toolexeclibdir/$multi_os_directory ;;
    esac
    ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

# Add -Wall -fno-repack-arrays -fno-underscoring if we are using GCC.
if test "x$GCC" = "xyes"; then
  AM_FCFLAGS="-I . -Wall -fno-repack-arrays -fno-underscoring"
  ## We like to use C99 routines when available.  This makes sure that
  ## __STDC_VERSION__ is set such that libc includes make them available.
  AM_CFLAGS="-std=gnu99 -Wall -Wstrict-prototypes -Wmissing-prototypes -Wold-style-definition -Wextra -Wwrite-strings"
fi
AC_SUBST(AM_FCFLAGS)
AC_SUBST(AM_CFLAGS)

# Find other programs we need.
AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Configure libtool
#AC_MSG_NOTICE([====== Starting libtool configuration])
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)
#AC_MSG_NOTICE([====== Finished libtool configuration]) ; sleep 10

# We need gfortran to compile parts of the library
#AC_PROG_FC(gfortran)
FC="$GFORTRAN"
AC_PROG_FC(gfortran)

# extra LD Flags which are required for targets
case "${host}" in
  *-darwin*)
    # Darwin needs -single_module when linking libgfortran
    extra_ldflags_libgfortran=-Wl,-single_module
    ;;
esac
AC_SUBST(extra_ldflags_libgfortran)

AC_SYS_LARGEFILE
AC_FUNC_MMAP
AC_TYPE_OFF_T

# check header files
AC_STDC_HEADERS
AC_HAVE_HEADERS(stdlib.h stdio.h string.h stddef.h math.h unistd.h signal.h)
AC_CHECK_HEADERS(time.h sys/params.h sys/time.h sys/times.h sys/resource.h)
AC_CHECK_HEADERS(sys/mman.h sys/types.h sys/stat.h floatingpoint.h ieeefp.h)
AC_CHECK_HEADERS(fenv.h fptrap.h float.h)
AC_CHECK_HEADER([complex.h],[AC_DEFINE([HAVE_COMPLEX_H], [1], [complex.h exists])])

AC_CHECK_MEMBERS([struct stat.st_blksize])
AC_CHECK_MEMBERS([struct stat.st_blocks])
AC_CHECK_MEMBERS([struct stat.st_rdev])

# Check for library functions.
AC_CHECK_FUNCS(getrusage times mkstemp strtof strtold snprintf ftruncate chsize)
AC_CHECK_FUNCS(chdir strerror getlogin gethostname kill link symlink perror)
AC_CHECK_FUNCS(sleep time ttyname signal alarm ctime)

# Check libc for getgid, getpid, getuid
AC_CHECK_LIB([c],[getgid],[AC_DEFINE([HAVE_GETGID],[1],[libc includes getgid])])
AC_CHECK_LIB([c],[getpid],[AC_DEFINE([HAVE_GETPID],[1],[libc includes getpid])])
AC_CHECK_LIB([c],[getuid],[AC_DEFINE([HAVE_GETUID],[1],[libc includes getuid])])

# Check for C99 (and other IEEE) math functions
# ??? This list seems awful long. Is there a better way to test for these?
AC_CHECK_LIB([m],[acosf],[AC_DEFINE([HAVE_ACOSF],[1],[libm includes acosf])])
AC_CHECK_LIB([m],[acos],[AC_DEFINE([HAVE_ACOS],[1],[libm includes acos])])
AC_CHECK_LIB([m],[acosl],[AC_DEFINE([HAVE_ACOSL],[1],[libm includes acosl])])
AC_CHECK_LIB([m],[acoshf],[AC_DEFINE([HAVE_ACOSHF],[1],[libm includes acoshf])])
AC_CHECK_LIB([m],[acosh],[AC_DEFINE([HAVE_ACOSH],[1],[libm includes acosh])])
AC_CHECK_LIB([m],[acoshl],[AC_DEFINE([HAVE_ACOSHL],[1],[libm includes acoshl])])
AC_CHECK_LIB([m],[asinf],[AC_DEFINE([HAVE_ASINF],[1],[libm includes asinf])])
AC_CHECK_LIB([m],[asin],[AC_DEFINE([HAVE_ASIN],[1],[libm includes asin])])
AC_CHECK_LIB([m],[asinl],[AC_DEFINE([HAVE_ASINL],[1],[libm includes asinl])])
AC_CHECK_LIB([m],[asinhf],[AC_DEFINE([HAVE_ASINHF],[1],[libm includes asinhf])])
AC_CHECK_LIB([m],[asinh],[AC_DEFINE([HAVE_ASINH],[1],[libm includes asinh])])
AC_CHECK_LIB([m],[asinhl],[AC_DEFINE([HAVE_ASINHL],[1],[libm includes asinhl])])
AC_CHECK_LIB([m],[atan2f],[AC_DEFINE([HAVE_ATAN2F],[1],[libm includes atan2f])])
AC_CHECK_LIB([m],[atan2],[AC_DEFINE([HAVE_ATAN2],[1],[libm includes atan2])])
AC_CHECK_LIB([m],[atan2l],[AC_DEFINE([HAVE_ATAN2L],[1],[libm includes atan2l])])
AC_CHECK_LIB([m],[atanf],[AC_DEFINE([HAVE_ATANF],[1],[libm includes atanf])])
AC_CHECK_LIB([m],[atan],[AC_DEFINE([HAVE_ATAN],[1],[libm includes atan])])
AC_CHECK_LIB([m],[atanl],[AC_DEFINE([HAVE_ATANL],[1],[libm includes atanl])])
AC_CHECK_LIB([m],[atanhf],[AC_DEFINE([HAVE_ATANHF],[1],[libm includes atanhf])])
AC_CHECK_LIB([m],[atanh],[AC_DEFINE([HAVE_ATANH],[1],[libm includes atanh])])
AC_CHECK_LIB([m],[atanhl],[AC_DEFINE([HAVE_ATANHL],[1],[libm includes atanhl])])
AC_CHECK_LIB([m],[cargf],[AC_DEFINE([HAVE_CARGF],[1],[libm includes cargf])])
AC_CHECK_LIB([m],[carg],[AC_DEFINE([HAVE_CARG],[1],[libm includes carg])])
AC_CHECK_LIB([m],[cargl],[AC_DEFINE([HAVE_CARGL],[1],[libm includes cargl])])
AC_CHECK_LIB([m],[ceilf],[AC_DEFINE([HAVE_CEILF],[1],[libm includes ceilf])])
AC_CHECK_LIB([m],[ceil],[AC_DEFINE([HAVE_CEIL],[1],[libm includes ceil])])
AC_CHECK_LIB([m],[ceill],[AC_DEFINE([HAVE_CEILL],[1],[libm includes ceill])])
AC_CHECK_LIB([m],[copysignf],[AC_DEFINE([HAVE_COPYSIGNF],[1],[libm includes copysignf])])
AC_CHECK_LIB([m],[copysign],[AC_DEFINE([HAVE_COPYSIGN],[1],[libm includes copysign])])
AC_CHECK_LIB([m],[copysignl],[AC_DEFINE([HAVE_COPYSIGNL],[1],[libm includes copysignl])])
AC_CHECK_LIB([m],[cosf],[AC_DEFINE([HAVE_COSF],[1],[libm includes cosf])])
AC_CHECK_LIB([m],[cos],[AC_DEFINE([HAVE_COS],[1],[libm includes cos])])
AC_CHECK_LIB([m],[cosl],[AC_DEFINE([HAVE_COSL],[1],[libm includes cosl])])
AC_CHECK_LIB([m],[ccosf],[AC_DEFINE([HAVE_CCOSF],[1],[libm includes ccosf])])
AC_CHECK_LIB([m],[ccos],[AC_DEFINE([HAVE_CCOS],[1],[libm includes ccos])])
AC_CHECK_LIB([m],[ccosl],[AC_DEFINE([HAVE_CCOSL],[1],[libm includes ccosl])])
AC_CHECK_LIB([m],[coshf],[AC_DEFINE([HAVE_COSHF],[1],[libm includes coshf])])
AC_CHECK_LIB([m],[cosh],[AC_DEFINE([HAVE_COSH],[1],[libm includes cosh])])
AC_CHECK_LIB([m],[coshl],[AC_DEFINE([HAVE_COSHL],[1],[libm includes coshl])])
AC_CHECK_LIB([m],[ccoshf],[AC_DEFINE([HAVE_CCOSHF],[1],[libm includes ccoshf])])
AC_CHECK_LIB([m],[ccosh],[AC_DEFINE([HAVE_CCOSH],[1],[libm includes ccosh])])
AC_CHECK_LIB([m],[ccoshl],[AC_DEFINE([HAVE_CCOSHL],[1],[libm includes ccoshl])])
AC_CHECK_LIB([m],[expf],[AC_DEFINE([HAVE_EXPF],[1],[libm includes expf])])
AC_CHECK_LIB([m],[exp],[AC_DEFINE([HAVE_EXP],[1],[libm includes exp])])
AC_CHECK_LIB([m],[expl],[AC_DEFINE([HAVE_EXPL],[1],[libm includes expl])])
AC_CHECK_LIB([m],[cexpf],[AC_DEFINE([HAVE_CEXPF],[1],[libm includes cexpf])])
AC_CHECK_LIB([m],[cexp],[AC_DEFINE([HAVE_CEXP],[1],[libm includes cexp])])
AC_CHECK_LIB([m],[cexpl],[AC_DEFINE([HAVE_CEXPL],[1],[libm includes cexpl])])
AC_CHECK_LIB([m],[fabsf],[AC_DEFINE([HAVE_FABSF],[1],[libm includes fabsf])])
AC_CHECK_LIB([m],[fabs],[AC_DEFINE([HAVE_FABS],[1],[libm includes fabs])])
AC_CHECK_LIB([m],[fabsl],[AC_DEFINE([HAVE_FABSL],[1],[libm includes fabsl])])
AC_CHECK_LIB([m],[cabsf],[AC_DEFINE([HAVE_CABSF],[1],[libm includes cabsf])])
AC_CHECK_LIB([m],[cabs],[AC_DEFINE([HAVE_CABS],[1],[libm includes cabs])])
AC_CHECK_LIB([m],[cabsl],[AC_DEFINE([HAVE_CABSL],[1],[libm includes cabsl])])
AC_CHECK_LIB([m],[floorf],[AC_DEFINE([HAVE_FLOORF],[1],[libm includes floorf])])
AC_CHECK_LIB([m],[floor],[AC_DEFINE([HAVE_FLOOR],[1],[libm includes floor])])
AC_CHECK_LIB([m],[floorl],[AC_DEFINE([HAVE_FLOORL],[1],[libm includes floorl])])
AC_CHECK_LIB([m],[frexpf],[AC_DEFINE([HAVE_FREXPF],[1],[libm includes frexpf])])
AC_CHECK_LIB([m],[frexp],[AC_DEFINE([HAVE_FREXP],[1],[libm includes frexp])])
AC_CHECK_LIB([m],[frexpl],[AC_DEFINE([HAVE_FREXPL],[1],[libm includes frexpl])])
AC_CHECK_LIB([m],[hypotf],[AC_DEFINE([HAVE_HYPOTF],[1],[libm includes hypotf])])
AC_CHECK_LIB([m],[hypot],[AC_DEFINE([HAVE_HYPOT],[1],[libm includes hypot])])
AC_CHECK_LIB([m],[hypotl],[AC_DEFINE([HAVE_HYPOTL],[1],[libm includes hypotl])])
AC_CHECK_LIB([m],[logf],[AC_DEFINE([HAVE_LOGF],[1],[libm includes logf])])
AC_CHECK_LIB([m],[log],[AC_DEFINE([HAVE_LOG],[1],[libm includes log])])
AC_CHECK_LIB([m],[logl],[AC_DEFINE([HAVE_LOGL],[1],[libm includes logl])])
AC_CHECK_LIB([m],[clogf],[AC_DEFINE([HAVE_CLOGF],[1],[libm includes clogf])])
AC_CHECK_LIB([m],[clog],[AC_DEFINE([HAVE_CLOG],[1],[libm includes clog])])
AC_CHECK_LIB([m],[clogl],[AC_DEFINE([HAVE_CLOGL],[1],[libm includes clogl])])
AC_CHECK_LIB([m],[log10f],[AC_DEFINE([HAVE_LOG10F],[1],[libm includes log10f])])
AC_CHECK_LIB([m],[log10],[AC_DEFINE([HAVE_LOG10],[1],[libm includes log10])])
AC_CHECK_LIB([m],[log10l],[AC_DEFINE([HAVE_LOG10L],[1],[libm includes log10l])])
AC_CHECK_LIB([m],[clog10f],[AC_DEFINE([HAVE_CLOG10F],[1],[libm includes clog10f])])
AC_CHECK_LIB([m],[clog10],[AC_DEFINE([HAVE_CLOG10],[1],[libm includes clog10])])
AC_CHECK_LIB([m],[clog10l],[AC_DEFINE([HAVE_CLOG10L],[1],[libm includes clog10l])])
AC_CHECK_LIB([m],[nextafterf],[AC_DEFINE([HAVE_NEXTAFTERF],[1],[libm includes nextafterf])])
AC_CHECK_LIB([m],[nextafter],[AC_DEFINE([HAVE_NEXTAFTER],[1],[libm includes nextafter])])
AC_CHECK_LIB([m],[nextafterl],[AC_DEFINE([HAVE_NEXTAFTERL],[1],[libm includes nextafterl])])
AC_CHECK_LIB([m],[powf],[AC_DEFINE([HAVE_POWF],[1],[libm includes powf])])
AC_CHECK_LIB([m],[pow],[AC_DEFINE([HAVE_POW],[1],[libm includes pow])])
AC_CHECK_LIB([m],[powl],[AC_DEFINE([HAVE_POWL],[1],[libm includes powl])])
AC_CHECK_LIB([m],[cpowf],[AC_DEFINE([HAVE_CPOWF],[1],[libm includes cpowf])])
AC_CHECK_LIB([m],[cpow],[AC_DEFINE([HAVE_CPOW],[1],[libm includes cpow])])
AC_CHECK_LIB([m],[cpowl],[AC_DEFINE([HAVE_CPOWL],[1],[libm includes cpowl])])
AC_CHECK_LIB([m],[roundf],[AC_DEFINE([HAVE_ROUNDF],[1],[libm includes roundf])])
AC_CHECK_LIB([m],[round],[AC_DEFINE([HAVE_ROUND],[1],[libm includes round])])
AC_CHECK_LIB([m],[roundl],[AC_DEFINE([HAVE_ROUNDL],[1],[libm includes roundl])])
AC_CHECK_LIB([m],[scalbnf],[AC_DEFINE([HAVE_SCALBNF],[1],[libm includes scalbnf])])
AC_CHECK_LIB([m],[scalbn],[AC_DEFINE([HAVE_SCALBN],[1],[libm includes scalbn])])
AC_CHECK_LIB([m],[scalbnl],[AC_DEFINE([HAVE_SCALBNL],[1],[libm includes scalbnl])])
AC_CHECK_LIB([m],[sinf],[AC_DEFINE([HAVE_SINF],[1],[libm includes sinf])])
AC_CHECK_LIB([m],[sin],[AC_DEFINE([HAVE_SIN],[1],[libm includes sin])])
AC_CHECK_LIB([m],[sinl],[AC_DEFINE([HAVE_SINL],[1],[libm includes sinl])])
AC_CHECK_LIB([m],[csinf],[AC_DEFINE([HAVE_CSINF],[1],[libm includes csinf])])
AC_CHECK_LIB([m],[csin],[AC_DEFINE([HAVE_CSIN],[1],[libm includes csin])])
AC_CHECK_LIB([m],[csinl],[AC_DEFINE([HAVE_CSINL],[1],[libm includes csinl])])
AC_CHECK_LIB([m],[sinhf],[AC_DEFINE([HAVE_SINHF],[1],[libm includes sinhf])])
AC_CHECK_LIB([m],[sinh],[AC_DEFINE([HAVE_SINH],[1],[libm includes sinh])])
AC_CHECK_LIB([m],[sinhl],[AC_DEFINE([HAVE_SINHL],[1],[libm includes sinhl])])
AC_CHECK_LIB([m],[csinhf],[AC_DEFINE([HAVE_CSINHF],[1],[libm includes csinhf])])
AC_CHECK_LIB([m],[csinh],[AC_DEFINE([HAVE_CSINH],[1],[libm includes csinh])])
AC_CHECK_LIB([m],[csinhl],[AC_DEFINE([HAVE_CSINHL],[1],[libm includes csinhl])])
AC_CHECK_LIB([m],[sqrtf],[AC_DEFINE([HAVE_SQRTF],[1],[libm includes sqrtf])])
AC_CHECK_LIB([m],[sqrt],[AC_DEFINE([HAVE_SQRT],[1],[libm includes sqrt])])
AC_CHECK_LIB([m],[sqrtl],[AC_DEFINE([HAVE_SQRTL],[1],[libm includes sqrtl])])
AC_CHECK_LIB([m],[csqrtf],[AC_DEFINE([HAVE_CSQRTF],[1],[libm includes csqrtf])])
AC_CHECK_LIB([m],[csqrt],[AC_DEFINE([HAVE_CSQRT],[1],[libm includes csqrt])])
AC_CHECK_LIB([m],[csqrtl],[AC_DEFINE([HAVE_CSQRTL],[1],[libm includes csqrtl])])
AC_CHECK_LIB([m],[tanf],[AC_DEFINE([HAVE_TANF],[1],[libm includes tanf])])
AC_CHECK_LIB([m],[tan],[AC_DEFINE([HAVE_TAN],[1],[libm includes tan])])
AC_CHECK_LIB([m],[tanl],[AC_DEFINE([HAVE_TANL],[1],[libm includes tanl])])
AC_CHECK_LIB([m],[ctanf],[AC_DEFINE([HAVE_CTANF],[1],[libm includes ctanf])])
AC_CHECK_LIB([m],[ctan],[AC_DEFINE([HAVE_CTAN],[1],[libm includes ctan])])
AC_CHECK_LIB([m],[ctanl],[AC_DEFINE([HAVE_CTANL],[1],[libm includes ctanl])])
AC_CHECK_LIB([m],[tanhf],[AC_DEFINE([HAVE_TANHF],[1],[libm includes tanhf])])
AC_CHECK_LIB([m],[tanh],[AC_DEFINE([HAVE_TANH],[1],[libm includes tanh])])
AC_CHECK_LIB([m],[tanhl],[AC_DEFINE([HAVE_TANHL],[1],[libm includes tanhl])])
AC_CHECK_LIB([m],[ctanhf],[AC_DEFINE([HAVE_CTANHF],[1],[libm includes ctanhf])])
AC_CHECK_LIB([m],[ctanh],[AC_DEFINE([HAVE_CTANH],[1],[libm includes ctanh])])
AC_CHECK_LIB([m],[ctanhl],[AC_DEFINE([HAVE_CTANHL],[1],[libm includes ctanhl])])
AC_CHECK_LIB([m],[truncf],[AC_DEFINE([HAVE_TRUNCF],[1],[libm includes truncf])])
AC_CHECK_LIB([m],[trunc],[AC_DEFINE([HAVE_TRUNC],[1],[libm includes trunc])])
AC_CHECK_LIB([m],[truncl],[AC_DEFINE([HAVE_TRUNCL],[1],[libm includes truncl])])
AC_CHECK_LIB([m],[erff],[AC_DEFINE([HAVE_ERFF],[1],[libm includes erff])])
AC_CHECK_LIB([m],[erf],[AC_DEFINE([HAVE_ERF],[1],[libm includes erf])])
AC_CHECK_LIB([m],[erfl],[AC_DEFINE([HAVE_ERFL],[1],[libm includes erfl])])
AC_CHECK_LIB([m],[erfcf],[AC_DEFINE([HAVE_ERFCF],[1],[libm includes erfcf])])
AC_CHECK_LIB([m],[erfc],[AC_DEFINE([HAVE_ERFC],[1],[libm includes erfc])])
AC_CHECK_LIB([m],[erfcl],[AC_DEFINE([HAVE_ERFCL],[1],[libm includes erfcl])])
AC_CHECK_LIB([m],[j0f],[AC_DEFINE([HAVE_J0F],[1],[libm includes j0f])])
AC_CHECK_LIB([m],[j0],[AC_DEFINE([HAVE_J0],[1],[libm includes j0])])
AC_CHECK_LIB([m],[j0l],[AC_DEFINE([HAVE_J0L],[1],[libm includes j0l])])
AC_CHECK_LIB([m],[j1f],[AC_DEFINE([HAVE_J1F],[1],[libm includes j1f])])
AC_CHECK_LIB([m],[j1],[AC_DEFINE([HAVE_J1],[1],[libm includes j1])])
AC_CHECK_LIB([m],[j1l],[AC_DEFINE([HAVE_J1L],[1],[libm includes j1l])])
AC_CHECK_LIB([m],[jnf],[AC_DEFINE([HAVE_JNF],[1],[libm includes jnf])])
AC_CHECK_LIB([m],[jn],[AC_DEFINE([HAVE_JN],[1],[libm includes jn])])
AC_CHECK_LIB([m],[jnl],[AC_DEFINE([HAVE_JNL],[1],[libm includes jnl])])
AC_CHECK_LIB([m],[y0f],[AC_DEFINE([HAVE_Y0F],[1],[libm includes y0f])])
AC_CHECK_LIB([m],[y0],[AC_DEFINE([HAVE_Y0],[1],[libm includes y0])])
AC_CHECK_LIB([m],[y0l],[AC_DEFINE([HAVE_Y0L],[1],[libm includes y0l])])
AC_CHECK_LIB([m],[y1f],[AC_DEFINE([HAVE_Y1F],[1],[libm includes y1f])])
AC_CHECK_LIB([m],[y1],[AC_DEFINE([HAVE_Y1],[1],[libm includes y1])])
AC_CHECK_LIB([m],[y1l],[AC_DEFINE([HAVE_Y1L],[1],[libm includes y1l])])
AC_CHECK_LIB([m],[ynf],[AC_DEFINE([HAVE_YNF],[1],[libm includes ynf])])
AC_CHECK_LIB([m],[yn],[AC_DEFINE([HAVE_YN],[1],[libm includes yn])])
AC_CHECK_LIB([m],[ynl],[AC_DEFINE([HAVE_YNL],[1],[libm includes ynl])])

# On AIX, clog is present in libm as __clog
AC_CHECK_LIB([m],[__clog],[AC_DEFINE([HAVE_CLOG],[1],[libm includes clog])])

# Check for a isfinite macro that works on long doubles.
LIBGFOR_CHECK_FOR_BROKEN_ISFINITE

# Check for a isnan macro that works on long doubles.
LIBGFOR_CHECK_FOR_BROKEN_ISNAN

# Check for a fpclassify macro that works on long doubles.
LIBGFOR_CHECK_FOR_BROKEN_FPCLASSIFY

# Check whether the system has a working stat()
LIBGFOR_CHECK_WORKING_STAT

# Fallback in case isfinite is not available.
AC_CHECK_LIB([m],[finite],[AC_DEFINE([HAVE_FINITE],[1],[libm includes finite])])

# Check for GNU libc feenableexcept
AC_CHECK_LIB([m],[feenableexcept],[have_feenableexcept=yes AC_DEFINE([HAVE_FEENABLEEXCEPT],[1],[libm includes feenableexcept])])

# Check for SysV fpsetmask
LIBGFOR_CHECK_FPSETMASK

# Check for AIX fp_trap and fp_enable
AC_CHECK_FUNC([fp_trap],[have_fp_trap=yes AC_DEFINE([HAVE_FP_TRAP],[1],[fp_trap is present])])
AC_CHECK_FUNC([fp_enable],[have_fp_enable=yes AC_DEFINE([HAVE_FP_ENABLE],[1],[fp_enable is present])])

# Runs configure.host to set up necessary host-dependent shell variables.
# We then display a message about it, and propagate them through the
# build chain.
. ${srcdir}/configure.host
AC_MSG_NOTICE([FPU dependent file will be ${fpu_host}.h])
FPU_HOST_HEADER=config/${fpu_host}.h
AC_SUBST(FPU_HOST_HEADER)

# The standard autoconf HAVE_STRUCT_TIMEZONE doesn't actually check
# for struct timezone, as you might think.  We also need to check how
# to call gettimeofday if we have it.
LIBGFOR_GETTIMEOFDAY

# Attempt to assert that the target is of common type in case we don't
# have C99 integer types at all.
LIBGFOR_TARGET_ILP32

# Check out attribute support.
LIBGFOR_CHECK_ATTRIBUTE_VISIBILITY
LIBGFOR_CHECK_ATTRIBUTE_DLLEXPORT
LIBGFOR_CHECK_ATTRIBUTE_ALIAS

# Check out sync builtins support.
LIBGFOR_CHECK_SYNC_FETCH_AND_ADD

# Check out thread support.
LIBGFOR_CHECK_GTHR_DEFAULT

# Check out #pragma weak.
LIBGFOR_GTHREAD_WEAK

# Various other checks on target
LIBGFOR_CHECK_UNLINK_OPEN_FILE

# Check whether line terminator is LF or CRLF
LIBGFOR_CHECK_CRLF

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Write our Makefile.
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
