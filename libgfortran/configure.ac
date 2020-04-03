# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_INIT([GNU Fortran Runtime Library], 0.3,,[libgfortran])
AC_CONFIG_HEADER(config.h)
GCC_TOPLEV_SUBDIRS

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
AS_HELP_STRING([--enable-version-specific-runtime-libs],
  [specify that runtime libraries should be installed in a compiler-specific directory]),
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

# Build with intermodule optimisations
AC_MSG_CHECKING([for --enable-intermodule])
AC_ARG_ENABLE(intermodule,
AS_HELP_STRING([--enable-intermodule],[build the library in one step]),
[case "$enable_intermodule" in
  yes) onestep="-onestep";;
    *) onestep="";;
esac],
[onestep=""])
AC_MSG_RESULT($enable_intermodule)
AM_CONDITIONAL(onestep,[test x$onestep = x-onestep])
AC_SUBST(onestep)

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
ACX_NONCANONICAL_TARGET

target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

# Sets up automake.  Must come after AC_CANONICAL_SYSTEM.  Each of the
# following is magically included in AUTOMAKE_OPTIONS in each Makefile.am.
#  1.9.6:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  no-dist:  we don't want 'dist' and related rules.
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is required.
AM_INIT_AUTOMAKE([1.9.6 no-define foreign no-dist -Wall -Wno-portability])

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

AC_USE_SYSTEM_EXTENSIONS

GCC_WITH_TOOLEXECLIBDIR

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
      case ${with_toolexeclibdir} in
	no)
	  toolexeclibdir='$(toolexecdir)/lib'
	  ;;
	*)
	  toolexeclibdir=${with_toolexeclibdir}
	  ;;
      esac
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

# Create a spec file, so that compile/link tests don't fail
test -f libgfortran.spec || touch libgfortran.spec

AC_LANG_C
# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AM_PROG_CC_C_O

# Add -Wall -fno-repack-arrays -fno-underscoring if we are using GCC.
if test "x$GCC" = "xyes"; then
  AM_FCFLAGS="-I . -Wall -Werror -fimplicit-none -fno-repack-arrays -fno-underscoring"
  ## We like to use C11 and C99 routines when available.  This makes
  ## sure that
  ## __STDC_VERSION__ is set such that libc includes make them available.
  AM_CFLAGS="-std=gnu11 -Wall -Wstrict-prototypes -Wmissing-prototypes -Wold-style-definition -Wextra -Wwrite-strings -Werror=implicit-function-declaration -Werror=vla"
  ## Compile the following tests with the same system header contents
  ## that we'll encounter when compiling our own source files.
  CFLAGS="-std=gnu11 $CFLAGS"
fi

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
AM_FCFLAGS="$AM_FCFLAGS $CET_FLAGS"
AM_CFLAGS="$AM_CFLAGS $CET_FLAGS"
CFLAGS="$CFLAGS $CET_FLAGS"

AC_SUBST(AM_FCFLAGS)
AC_SUBST(AM_CFLAGS)
AC_SUBST(CFLAGS)

# Check for symbol versioning (copied from libssp).
AC_MSG_CHECKING([whether symbol versioning is supported])
AC_ARG_ENABLE(symvers,
AS_HELP_STRING([--disable-symvers],
  [disable symbol versioning for libgfortran]),
gfortran_use_symver=$enableval,
gfortran_use_symver=yes)
if test "x$gfortran_use_symver" != xno; then
  save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS -fPIC -shared -Wl,--version-script,./conftest.map"
  cat > conftest.map <<EOF
FOO_1.0 {
  global: *foo*; bar; local: *;
};
EOF
  AC_LINK_IFELSE([AC_LANG_PROGRAM([[int foo;]],[[]])],[gfortran_use_symver=gnu],[gfortran_use_symver=no])
  if test x$gfortran_use_symver = xno; then
    case "$target_os" in
      solaris2*)
        LDFLAGS="$save_LDFLAGS"
        LDFLAGS="$LDFLAGS -fPIC -shared -Wl,-M,./conftest.map"
        # Sun ld cannot handle wildcards and treats all entries as undefined.
        cat > conftest.map <<EOF
FOO_1.0 {
  global: foo; local: *;
};
EOF
        AC_LINK_IFELSE([AC_LANG_PROGRAM([[int foo;]],[[]])],[gfortran_use_symver=sun],[gfortran_use_symver=no])
        ;;
    esac
  fi
  LDFLAGS="$save_LDFLAGS"
fi
AC_MSG_RESULT($gfortran_use_symver)
AM_CONDITIONAL(LIBGFOR_USE_SYMVER, [test "x$gfortran_use_symver" != xno])
AM_CONDITIONAL(LIBGFOR_USE_SYMVER_GNU, [test "x$gfortran_use_symver" = xgnu])
AM_CONDITIONAL(LIBGFOR_USE_SYMVER_SUN, [test "x$gfortran_use_symver" = xsun])

# For GPU offloading, not everything in libfortran can be supported.
# Currently, the only target that has this problem is nvptx.  The
# following is a (partial) list of features that are unsupportable on
# this particular target:
# * Constructors
# * alloca
# * C library support for I/O, with printf as the one notable exception
# * C library support for other features such as signal, environment
#   variables, time functions

AM_CONDITIONAL(LIBGFOR_MINIMAL, [test "x${target_cpu}" = xnvptx])

# Figure out whether the compiler supports "-ffunction-sections -fdata-sections",
# similarly to how libstdc++ does it
ac_test_CFLAGS="${CFLAGS+set}"
ac_save_CFLAGS="$CFLAGS"

# Check for -ffunction-sections -fdata-sections
AC_MSG_CHECKING([for gcc that supports -ffunction-sections -fdata-sections])
CFLAGS='-Werror -ffunction-sections -fdata-sections'
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[int foo;]])], [ac_fdsections=yes], [ac_fdsections=no])
if test "$ac_test_CFLAGS" = set; then
  CFLAGS="$ac_save_CFLAGS"
else
  # this is the suspicious part
  CFLAGS=""
fi
if test x"$ac_fdsections" = x"yes"; then
  SECTION_FLAGS='-ffunction-sections -fdata-sections'
fi
AC_MSG_RESULT($ac_fdsections)
AC_SUBST(SECTION_FLAGS)

# Check linker hardware capability support.
GCC_CHECK_LINKER_HWCAP

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
ACX_LT_HOST_FLAGS
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

# We need a working compiler at that point, otherwise give a clear
# error message and bail out.
LIBGFOR_WORKING_GFORTRAN

AC_SYS_LARGEFILE

# Types
AC_TYPE_OFF_T
AC_TYPE_INTPTR_T
AC_TYPE_UINTPTR_T
AC_CHECK_TYPES([ptrdiff_t])

# check header files (we assume C89 is available, so don't check for that)
AC_CHECK_HEADERS_ONCE(unistd.h sys/random.h sys/time.h sys/times.h \
sys/resource.h sys/types.h sys/stat.h sys/uio.h sys/wait.h \
floatingpoint.h ieeefp.h fenv.h fptrap.h \
fpxcp.h pwd.h complex.h xlocale.h)

GCC_HEADER_STDINT(gstdint.h)

AC_CHECK_MEMBERS([struct stat.st_blksize, struct stat.st_blocks, struct stat.st_rdev])

case "${host}--x${with_newlib}" in
  mips*--xyes)
    hardwire_newlib=1;;
  nvptx*--xyes)
    hardwire_newlib=1;;
esac

# Check for library functions.
if test "${hardwire_newlib:-0}" -eq 1; then
   # We are being configured with a cross compiler.  AC_REPLACE_FUNCS
   # may not work correctly, because the compiler may not be able to
   # link executables.
   AC_DEFINE(HAVE_MKSTEMP, 1, [Define if you have mkstemp.])
   AC_DEFINE(HAVE_STRTOF, 1, [Define if you have strtof.])
   AC_DEFINE(HAVE_SNPRINTF, 1, [Define if you have snprintf.])
   AC_DEFINE(HAVE_VSNPRINTF, 1, [Define if you have vsnprintf.])
   AC_DEFINE(HAVE_LOCALTIME_R, 1, [Define if you have localtime_r.])
   AC_DEFINE(HAVE_GMTIME_R, 1, [Define if you have gmtime_r.])
   AC_DEFINE(HAVE_STRNLEN, 1, [Define if you have strnlen.])
   AC_DEFINE(HAVE_STRNDUP, 1, [Define if you have strndup.])

   # At some point, we should differentiate between architectures
   # like x86, which have long double versions, and alpha/powerpc/etc.,
   # which don't. For the time being, punt.
   if test x"long_double_math_on_this_cpu" = x"yes"; then
     AC_DEFINE(HAVE_STRTOLD, 1, [Define if you have strtold.])
   fi
else
   AC_CHECK_FUNCS_ONCE(getrusage times mkstemp strtof strtold snprintf \
   ftruncate chsize chdir getentropy getlogin gethostname kill link symlink \
   sleep ttyname sigaction waitpid \
   alarm access fork posix_spawn setmode fcntl writev \
   gettimeofday stat fstat lstat getpwuid vsnprintf dup \
   getcwd localtime_r gmtime_r getpwuid_r ttyname_r clock_gettime \
   getgid getpid getuid geteuid umask getegid \
   secure_getenv __secure_getenv mkostemp strnlen strndup newlocale \
   freelocale uselocale strerror_l)
fi

# Check strerror_r, cannot be above as versions with two and three arguments exist
LIBGFOR_CHECK_STRERROR_R

# Check for C99 (and other IEEE) math functions
GCC_CHECK_MATH_FUNC([acosf])
GCC_CHECK_MATH_FUNC([acos])
GCC_CHECK_MATH_FUNC([acosl])
GCC_CHECK_MATH_FUNC([acoshf])
GCC_CHECK_MATH_FUNC([acosh])
GCC_CHECK_MATH_FUNC([acoshl])
GCC_CHECK_MATH_FUNC([asinf])
GCC_CHECK_MATH_FUNC([asin])
GCC_CHECK_MATH_FUNC([asinl])
GCC_CHECK_MATH_FUNC([asinhf])
GCC_CHECK_MATH_FUNC([asinh])
GCC_CHECK_MATH_FUNC([asinhl])
GCC_CHECK_MATH_FUNC([atan2f])
GCC_CHECK_MATH_FUNC([atan2])
GCC_CHECK_MATH_FUNC([atan2l])
GCC_CHECK_MATH_FUNC([atanf])
GCC_CHECK_MATH_FUNC([atan])
GCC_CHECK_MATH_FUNC([atanl])
GCC_CHECK_MATH_FUNC([atanhf])
GCC_CHECK_MATH_FUNC([atanh])
GCC_CHECK_MATH_FUNC([atanhl])
GCC_CHECK_MATH_FUNC([cargf])
GCC_CHECK_MATH_FUNC([carg])
GCC_CHECK_MATH_FUNC([cargl])
GCC_CHECK_MATH_FUNC([ceilf])
GCC_CHECK_MATH_FUNC([ceil])
GCC_CHECK_MATH_FUNC([ceill])
GCC_CHECK_MATH_FUNC([copysignf])
GCC_CHECK_MATH_FUNC([copysign])
GCC_CHECK_MATH_FUNC([copysignl])
GCC_CHECK_MATH_FUNC([cosf])
GCC_CHECK_MATH_FUNC([cos])
GCC_CHECK_MATH_FUNC([cosl])
GCC_CHECK_MATH_FUNC([ccosf])
GCC_CHECK_MATH_FUNC([ccos])
GCC_CHECK_MATH_FUNC([ccosl])
GCC_CHECK_MATH_FUNC([coshf])
GCC_CHECK_MATH_FUNC([cosh])
GCC_CHECK_MATH_FUNC([coshl])
GCC_CHECK_MATH_FUNC([ccoshf])
GCC_CHECK_MATH_FUNC([ccosh])
GCC_CHECK_MATH_FUNC([ccoshl])
GCC_CHECK_MATH_FUNC([expf])
GCC_CHECK_MATH_FUNC([exp])
GCC_CHECK_MATH_FUNC([expl])
GCC_CHECK_MATH_FUNC([cexpf])
GCC_CHECK_MATH_FUNC([cexp])
GCC_CHECK_MATH_FUNC([cexpl])
GCC_CHECK_MATH_FUNC([fabsf])
GCC_CHECK_MATH_FUNC([fabs])
GCC_CHECK_MATH_FUNC([fabsl])
GCC_CHECK_MATH_FUNC([cabsf])
GCC_CHECK_MATH_FUNC([cabs])
GCC_CHECK_MATH_FUNC([cabsl])
GCC_CHECK_MATH_FUNC([floorf])
GCC_CHECK_MATH_FUNC([floor])
GCC_CHECK_MATH_FUNC([floorl])
GCC_CHECK_MATH_FUNC([fmodf])
GCC_CHECK_MATH_FUNC([fmod])
GCC_CHECK_MATH_FUNC([fmodl])
GCC_CHECK_MATH_FUNC([frexpf])
GCC_CHECK_MATH_FUNC([frexp])
GCC_CHECK_MATH_FUNC([frexpl])
GCC_CHECK_MATH_FUNC([hypotf])
GCC_CHECK_MATH_FUNC([hypot])
GCC_CHECK_MATH_FUNC([hypotl])
GCC_CHECK_MATH_FUNC([ldexpf])
GCC_CHECK_MATH_FUNC([ldexp])
GCC_CHECK_MATH_FUNC([ldexpl])
GCC_CHECK_MATH_FUNC([logf])
GCC_CHECK_MATH_FUNC([log])
GCC_CHECK_MATH_FUNC([logl])
GCC_CHECK_MATH_FUNC([clogf])
GCC_CHECK_MATH_FUNC([clog])
GCC_CHECK_MATH_FUNC([clogl])
GCC_CHECK_MATH_FUNC([log10f])
GCC_CHECK_MATH_FUNC([log10])
GCC_CHECK_MATH_FUNC([log10l])
GCC_CHECK_MATH_FUNC([clog10f])
GCC_CHECK_MATH_FUNC([clog10])
GCC_CHECK_MATH_FUNC([clog10l])
GCC_CHECK_MATH_FUNC([nextafterf])
GCC_CHECK_MATH_FUNC([nextafter])
GCC_CHECK_MATH_FUNC([nextafterl])
GCC_CHECK_MATH_FUNC([powf])
GCC_CHECK_MATH_FUNC([pow])
GCC_CHECK_MATH_FUNC([cpowf])
GCC_CHECK_MATH_FUNC([cpow])
GCC_CHECK_MATH_FUNC([cpowl])
GCC_CHECK_MATH_FUNC([roundf])
GCC_CHECK_MATH_FUNC([round])
GCC_CHECK_MATH_FUNC([roundl])
GCC_CHECK_MATH_FUNC([lroundf])
GCC_CHECK_MATH_FUNC([lround])
GCC_CHECK_MATH_FUNC([lroundl])
GCC_CHECK_MATH_FUNC([llroundf])
GCC_CHECK_MATH_FUNC([llround])
GCC_CHECK_MATH_FUNC([llroundl])
GCC_CHECK_MATH_FUNC([scalbnf])
GCC_CHECK_MATH_FUNC([scalbn])
GCC_CHECK_MATH_FUNC([scalbnl])
GCC_CHECK_MATH_FUNC([sinf])
GCC_CHECK_MATH_FUNC([sin])
GCC_CHECK_MATH_FUNC([sinl])
GCC_CHECK_MATH_FUNC([csinf])
GCC_CHECK_MATH_FUNC([csin])
GCC_CHECK_MATH_FUNC([csinl])
GCC_CHECK_MATH_FUNC([sinhf])
GCC_CHECK_MATH_FUNC([sinh])
GCC_CHECK_MATH_FUNC([sinhl])
GCC_CHECK_MATH_FUNC([csinhf])
GCC_CHECK_MATH_FUNC([csinh])
GCC_CHECK_MATH_FUNC([csinhl])
GCC_CHECK_MATH_FUNC([sqrtf])
GCC_CHECK_MATH_FUNC([sqrt])
GCC_CHECK_MATH_FUNC([sqrtl])
GCC_CHECK_MATH_FUNC([csqrtf])
GCC_CHECK_MATH_FUNC([csqrt])
GCC_CHECK_MATH_FUNC([csqrtl])
GCC_CHECK_MATH_FUNC([tanf])
GCC_CHECK_MATH_FUNC([tan])
GCC_CHECK_MATH_FUNC([tanl])
GCC_CHECK_MATH_FUNC([ctanf])
GCC_CHECK_MATH_FUNC([ctan])
GCC_CHECK_MATH_FUNC([ctanl])
GCC_CHECK_MATH_FUNC([tanhf])
GCC_CHECK_MATH_FUNC([tanh])
GCC_CHECK_MATH_FUNC([tanhl])
GCC_CHECK_MATH_FUNC([ctanhf])
GCC_CHECK_MATH_FUNC([ctanh])
GCC_CHECK_MATH_FUNC([ctanhl])
GCC_CHECK_MATH_FUNC([truncf])
GCC_CHECK_MATH_FUNC([trunc])
GCC_CHECK_MATH_FUNC([truncl])
GCC_CHECK_MATH_FUNC([erff])
GCC_CHECK_MATH_FUNC([erf])
GCC_CHECK_MATH_FUNC([erfcf])
GCC_CHECK_MATH_FUNC([erfc])
GCC_CHECK_MATH_FUNC([erfcl])
GCC_CHECK_MATH_FUNC([j0f])
GCC_CHECK_MATH_FUNC([j0])
GCC_CHECK_MATH_FUNC([j1f])
GCC_CHECK_MATH_FUNC([j1])
GCC_CHECK_MATH_FUNC([jnf])
GCC_CHECK_MATH_FUNC([jn])
GCC_CHECK_MATH_FUNC([jnl])
GCC_CHECK_MATH_FUNC([y0f])
GCC_CHECK_MATH_FUNC([y0])
GCC_CHECK_MATH_FUNC([y1f])
GCC_CHECK_MATH_FUNC([y1])
GCC_CHECK_MATH_FUNC([ynf])
GCC_CHECK_MATH_FUNC([yn])
GCC_CHECK_MATH_FUNC([ynl])
GCC_CHECK_MATH_FUNC([tgamma])
GCC_CHECK_MATH_FUNC([tgammaf])
GCC_CHECK_MATH_FUNC([lgamma])
GCC_CHECK_MATH_FUNC([lgammaf])

# Check for GFORTRAN_C99_1.1 funcs
GCC_CHECK_MATH_FUNC([cacos])
GCC_CHECK_MATH_FUNC([cacosf])
GCC_CHECK_MATH_FUNC([cacosh])
GCC_CHECK_MATH_FUNC([cacoshf])
GCC_CHECK_MATH_FUNC([cacoshl])
GCC_CHECK_MATH_FUNC([cacosl])
GCC_CHECK_MATH_FUNC([casin])
GCC_CHECK_MATH_FUNC([casinf])
GCC_CHECK_MATH_FUNC([casinh])
GCC_CHECK_MATH_FUNC([casinhf])
GCC_CHECK_MATH_FUNC([casinhl])
GCC_CHECK_MATH_FUNC([casinl])
GCC_CHECK_MATH_FUNC([catan])
GCC_CHECK_MATH_FUNC([catanf])
GCC_CHECK_MATH_FUNC([catanh])
GCC_CHECK_MATH_FUNC([catanhf])
GCC_CHECK_MATH_FUNC([catanhl])
GCC_CHECK_MATH_FUNC([catanl])

# On AIX, clog is present in libm as __clog
AC_CHECK_LIB([m],[__clog],[AC_DEFINE([HAVE_CLOG],[1],[libm includes clog])])

# Check whether the system has a working stat()
LIBGFOR_CHECK_WORKING_STAT

# Check whether __mingw_snprintf() is present
LIBGFOR_CHECK_MINGW_SNPRINTF

# Check whether libquadmath should be used
AC_ARG_ENABLE(libquadmath-support,
AS_HELP_STRING([--disable-libquadmath-support],
  [disable libquadmath support for Fortran]),
ENABLE_LIBQUADMATH_SUPPORT=$enableval,
ENABLE_LIBQUADMATH_SUPPORT=yes)
enable_libquadmath_support=
if test "${ENABLE_LIBQUADMATH_SUPPORT}" = "no" ; then
  enable_libquadmath_support=no
fi

# Check whether we have a __float128 type, depends on enable_libquadmath_support
LIBGFOR_CHECK_FLOAT128

# Check for GNU libc feenableexcept
AC_CHECK_LIB([m],[feenableexcept],[have_feenableexcept=yes AC_DEFINE([HAVE_FEENABLEEXCEPT],[1],[libm includes feenableexcept])])

# At least for glibc, clock_gettime is in librt.  But don't
# pull that in if it still doesn't give us the function we want.  This
# test is copied from libgomp, and modified to not link in -lrt as
# libgfortran calls clock_gettime via a weak reference if it's found
# in librt.
if test "$ac_cv_func_clock_gettime" = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [AC_DEFINE(HAVE_CLOCK_GETTIME_LIBRT, 1,
               [Define to 1 if you have the `clock_gettime' function in librt.])])
fi

# Check for SysV fpsetmask
LIBGFOR_CHECK_FPSETMASK
AC_CHECK_TYPES([fp_except,fp_except_t], [], [], [[
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#include <math.h>
]])
AC_CHECK_TYPES([fp_rnd,fp_rnd_t], [], [], [[
#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif
#include <math.h>
]])

# Check whether we have fpsetsticky or fpresetsticky
AC_CHECK_FUNC([fpsetsticky],[have_fpsetsticky=yes AC_DEFINE([HAVE_FPSETSTICKY],[1],[fpsetsticky is present])])
AC_CHECK_FUNC([fpresetsticky],[have_fpresetsticky=yes AC_DEFINE([HAVE_FPRESETSTICKY],[1],[fpresetsticky is present])])

# Check for AIX fp_trap and fp_enable
AC_CHECK_FUNC([fp_trap],[have_fp_trap=yes AC_DEFINE([HAVE_FP_TRAP],[1],[fp_trap is present])])
AC_CHECK_FUNC([fp_enable],[have_fp_enable=yes AC_DEFINE([HAVE_FP_ENABLE],[1],[fp_enable is present])])

# Check if _SOFT_FLOAT is defined
AC_CHECK_DEFINE([_SOFT_FLOAT],[have_soft_float=yes])

# Runs configure.host to set up necessary host-dependent shell variables.
# We then display a message about it, and propagate them through the
# build chain.
. ${srcdir}/configure.host
AC_MSG_NOTICE([FPU dependent file will be ${fpu_host}.h])
AC_MSG_NOTICE([Support for IEEE modules: ${ieee_support}])
FPU_HOST_HEADER=config/${fpu_host}.h
AC_SUBST(FPU_HOST_HEADER)

# Whether we will build the IEEE modules
AM_CONDITIONAL(IEEE_SUPPORT,[test x${ieee_support} = xyes])
AC_SUBST(IEEE_SUPPORT)

# Some targets require additional compiler options for IEEE compatibility.
IEEE_FLAGS="${ieee_flags}"
AC_SUBST(IEEE_FLAGS)


# Check for POSIX getpwuid_r 
#
# There are two versions of getpwuid_r, the POSIX one with 5
# arguments, and another one with 4 arguments used by at least HP-UX
# 10.2.
if test "$ac_cv_func_getpwuid_r" = "yes"; then
   AC_CACHE_CHECK([POSIX version of getpwuid_r with 5 arguments], libgfor_cv_posix_getpwuid_r, [
   AC_LINK_IFELSE([AC_LANG_PROGRAM([
#include <stdio.h>
#include <sys/types.h>
#include <pwd.h>], [
    getpwuid_r(0, NULL, NULL, 0, NULL);
    ])],  [libgfor_cv_posix_getpwuid_r="yes"], [libgfor_cv_posix_getpwuid_r="no"])])
fi
if test "$libgfor_cv_posix_getpwuid_r" = "yes"; then
    AC_DEFINE([HAVE_POSIX_GETPWUID_R], [1], [Define to 1 if we have POSIX getpwuid_r which takes 5 arguments.])
fi


# Check out attribute support.
LIBGFOR_CHECK_ATTRIBUTE_VISIBILITY
LIBGFOR_CHECK_ATTRIBUTE_ALIAS

# Check out atomic builtins support.
LIBGFOR_CHECK_ATOMIC_FETCH_ADD

# Check out #pragma weak.
LIBGFOR_GTHREAD_WEAK

# Check out weakref support.
LIBGFOR_CHECK_WEAKREF

# Various other checks on target
LIBGFOR_CHECK_UNLINK_OPEN_FILE

# Check whether line terminator is LF or CRLF
LIBGFOR_CHECK_CRLF

# Check whether we support AVX extensions
LIBGFOR_CHECK_AVX

# Check wether we support AVX2 extensions
LIBGFOR_CHECK_AVX2

# Check wether we support AVX512f extensions
LIBGFOR_CHECK_AVX512F

# Check for FMA3 extensions
LIBGFOR_CHECK_FMA3

# Check for FMA4 extensions
LIBGFOR_CHECK_FMA4

# Check if AVX128 works
LIBGFOR_CHECK_AVX128

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Write our Makefile and spec file.
AC_CONFIG_FILES([
Makefile
libgfortran.spec
])
AC_OUTPUT
