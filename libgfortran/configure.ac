# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_PREREQ(2.64)
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
  ## We like to use C99 routines when available.  This makes sure that
  ## __STDC_VERSION__ is set such that libc includes make them available.
  AM_CFLAGS="-std=gnu99 -Wall -Wstrict-prototypes -Wmissing-prototypes -Wold-style-definition -Wextra -Wwrite-strings"
  ## Compile the following tests with the same system header contents
  ## that we'll encounter when compiling our own source files.
  CFLAGS="-std=gnu99 $CFLAGS"
fi
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
if test "x$gfortran_use_symver" = xyes; then
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
AC_CHECK_HEADERS_ONCE(unistd.h sys/time.h sys/times.h sys/resource.h \
sys/types.h sys/stat.h sys/wait.h floatingpoint.h ieeefp.h fenv.h fptrap.h \
pwd.h complex.h) 

GCC_HEADER_STDINT(gstdint.h)

AC_CHECK_MEMBERS([struct stat.st_blksize, struct stat.st_blocks, struct stat.st_rdev])

# Check for library functions.
AC_CHECK_FUNCS_ONCE(getrusage times mkstemp strtof strtold snprintf \
ftruncate chsize chdir getlogin gethostname kill link symlink sleep ttyname \
alarm access fork execl wait setmode execve pipe dup2 close \
strcasestr getrlimit gettimeofday stat fstat lstat getpwuid vsnprintf dup \
getcwd localtime_r gmtime_r strerror_r getpwuid_r ttyname_r clock_gettime \
readlink getgid getpid getppid getuid geteuid umask)

# Check for C99 (and other IEEE) math functions
GCC_CHECK_MATH_FUNC([acosf],[AC_DEFINE([HAVE_ACOSF],[1],[acosf is available])])
GCC_CHECK_MATH_FUNC([acos],[AC_DEFINE([HAVE_ACOS],[1],[acos is available])])
GCC_CHECK_MATH_FUNC([acosl],[AC_DEFINE([HAVE_ACOSL],[1],[acosl is available])])
GCC_CHECK_MATH_FUNC([acoshf],[AC_DEFINE([HAVE_ACOSHF],[1],[acoshf is available])])
GCC_CHECK_MATH_FUNC([acosh],[AC_DEFINE([HAVE_ACOSH],[1],[acosh is available])])
GCC_CHECK_MATH_FUNC([acoshl],[AC_DEFINE([HAVE_ACOSHL],[1],[acoshl is available])])
GCC_CHECK_MATH_FUNC([asinf],[AC_DEFINE([HAVE_ASINF],[1],[asinf is available])])
GCC_CHECK_MATH_FUNC([asin],[AC_DEFINE([HAVE_ASIN],[1],[asin is available])])
GCC_CHECK_MATH_FUNC([asinl],[AC_DEFINE([HAVE_ASINL],[1],[asinl is available])])
GCC_CHECK_MATH_FUNC([asinhf],[AC_DEFINE([HAVE_ASINHF],[1],[asinhf is available])])
GCC_CHECK_MATH_FUNC([asinh],[AC_DEFINE([HAVE_ASINH],[1],[asinh is available])])
GCC_CHECK_MATH_FUNC([asinhl],[AC_DEFINE([HAVE_ASINHL],[1],[asinhl is available])])
GCC_CHECK_MATH_FUNC([atan2f],[AC_DEFINE([HAVE_ATAN2F],[1],[atan2f is available])])
GCC_CHECK_MATH_FUNC([atan2],[AC_DEFINE([HAVE_ATAN2],[1],[atan2 is available])])
GCC_CHECK_MATH_FUNC([atan2l],[AC_DEFINE([HAVE_ATAN2L],[1],[atan2l is available])])
GCC_CHECK_MATH_FUNC([atanf],[AC_DEFINE([HAVE_ATANF],[1],[atanf is available])])
GCC_CHECK_MATH_FUNC([atan],[AC_DEFINE([HAVE_ATAN],[1],[atan is available])])
GCC_CHECK_MATH_FUNC([atanl],[AC_DEFINE([HAVE_ATANL],[1],[atanl is available])])
GCC_CHECK_MATH_FUNC([atanhf],[AC_DEFINE([HAVE_ATANHF],[1],[atanhf is available])])
GCC_CHECK_MATH_FUNC([atanh],[AC_DEFINE([HAVE_ATANH],[1],[atanh is available])])
GCC_CHECK_MATH_FUNC([atanhl],[AC_DEFINE([HAVE_ATANHL],[1],[atanhl is available])])
GCC_CHECK_MATH_FUNC([cargf],[AC_DEFINE([HAVE_CARGF],[1],[cargf is available])])
GCC_CHECK_MATH_FUNC([carg],[AC_DEFINE([HAVE_CARG],[1],[carg is available])])
GCC_CHECK_MATH_FUNC([cargl],[AC_DEFINE([HAVE_CARGL],[1],[cargl is available])])
GCC_CHECK_MATH_FUNC([ceilf],[AC_DEFINE([HAVE_CEILF],[1],[ceilf is available])])
GCC_CHECK_MATH_FUNC([ceil],[AC_DEFINE([HAVE_CEIL],[1],[ceil is available])])
GCC_CHECK_MATH_FUNC([ceill],[AC_DEFINE([HAVE_CEILL],[1],[ceill is available])])
GCC_CHECK_MATH_FUNC([copysignf],[AC_DEFINE([HAVE_COPYSIGNF],[1],[copysignf is available])])
GCC_CHECK_MATH_FUNC([copysign],[AC_DEFINE([HAVE_COPYSIGN],[1],[copysign is available])])
GCC_CHECK_MATH_FUNC([copysignl],[AC_DEFINE([HAVE_COPYSIGNL],[1],[copysignl is available])])
GCC_CHECK_MATH_FUNC([cosf],[AC_DEFINE([HAVE_COSF],[1],[cosf is available])])
GCC_CHECK_MATH_FUNC([cos],[AC_DEFINE([HAVE_COS],[1],[cos is available])])
GCC_CHECK_MATH_FUNC([cosl],[AC_DEFINE([HAVE_COSL],[1],[cosl is available])])
GCC_CHECK_MATH_FUNC([ccosf],[AC_DEFINE([HAVE_CCOSF],[1],[ccosf is available])])
GCC_CHECK_MATH_FUNC([ccos],[AC_DEFINE([HAVE_CCOS],[1],[ccos is available])])
GCC_CHECK_MATH_FUNC([ccosl],[AC_DEFINE([HAVE_CCOSL],[1],[ccosl is available])])
GCC_CHECK_MATH_FUNC([coshf],[AC_DEFINE([HAVE_COSHF],[1],[coshf is available])])
GCC_CHECK_MATH_FUNC([cosh],[AC_DEFINE([HAVE_COSH],[1],[cosh is available])])
GCC_CHECK_MATH_FUNC([coshl],[AC_DEFINE([HAVE_COSHL],[1],[coshl is available])])
GCC_CHECK_MATH_FUNC([ccoshf],[AC_DEFINE([HAVE_CCOSHF],[1],[ccoshf is available])])
GCC_CHECK_MATH_FUNC([ccosh],[AC_DEFINE([HAVE_CCOSH],[1],[ccosh is available])])
GCC_CHECK_MATH_FUNC([ccoshl],[AC_DEFINE([HAVE_CCOSHL],[1],[ccoshl is available])])
GCC_CHECK_MATH_FUNC([expf],[AC_DEFINE([HAVE_EXPF],[1],[expf is available])])
GCC_CHECK_MATH_FUNC([exp],[AC_DEFINE([HAVE_EXP],[1],[exp is available])])
GCC_CHECK_MATH_FUNC([expl],[AC_DEFINE([HAVE_EXPL],[1],[expl is available])])
GCC_CHECK_MATH_FUNC([cexpf],[AC_DEFINE([HAVE_CEXPF],[1],[cexpf is available])])
GCC_CHECK_MATH_FUNC([cexp],[AC_DEFINE([HAVE_CEXP],[1],[cexp is available])])
GCC_CHECK_MATH_FUNC([cexpl],[AC_DEFINE([HAVE_CEXPL],[1],[cexpl is available])])
GCC_CHECK_MATH_FUNC([fabsf],[AC_DEFINE([HAVE_FABSF],[1],[fabsf is available])])
GCC_CHECK_MATH_FUNC([fabs],[AC_DEFINE([HAVE_FABS],[1],[fabs is available])])
GCC_CHECK_MATH_FUNC([fabsl],[AC_DEFINE([HAVE_FABSL],[1],[fabsl is available])])
GCC_CHECK_MATH_FUNC([cabsf],[AC_DEFINE([HAVE_CABSF],[1],[cabsf is available])])
GCC_CHECK_MATH_FUNC([cabs],[AC_DEFINE([HAVE_CABS],[1],[cabs is available])])
GCC_CHECK_MATH_FUNC([cabsl],[AC_DEFINE([HAVE_CABSL],[1],[cabsl is available])])
GCC_CHECK_MATH_FUNC([floorf],[AC_DEFINE([HAVE_FLOORF],[1],[floorf is available])])
GCC_CHECK_MATH_FUNC([floor],[AC_DEFINE([HAVE_FLOOR],[1],[floor is available])])
GCC_CHECK_MATH_FUNC([floorl],[AC_DEFINE([HAVE_FLOORL],[1],[floorl is available])])
GCC_CHECK_MATH_FUNC([fmodf],[AC_DEFINE([HAVE_FMODF],[1],[fmodf is available])])
GCC_CHECK_MATH_FUNC([fmod],[AC_DEFINE([HAVE_FMOD],[1],[fmod is available])])
GCC_CHECK_MATH_FUNC([fmodl],[AC_DEFINE([HAVE_FMODL],[1],[fmodl is available])])
GCC_CHECK_MATH_FUNC([frexpf],[AC_DEFINE([HAVE_FREXPF],[1],[frexpf is available])])
GCC_CHECK_MATH_FUNC([frexp],[AC_DEFINE([HAVE_FREXP],[1],[frexp is available])])
GCC_CHECK_MATH_FUNC([frexpl],[AC_DEFINE([HAVE_FREXPL],[1],[frexpl is available])])
GCC_CHECK_MATH_FUNC([hypotf],[AC_DEFINE([HAVE_HYPOTF],[1],[hypotf is available])])
GCC_CHECK_MATH_FUNC([hypot],[AC_DEFINE([HAVE_HYPOT],[1],[hypot is available])])
GCC_CHECK_MATH_FUNC([hypotl],[AC_DEFINE([HAVE_HYPOTL],[1],[hypotl is available])])
GCC_CHECK_MATH_FUNC([ldexpf],[AC_DEFINE([HAVE_LDEXPF],[1],[ldexpf is available])])
GCC_CHECK_MATH_FUNC([ldexp],[AC_DEFINE([HAVE_LDEXP],[1],[ldexp is available])])
GCC_CHECK_MATH_FUNC([ldexpl],[AC_DEFINE([HAVE_LDEXPL],[1],[ldexpl is available])])
GCC_CHECK_MATH_FUNC([logf],[AC_DEFINE([HAVE_LOGF],[1],[logf is available])])
GCC_CHECK_MATH_FUNC([log],[AC_DEFINE([HAVE_LOG],[1],[log is available])])
GCC_CHECK_MATH_FUNC([logl],[AC_DEFINE([HAVE_LOGL],[1],[logl is available])])
GCC_CHECK_MATH_FUNC([clogf],[AC_DEFINE([HAVE_CLOGF],[1],[clogf is available])])
GCC_CHECK_MATH_FUNC([clog],[AC_DEFINE([HAVE_CLOG],[1],[clog is available])])
GCC_CHECK_MATH_FUNC([clogl],[AC_DEFINE([HAVE_CLOGL],[1],[clogl is available])])
GCC_CHECK_MATH_FUNC([log10f],[AC_DEFINE([HAVE_LOG10F],[1],[log10f is available])])
GCC_CHECK_MATH_FUNC([log10],[AC_DEFINE([HAVE_LOG10],[1],[log10 is available])])
GCC_CHECK_MATH_FUNC([log10l],[AC_DEFINE([HAVE_LOG10L],[1],[log10l is available])])
GCC_CHECK_MATH_FUNC([clog10f],[AC_DEFINE([HAVE_CLOG10F],[1],[clog10f is available])])
GCC_CHECK_MATH_FUNC([clog10],[AC_DEFINE([HAVE_CLOG10],[1],[clog10 is available])])
GCC_CHECK_MATH_FUNC([clog10l],[AC_DEFINE([HAVE_CLOG10L],[1],[clog10l is available])])
GCC_CHECK_MATH_FUNC([nextafterf],[AC_DEFINE([HAVE_NEXTAFTERF],[1],[nextafterf is available])])
GCC_CHECK_MATH_FUNC([nextafter],[AC_DEFINE([HAVE_NEXTAFTER],[1],[nextafter is available])])
GCC_CHECK_MATH_FUNC([nextafterl],[AC_DEFINE([HAVE_NEXTAFTERL],[1],[nextafterl is available])])
GCC_CHECK_MATH_FUNC([powf],[AC_DEFINE([HAVE_POWF],[1],[powf is available])])
GCC_CHECK_MATH_FUNC([pow],[AC_DEFINE([HAVE_POW],[1],[pow is available])])
GCC_CHECK_MATH_FUNC([powl],[AC_DEFINE([HAVE_POWL],[1],[powl is available])])
GCC_CHECK_MATH_FUNC([cpowf],[AC_DEFINE([HAVE_CPOWF],[1],[cpowf is available])])
GCC_CHECK_MATH_FUNC([cpow],[AC_DEFINE([HAVE_CPOW],[1],[cpow is available])])
GCC_CHECK_MATH_FUNC([cpowl],[AC_DEFINE([HAVE_CPOWL],[1],[cpowl is available])])
GCC_CHECK_MATH_FUNC([roundf],[AC_DEFINE([HAVE_ROUNDF],[1],[roundf is available])])
GCC_CHECK_MATH_FUNC([round],[AC_DEFINE([HAVE_ROUND],[1],[round is available])])
GCC_CHECK_MATH_FUNC([roundl],[AC_DEFINE([HAVE_ROUNDL],[1],[roundl is available])])
GCC_CHECK_MATH_FUNC([lroundf],[AC_DEFINE([HAVE_LROUNDF],[1],[lroundf is available])])
GCC_CHECK_MATH_FUNC([lround],[AC_DEFINE([HAVE_LROUND],[1],[lround is available])])
GCC_CHECK_MATH_FUNC([lroundl],[AC_DEFINE([HAVE_LROUNDL],[1],[lroundl is available])])
GCC_CHECK_MATH_FUNC([llroundf],[AC_DEFINE([HAVE_LLROUNDF],[1],[llroundf is available])])
GCC_CHECK_MATH_FUNC([llround],[AC_DEFINE([HAVE_LLROUND],[1],[llround is available])])
GCC_CHECK_MATH_FUNC([llroundl],[AC_DEFINE([HAVE_LLROUNDL],[1],[llroundl is available])])
GCC_CHECK_MATH_FUNC([scalbnf],[AC_DEFINE([HAVE_SCALBNF],[1],[scalbnf is available])])
GCC_CHECK_MATH_FUNC([scalbn],[AC_DEFINE([HAVE_SCALBN],[1],[scalbn is available])])
GCC_CHECK_MATH_FUNC([scalbnl],[AC_DEFINE([HAVE_SCALBNL],[1],[scalbnl is available])])
GCC_CHECK_MATH_FUNC([sinf],[AC_DEFINE([HAVE_SINF],[1],[sinf is available])])
GCC_CHECK_MATH_FUNC([sin],[AC_DEFINE([HAVE_SIN],[1],[sin is available])])
GCC_CHECK_MATH_FUNC([sinl],[AC_DEFINE([HAVE_SINL],[1],[sinl is available])])
GCC_CHECK_MATH_FUNC([csinf],[AC_DEFINE([HAVE_CSINF],[1],[csinf is available])])
GCC_CHECK_MATH_FUNC([csin],[AC_DEFINE([HAVE_CSIN],[1],[csin is available])])
GCC_CHECK_MATH_FUNC([csinl],[AC_DEFINE([HAVE_CSINL],[1],[csinl is available])])
GCC_CHECK_MATH_FUNC([sinhf],[AC_DEFINE([HAVE_SINHF],[1],[sinhf is available])])
GCC_CHECK_MATH_FUNC([sinh],[AC_DEFINE([HAVE_SINH],[1],[sinh is available])])
GCC_CHECK_MATH_FUNC([sinhl],[AC_DEFINE([HAVE_SINHL],[1],[sinhl is available])])
GCC_CHECK_MATH_FUNC([csinhf],[AC_DEFINE([HAVE_CSINHF],[1],[csinhf is available])])
GCC_CHECK_MATH_FUNC([csinh],[AC_DEFINE([HAVE_CSINH],[1],[csinh is available])])
GCC_CHECK_MATH_FUNC([csinhl],[AC_DEFINE([HAVE_CSINHL],[1],[csinhl is available])])
GCC_CHECK_MATH_FUNC([sqrtf],[AC_DEFINE([HAVE_SQRTF],[1],[sqrtf is available])])
GCC_CHECK_MATH_FUNC([sqrt],[AC_DEFINE([HAVE_SQRT],[1],[sqrt is available])])
GCC_CHECK_MATH_FUNC([sqrtl],[AC_DEFINE([HAVE_SQRTL],[1],[sqrtl is available])])
GCC_CHECK_MATH_FUNC([csqrtf],[AC_DEFINE([HAVE_CSQRTF],[1],[csqrtf is available])])
GCC_CHECK_MATH_FUNC([csqrt],[AC_DEFINE([HAVE_CSQRT],[1],[csqrt is available])])
GCC_CHECK_MATH_FUNC([csqrtl],[AC_DEFINE([HAVE_CSQRTL],[1],[csqrtl is available])])
GCC_CHECK_MATH_FUNC([tanf],[AC_DEFINE([HAVE_TANF],[1],[tanf is available])])
GCC_CHECK_MATH_FUNC([tan],[AC_DEFINE([HAVE_TAN],[1],[tan is available])])
GCC_CHECK_MATH_FUNC([tanl],[AC_DEFINE([HAVE_TANL],[1],[tanl is available])])
GCC_CHECK_MATH_FUNC([ctanf],[AC_DEFINE([HAVE_CTANF],[1],[ctanf is available])])
GCC_CHECK_MATH_FUNC([ctan],[AC_DEFINE([HAVE_CTAN],[1],[ctan is available])])
GCC_CHECK_MATH_FUNC([ctanl],[AC_DEFINE([HAVE_CTANL],[1],[ctanl is available])])
GCC_CHECK_MATH_FUNC([tanhf],[AC_DEFINE([HAVE_TANHF],[1],[tanhf is available])])
GCC_CHECK_MATH_FUNC([tanh],[AC_DEFINE([HAVE_TANH],[1],[tanh is available])])
GCC_CHECK_MATH_FUNC([tanhl],[AC_DEFINE([HAVE_TANHL],[1],[tanhl is available])])
GCC_CHECK_MATH_FUNC([ctanhf],[AC_DEFINE([HAVE_CTANHF],[1],[ctanhf is available])])
GCC_CHECK_MATH_FUNC([ctanh],[AC_DEFINE([HAVE_CTANH],[1],[ctanh is available])])
GCC_CHECK_MATH_FUNC([ctanhl],[AC_DEFINE([HAVE_CTANHL],[1],[ctanhl is available])])
GCC_CHECK_MATH_FUNC([truncf],[AC_DEFINE([HAVE_TRUNCF],[1],[truncf is available])])
GCC_CHECK_MATH_FUNC([trunc],[AC_DEFINE([HAVE_TRUNC],[1],[trunc is available])])
GCC_CHECK_MATH_FUNC([truncl],[AC_DEFINE([HAVE_TRUNCL],[1],[truncl is available])])
GCC_CHECK_MATH_FUNC([erff],[AC_DEFINE([HAVE_ERFF],[1],[erff is available])])
GCC_CHECK_MATH_FUNC([erf],[AC_DEFINE([HAVE_ERF],[1],[erf is available])])
GCC_CHECK_MATH_FUNC([erfl],[AC_DEFINE([HAVE_ERFL],[1],[erfl is available])])
GCC_CHECK_MATH_FUNC([erfcf],[AC_DEFINE([HAVE_ERFCF],[1],[erfcf is available])])
GCC_CHECK_MATH_FUNC([erfc],[AC_DEFINE([HAVE_ERFC],[1],[erfc is available])])
GCC_CHECK_MATH_FUNC([erfcl],[AC_DEFINE([HAVE_ERFCL],[1],[erfcl is available])])
GCC_CHECK_MATH_FUNC([j0f],[AC_DEFINE([HAVE_J0F],[1],[j0f is available])])
GCC_CHECK_MATH_FUNC([j0],[AC_DEFINE([HAVE_J0],[1],[j0 is available])])
GCC_CHECK_MATH_FUNC([j0l],[AC_DEFINE([HAVE_J0L],[1],[j0l is available])])
GCC_CHECK_MATH_FUNC([j1f],[AC_DEFINE([HAVE_J1F],[1],[j1f is available])])
GCC_CHECK_MATH_FUNC([j1],[AC_DEFINE([HAVE_J1],[1],[j1 is available])])
GCC_CHECK_MATH_FUNC([j1l],[AC_DEFINE([HAVE_J1L],[1],[j1l is available])])
GCC_CHECK_MATH_FUNC([jnf],[AC_DEFINE([HAVE_JNF],[1],[jnf is available])])
GCC_CHECK_MATH_FUNC([jn],[AC_DEFINE([HAVE_JN],[1],[jn is available])])
GCC_CHECK_MATH_FUNC([jnl],[AC_DEFINE([HAVE_JNL],[1],[jnl is available])])
GCC_CHECK_MATH_FUNC([y0f],[AC_DEFINE([HAVE_Y0F],[1],[y0f is available])])
GCC_CHECK_MATH_FUNC([y0],[AC_DEFINE([HAVE_Y0],[1],[y0 is available])])
GCC_CHECK_MATH_FUNC([y0l],[AC_DEFINE([HAVE_Y0L],[1],[y0l is available])])
GCC_CHECK_MATH_FUNC([y1f],[AC_DEFINE([HAVE_Y1F],[1],[y1f is available])])
GCC_CHECK_MATH_FUNC([y1],[AC_DEFINE([HAVE_Y1],[1],[y1 is available])])
GCC_CHECK_MATH_FUNC([y1l],[AC_DEFINE([HAVE_Y1L],[1],[y1l is available])])
GCC_CHECK_MATH_FUNC([ynf],[AC_DEFINE([HAVE_YNF],[1],[ynf is available])])
GCC_CHECK_MATH_FUNC([yn],[AC_DEFINE([HAVE_YN],[1],[yn is available])])
GCC_CHECK_MATH_FUNC([ynl],[AC_DEFINE([HAVE_YNL],[1],[ynl is available])])
GCC_CHECK_MATH_FUNC([tgamma],[AC_DEFINE([HAVE_TGAMMA],[1],[tgamma is available])])
GCC_CHECK_MATH_FUNC([tgammaf],[AC_DEFINE([HAVE_TGAMMAF],[1],[tgammaf is available])])
GCC_CHECK_MATH_FUNC([tgammal],[AC_DEFINE([HAVE_TGAMMAL],[1],[tgammal is available])])
GCC_CHECK_MATH_FUNC([lgamma],[AC_DEFINE([HAVE_LGAMMA],[1],[lgamma is available])])
GCC_CHECK_MATH_FUNC([lgammaf],[AC_DEFINE([HAVE_LGAMMAF],[1],[lgammaf is available])])
GCC_CHECK_MATH_FUNC([lgammal],[AC_DEFINE([HAVE_LGAMMAL],[1],[lgammal is available])])

# Check for GFORTRAN_C99_1.1 funcs
GCC_CHECK_MATH_FUNC([cacos],[AC_DEFINE([HAVE_CACOS],[1],[cacos is available])])
GCC_CHECK_MATH_FUNC([cacosf],[AC_DEFINE([HAVE_CACOSF],[1],[cacosf is available])])
GCC_CHECK_MATH_FUNC([cacosh],[AC_DEFINE([HAVE_CACOSH],[1],[cacosh is available])])
GCC_CHECK_MATH_FUNC([cacoshf],[AC_DEFINE([HAVE_CACOSHF],[1],[cacoshf is available])])
GCC_CHECK_MATH_FUNC([cacoshl],[AC_DEFINE([HAVE_CACOSHL],[1],[cacoshl is available])])
GCC_CHECK_MATH_FUNC([cacosl],[AC_DEFINE([HAVE_CACOSL],[1],[cacosl is available])])
GCC_CHECK_MATH_FUNC([casin],[AC_DEFINE([HAVE_CASIN],[1],[casin is available])])
GCC_CHECK_MATH_FUNC([casinf],[AC_DEFINE([HAVE_CASINF],[1],[casinf is available])])
GCC_CHECK_MATH_FUNC([casinh],[AC_DEFINE([HAVE_CASINH],[1],[casinh is available])])
GCC_CHECK_MATH_FUNC([casinhf],[AC_DEFINE([HAVE_CASINHF],[1],[casinhf is available])])
GCC_CHECK_MATH_FUNC([casinhl],[AC_DEFINE([HAVE_CASINHL],[1],[casinhl is available])])
GCC_CHECK_MATH_FUNC([casinl],[AC_DEFINE([HAVE_CASINL],[1],[casinl is available])])
GCC_CHECK_MATH_FUNC([catan],[AC_DEFINE([HAVE_CATAN],[1],[catan is available])])
GCC_CHECK_MATH_FUNC([catanf],[AC_DEFINE([HAVE_CATANF],[1],[catanf is available])])
GCC_CHECK_MATH_FUNC([catanh],[AC_DEFINE([HAVE_CATANH],[1],[catanh is available])])
GCC_CHECK_MATH_FUNC([catanhf],[AC_DEFINE([HAVE_CATANHF],[1],[catanhf is available])])
GCC_CHECK_MATH_FUNC([catanhl],[AC_DEFINE([HAVE_CATANHL],[1],[catanhl is available])])
GCC_CHECK_MATH_FUNC([catanl],[AC_DEFINE([HAVE_CATANL],[1],[catanl is available])])

# On AIX, clog is present in libm as __clog
AC_CHECK_LIB([m],[__clog],[AC_DEFINE([HAVE_CLOG],[1],[libm includes clog])])

# Check whether the system has a working stat()
LIBGFOR_CHECK_WORKING_STAT

# Check whether __mingw_snprintf() is present
LIBGFOR_CHECK_MINGW_SNPRINTF

# Check for a broken powf implementation
LIBGFOR_CHECK_FOR_BROKEN_POWF

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
if test $ac_cv_func_clock_gettime = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [AC_DEFINE(HAVE_CLOCK_GETTIME_LIBRT, 1,
               [Define to 1 if you have the `clock_gettime' function in librt.])])
fi

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
LIBGFOR_CHECK_ATTRIBUTE_DLLEXPORT
LIBGFOR_CHECK_ATTRIBUTE_ALIAS

# Check out sync builtins support.
LIBGFOR_CHECK_SYNC_FETCH_AND_ADD

# Check out #pragma weak.
LIBGFOR_GTHREAD_WEAK

# Various other checks on target
LIBGFOR_CHECK_UNLINK_OPEN_FILE

# Check whether line terminator is LF or CRLF
LIBGFOR_CHECK_CRLF

# Check whether we have _Unwind_GetIPInfo for backtrace
GCC_CHECK_UNWIND_GETIPINFO

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
