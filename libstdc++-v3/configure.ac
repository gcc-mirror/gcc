# Process this file with autoreconf to produce a configure script.

AC_INIT(package-unused, version-unused,, libstdc++)
AC_CONFIG_SRCDIR(src/shared/hashtable-aux.cc)
AC_CONFIG_HEADER(config.h)

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.  Only used at the end of this file.
### am handles this now?  ORIGINAL_LD_FOR_MULTILIBS=$LD

# Find the rest of the source tree framework.
AM_ENABLE_MULTILIB(, ..)

# Gets build, host, target, *_vendor, *_cpu, *_os, etc.
#
# You will slowly go insane if you do not grok the following fact:  when
# building v3 as part of the compiler, the top-level /target/ becomes the
# library's /host/.  configure then causes --target to default to --host,
# exactly like any other package using autoconf.  Therefore, 'target' and
# 'host' will always be the same.  This makes sense both for native and
# cross compilers, just think about it for a little while.  :-)
#
# Also, if v3 is being configured as part of a cross compiler, the top-level
# configure script will pass the "real" host as $with_cross_host.
#
# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

# Handy for debugging:
#AC_MSG_NOTICE($build / $host / $target / $host_alias / $target_alias); sleep 5

if test "$build" != "$host"; then
  # We are being configured with some form of cross compiler.
  GLIBCXX_IS_NATIVE=false
  case "$host","$target" in
    # Darwin crosses can use the host system's libraries and headers,
    # because of the fat library support.  Of course, it must be the
    # same version of Darwin on both sides.  Allow the user to
    # just say --target=foo-darwin without a version number to mean
    # "the version on this system".
      *-*-darwin*,*-*-darwin*)
	hostos=`echo $host | sed 's/.*-darwin/darwin/'`
	targetos=`echo $target | sed 's/.*-darwin/darwin/'`
	if test $hostos = $targetos || test $targetos = darwin ; then
	  GLIBCXX_IS_NATIVE=true
	fi
	;;

      *)
	GCC_NO_EXECUTABLES
	;;
  esac
else
  GLIBCXX_IS_NATIVE=true
fi

# In the case that we're building without headers, we won't have <stdio.h>
# available. In these cases, we have to instruct autotools to never include
# <stdio.h> as a part of default headers.
m4_version_prereq([2.70], [], [
if test "x$with_headers" = "xno"; then
  ac_includes_default=`echo "$ac_includes_default" | sed '/^#include <stdio.h>$/d'`
fi
])

# Sets up automake.  Must come after AC_CANONICAL_SYSTEM.  Each of the
# following is magically included in AUTOMAKE_OPTIONS in each Makefile.am.
#  1.x:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  no-dependencies:  turns off auto dependency generation (just for now)
#  no-dist:  we don't want 'dist' and related rules.
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is now required.
AM_INIT_AUTOMAKE([1.9.3 no-define foreign no-dependencies no-dist -Wall -Wno-portability -Wno-override])
AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

# -fno-builtin must be present here so that a non-conflicting form of
# std::exit can be guessed by AC_PROG_CXX, and used in later tests.

save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -fno-builtin"
AC_PROG_CC
AC_PROG_CXX
CXXFLAGS="$save_CXXFLAGS"

AC_SYS_LARGEFILE

# Runs configure.host, and assorted other critical bits.  Sets
# up critical shell variables.
GLIBCXX_CONFIGURE

# Libtool setup.
if test "x${with_newlib}" != "xyes" &&
    test "x${with_avrlibc}" != "xyes" &&
    test "x$with_headers" != "xno"; then
  AC_LIBTOOL_DLOPEN
fi
AM_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AC_SUBST(enable_shared)
AC_SUBST(enable_static)
AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])
os_is_darwin=no
case ${host_os} in
  darwin*) os_is_darwin=yes ;;
  *) ;;
esac
AM_CONDITIONAL([OS_IS_DARWIN], [test x${os_is_darwin} = xyes])

if test "$enable_vtable_verify" = yes; then
  predep_objects_CXX="${predep_objects_CXX} ${glibcxx_builddir}/../libgcc/vtv_start.o"
  postdep_objects_CXX="${postdep_objects_CXX} ${glibcxx_builddir}/../libgcc/vtv_end.o"
fi


# libtool variables for C++ shared and position-independent compiles.
#
# Use glibcxx_lt_pic_flag to designate the automake variable
# used to encapsulate the default libtool approach to creating objects
# with position-independent code. Default: -prefer-pic.
#
# Use glibcxx_compiler_shared_flag to designate a compile-time flags for
# creating shared objects. Default: -D_GLIBCXX_SHARED.
#
# Use glibcxx_compiler_pic_flag to designate a compile-time flags for
# creating position-independent objects. This varies with the target
# hardware and operating system, but is often: -DPIC -fPIC.
if test "$enable_shared" = yes; then
  glibcxx_lt_pic_flag="-prefer-pic"
  glibcxx_compiler_pic_flag="$lt_prog_compiler_pic_CXX"
  glibcxx_compiler_shared_flag="-D_GLIBCXX_SHARED"

else
  glibcxx_lt_pic_flag=
  glibcxx_compiler_pic_flag=
  glibcxx_compiler_shared_flag=
fi
AC_SUBST(glibcxx_lt_pic_flag)
AC_SUBST(glibcxx_compiler_pic_flag)
AC_SUBST(glibcxx_compiler_shared_flag)

# Override the libtool's pic_flag and pic_mode.
# Do this step after AM_PROG_LIBTOOL, but before AC_OUTPUT.
# NB: this impacts --with-pic and --without-pic.
lt_prog_compiler_pic_CXX="$glibcxx_compiler_pic_flag $glibcxx_compiler_shared_flag"
pic_mode='default'

# Eliminate -lstdc++ addition to postdeps for cross compiles.
postdeps_CXX=`echo " $postdeps_CXX " | sed 's, -lstdc++ ,,g'`

# Possibly disable most of the library.
## TODO: Consider skipping unncessary tests altogether in this case, rather
## than just ignoring the results.  Faster /and/ more correct, win win.
GLIBCXX_ENABLE_HOSTED

# Enable descriptive messages to standard output on termination.
GLIBCXX_ENABLE_VERBOSE

# Enable compiler support that doesn't require linking.
GLIBCXX_ENABLE_PCH($is_hosted)
GLIBCXX_ENABLE_THREADS
GLIBCXX_ENABLE_ATOMIC_BUILTINS
GLIBCXX_ENABLE_LOCK_POLICY
GLIBCXX_ENABLE_DECIMAL_FLOAT
GLIBCXX_ENABLE_FLOAT128
if test "$enable_float128" = yes; then
  port_specific_symbol_files="$port_specific_symbol_files \$(top_srcdir)/config/abi/pre/float128.ver"
fi

# Checks for compiler support that doesn't require linking.
GLIBCXX_CHECK_COMPILER_FEATURES

# Enable all the variable C++ runtime options that don't require linking.
GLIBCXX_ENABLE_CSTDIO
GLIBCXX_ENABLE_CLOCALE
GLIBCXX_ENABLE_ALLOCATOR
GLIBCXX_ENABLE_CHEADERS($c_model)  dnl c_model from configure.host
GLIBCXX_ENABLE_LONG_LONG([yes])
GLIBCXX_ENABLE_WCHAR_T([yes])
GLIBCXX_ENABLE_C99([yes])
GLIBCXX_ENABLE_CONCEPT_CHECKS([no])
GLIBCXX_ENABLE_DEBUG_FLAGS(["-g3 -O0 -D_GLIBCXX_ASSERTIONS"])
GLIBCXX_ENABLE_DEBUG([no])
GLIBCXX_ENABLE_PARALLEL([yes])
GLIBCXX_ENABLE_CXX_FLAGS
GLIBCXX_ENABLE_FULLY_DYNAMIC_STRING([no])
GLIBCXX_ENABLE_EXTERN_TEMPLATE([yes])
GLIBCXX_ENABLE_PYTHON
GLIBCXX_ENABLE_WERROR([no])
GLIBCXX_ENABLE_VTABLE_VERIFY([no])

# Checks for operating systems support that doesn't require linking.
GLIBCXX_CHECK_STDIO_PROTO
GLIBCXX_CHECK_MATH11_PROTO
GLIBCXX_CHECK_UCHAR_H

# For LFS support.
GLIBCXX_CHECK_LFS

# For showmanyc_helper().
AC_CHECK_HEADERS(sys/ioctl.h sys/filio.h)
GLIBCXX_CHECK_POLL
GLIBCXX_CHECK_S_ISREG_OR_S_IFREG

# For xsputn_2().
AC_CHECK_HEADERS(sys/uio.h)
GLIBCXX_CHECK_WRITEV

# Check for fenv.h and complex.h before GLIBCXX_CHECK_C99_TR1
# so that the check is done with the C compiler (not C++).
# Checking with C++ can break a canadian cross build if either
# file does not exist in C but does in C++.
AC_CHECK_HEADERS(fenv.h complex.h)

# For C99 support to TR1.
GLIBCXX_CHECK_C99_TR1

# For the EOF, SEEK_CUR, and SEEK_END integer constants.
GLIBCXX_COMPUTE_STDIO_INTEGER_CONSTANTS

# For gettimeofday support.
GLIBCXX_CHECK_GETTIMEOFDAY

# For clock_gettime, nanosleep and sched_yield support.
GLIBCXX_ENABLE_LIBSTDCXX_TIME

# Check for tmpnam which is obsolescent in POSIX.1-2008
GLIBCXX_CHECK_TMPNAM

# For pthread_cond_clockwait
GLIBCXX_CHECK_PTHREAD_COND_CLOCKWAIT

# For pthread_mutex_clocklock
GLIBCXX_CHECK_PTHREAD_MUTEX_CLOCKLOCK

# For pthread_rwlock_clockrdlock and pthread_rwlock_clockwrlock
GLIBCXX_CHECK_PTHREAD_RWLOCK_CLOCKLOCK

AC_LC_MESSAGES

# For hardware_concurrency
AC_CHECK_HEADERS(sys/sysinfo.h)
GLIBCXX_CHECK_GET_NPROCS
AC_CHECK_HEADERS(unistd.h)
GLIBCXX_CHECK_SC_NPROCESSORS_ONLN
GLIBCXX_CHECK_SC_NPROC_ONLN
GLIBCXX_CHECK_PTHREADS_NUM_PROCESSORS_NP
GLIBCXX_CHECK_SYSCTL_HW_NCPU
GLIBCXX_CHECK_SDT_H

# Check for available headers.
AC_CHECK_HEADERS([endian.h execinfo.h float.h fp.h ieeefp.h inttypes.h \
locale.h machine/endian.h machine/param.h nan.h stdint.h stdlib.h string.h \
strings.h sys/ipc.h sys/isa_defs.h sys/machine.h sys/param.h \
sys/resource.h sys/sem.h sys/stat.h sys/time.h sys/types.h unistd.h \
wchar.h wctype.h linux/types.h])

AC_CHECK_HEADERS([linux/random.h], [], [],
[[#ifdef HAVE_LINUX_TYPES_H
# include <linux/types.h>
#endif
]])

AC_CHECK_HEADERS([xlocale.h])

# Only do link tests if native. Else, hardcode.
if $GLIBCXX_IS_NATIVE; then

  # We can do more elaborate tests that assume a working linker.
  CANADIAN=no

  GLIBCXX_CHECK_LINKER_FEATURES
  GLIBCXX_CHECK_MATH_SUPPORT
  GLIBCXX_CHECK_STDLIB_SUPPORT

  # For /dev/random and /dev/urandom for std::random_device.
  GLIBCXX_CHECK_DEV_RANDOM

  # For TLS support.
  GCC_CHECK_TLS

  AC_CHECK_FUNCS(__cxa_thread_atexit_impl __cxa_thread_atexit)
  AC_CHECK_FUNCS(aligned_alloc posix_memalign memalign _aligned_malloc)
  AC_CHECK_FUNCS(_wfopen)
  AC_CHECK_FUNCS(secure_getenv)

  # C11 functions for C++17 library
  AC_CHECK_FUNCS(timespec_get)

  # For Networking TS.
  AC_CHECK_FUNCS(sockatmark)

  # Non-standard functions used by C++17 std::from_chars
  AC_CHECK_FUNCS(uselocale)

  # For iconv support.
  AM_ICONV

else

  # This lets us hard-code the functionality we know we'll have in the cross
  # target environment.  "Let" is a sugar-coated word placed on an especially
  # dull and tedious hack, actually.
  #
  # Here's why GLIBCXX_CHECK_MATH_SUPPORT, and other autoconf macros
  # that involve linking, can't be used:
  #    "cannot open sim-crt0.o"
  #    "cannot open crt0.o"
  # etc.  All this is because there currently exists no unified, consistent
  # way for top level CC information to be passed down to target directories:
  # newlib includes, newlib linking info, libgloss versus newlib crt0.o, etc.
  # When all of that is done, all of this hokey, excessive AC_DEFINE junk for
  # crosses can be removed.

  # If Canadian cross, then don't pick up tools from the build directory.
  # Used only in GLIBCXX_EXPORT_INCLUDES.
  if test -n "$with_cross_host" &&
     test x"$build_alias" != x"$with_cross_host" &&
     test x"$build" != x"$target";
  then
    CANADIAN=yes
  else
    CANADIAN=no
  fi

  # Construct crosses by hand, eliminating bits that need ld...
  # GLIBCXX_CHECK_MATH_SUPPORT

  # First, test for "known" system libraries.  We may be using newlib even
  # on a hosted environment.
  if test "x${with_newlib}" = "xyes"; then
    os_include_dir="os/newlib"
    AC_DEFINE(HAVE_HYPOT)

    # GLIBCXX_CHECK_STDLIB_SUPPORT
    AC_DEFINE(HAVE_STRTOF)

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
    AC_DEFINE(HAVE_HYPOTF)
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

dnl # Support for the long version of some math libraries depends on
dnl # architecture and newlib version.  So test for their availability
dnl # rather than hardcoding that information.
    GLIBCXX_CHECK_MATH_DECLS([
      acosl asinl atan2l atanl ceill coshl cosl expl fabsl floorl fmodl
      frexpl hypotl ldexpl log10l logl modfl powl sinhl sinl sqrtl
      tanhl tanl])

    AC_DEFINE(HAVE_ICONV)
    AC_DEFINE(HAVE_MEMALIGN)

    case "${target}" in
      *-rtems*)
        case "${target}" in
          bfin* | lm32* | mips* | moxie* | or1k* | v850*)
            ;;
          *)
            AC_DEFINE(HAVE_TLS)
            ;;
        esac
        AC_DEFINE(HAVE_ALIGNED_ALLOC)
        AC_DEFINE(HAVE_AT_QUICK_EXIT)
        AC_DEFINE(HAVE_LINK)
        AC_DEFINE(HAVE_QUICK_EXIT)
        AC_DEFINE(HAVE_READLINK)
        AC_DEFINE(HAVE_SETENV)
        AC_DEFINE(HAVE_SLEEP)
        AC_DEFINE(HAVE_SOCKATMARK)
        AC_DEFINE(HAVE_STRERROR_L)
        AC_DEFINE(HAVE_SYMLINK)
        AC_DEFINE(HAVE_TRUNCATE)
        AC_DEFINE(HAVE_USLEEP)
        ;;
    esac
  elif test "x$with_headers" != "xno"; then
    GLIBCXX_CROSSCONFIG
  fi

  # At some point, we should differentiate between architectures
  # like x86, which have long double versions, and alpha/powerpc/etc.,
  # which don't. For the time being, punt.
  if test x"long_double_math_on_this_cpu" = x"yes"; then
    AC_DEFINE(HAVE_ACOSL)
    AC_DEFINE(HAVE_ASINL)
    AC_DEFINE(HAVE_ATAN2L)
    AC_DEFINE(HAVE_ATANL)
    AC_DEFINE(HAVE_CEILL)
    AC_DEFINE(HAVE_COSL)
    AC_DEFINE(HAVE_COSHL)
    AC_DEFINE(HAVE_EXPL)
    AC_DEFINE(HAVE_FABSL)
    AC_DEFINE(HAVE_FLOORL)
    AC_DEFINE(HAVE_FMODL)
    AC_DEFINE(HAVE_FREXPL)
    AC_DEFINE(HAVE_LDEXPL)
    AC_DEFINE(HAVE_LOG10L)
    AC_DEFINE(HAVE_LOGL)
    AC_DEFINE(HAVE_MODFL)
    AC_DEFINE(HAVE_POWL)
    AC_DEFINE(HAVE_SINCOSL)
    AC_DEFINE(HAVE_SINL)
    AC_DEFINE(HAVE_SINHL)
    AC_DEFINE(HAVE_SQRTL)
    AC_DEFINE(HAVE_TANL)
    AC_DEFINE(HAVE_TANHL)
  fi
fi

# Check for _Unwind_GetIPInfo.
GCC_CHECK_UNWIND_GETIPINFO

GCC_LINUX_FUTEX([AC_DEFINE(HAVE_LINUX_FUTEX, 1, [Define if futex syscall is available.])])

GLIBCXX_ENABLE_SYMVERS([yes])
AC_SUBST(libtool_VERSION)

GLIBCXX_ENABLE_LIBSTDCXX_VISIBILITY([yes])

GLIBCXX_ENABLE_LIBSTDCXX_DUAL_ABI([yes])
GLIBCXX_DEFAULT_ABI

ac_ldbl_compat=no
ac_ldbl_alt128_compat=no
ac_ldbl_ieee128_default=no
LONG_DOUBLE_COMPAT_FLAGS="-mlong-double-64"
LONG_DOUBLE_128_FLAGS=
LONG_DOUBLE_ALT128_COMPAT_FLAGS=
case "$target" in
  powerpc*-*-linux* | \
  sparc*-*-linux* | \
  s390*-*-linux* | \
  alpha*-*-linux*)
  AC_TRY_COMPILE(, [
#if !defined __LONG_DOUBLE_128__ || (defined(__sparc__) && defined(__arch64__))
#error no need for long double compatibility
#endif
  ], [ac_ldbl_compat=yes], [ac_ldbl_compat=no])
  if test "$ac_ldbl_compat" = yes; then
    AC_DEFINE([_GLIBCXX_LONG_DOUBLE_COMPAT],1,
	      [Define if compatibility should be provided for -mlong-double-64.])
    port_specific_symbol_files="\$(top_srcdir)/config/os/gnu-linux/ldbl-extra.ver"
    case "$target" in
      powerpc*-*-linux*)
	LONG_DOUBLE_COMPAT_FLAGS="$LONG_DOUBLE_COMPAT_FLAGS -mno-gnu-attribute"
        # Check for IEEE128 support in libm:
        AC_CHECK_LIB(m, __frexpieee128,
                     [ac_ldbl_ieee128_in_libc=yes],
                     [ac_ldbl_ieee128_in_libc=no])
        if test $ac_ldbl_ieee128_in_libc = yes; then
          # Determine which long double format is the compiler's default:
          AC_TRY_COMPILE(, [
            #ifndef __LONG_DOUBLE_IEEE128__
            #error compiler defaults to ibm128
            #endif
          ], [ac_ldbl_ieee128_default=yes], [ac_ldbl_ieee128_default=no])
          # Library objects should use default long double format.
          if test "$ac_ldbl_ieee128_default" = yes; then
            LONG_DOUBLE_128_FLAGS="-mno-gnu-attribute"
            # Except for the ones that explicitly use these flags:
            LONG_DOUBLE_ALT128_COMPAT_FLAGS="-mabi=ibmlongdouble -mno-gnu-attribute -Wno-psabi"
          else
            LONG_DOUBLE_128_FLAGS="-mno-gnu-attribute"
            LONG_DOUBLE_ALT128_COMPAT_FLAGS="-mabi=ieeelongdouble -mno-gnu-attribute -Wno-psabi"
          fi
          AC_DEFINE([_GLIBCXX_LONG_DOUBLE_ALT128_COMPAT],1,
                [Define if compatibility should be provided for alternative 128-bit long double formats.])
          port_specific_symbol_files="$port_specific_symbol_files \$(top_srcdir)/config/os/gnu-linux/ldbl-ieee128-extra.ver"
          ac_ldbl_alt128_compat=yes
        else
          ac_ldbl_alt128_compat=no
        fi
	;;
    esac
  fi
esac
AC_SUBST(LONG_DOUBLE_COMPAT_FLAGS)
AC_SUBST(LONG_DOUBLE_128_FLAGS)
AC_SUBST(LONG_DOUBLE_ALT128_COMPAT_FLAGS)
GLIBCXX_CONDITIONAL(GLIBCXX_LDBL_COMPAT, test $ac_ldbl_compat = yes)
GLIBCXX_CONDITIONAL(GLIBCXX_LDBL_ALT128_COMPAT, test $ac_ldbl_alt128_compat = yes)

# Check if assembler supports disabling hardware capability support.
GCC_CHECK_ASSEMBLER_HWCAP

# Check if assembler supports rdrand opcode.
GLIBCXX_CHECK_X86_RDRAND
# Check if assembler supports rdseed opcode.
GLIBCXX_CHECK_X86_RDSEED

# Check for other random number APIs
GLIBCXX_CHECK_GETENTROPY
GLIBCXX_CHECK_ARC4RANDOM

# This depends on GLIBCXX_ENABLE_SYMVERS and GLIBCXX_IS_NATIVE.
GLIBCXX_CONFIGURE_TESTSUITE

# For gthread support.  Depends on GLIBCXX_ENABLE_SYMVERS.
GLIBCXX_CHECK_GTHREADS

# For Filesystem TS.
AC_CHECK_HEADERS([fcntl.h dirent.h sys/statvfs.h utime.h])
GLIBCXX_ENABLE_FILESYSTEM_TS
GLIBCXX_CHECK_FILESYSTEM_DEPS

GLIBCXX_ENABLE_BACKTRACE

# For Networking TS.
AC_CHECK_HEADERS([fcntl.h sys/ioctl.h sys/socket.h sys/uio.h poll.h netdb.h arpa/inet.h netinet/in.h netinet/tcp.h])
AC_CHECK_DECL(F_GETFL,,,[#include <fcntl.h>])
AC_CHECK_DECL(F_SETFL,,,[#include <fcntl.h>])
if test "$ac_cv_have_decl_F_GETFL$ac_cv_have_decl_F_SETFL" = yesyes ; then
  AC_CHECK_DECL(O_NONBLOCK,,,[#include <fcntl.h>])
fi

# For Transactional Memory TS
GLIBCXX_CHECK_SIZE_T_MANGLING

# Check which release added std::exception_ptr for the target
GLIBCXX_CHECK_EXCEPTION_PTR_SYMVER

# For libsupc++/eh_alloc.cc defaults.
GLIBCXX_EMERGENCY_EH_ALLOC

# For src/c++20/tzdb.cc defaults.
GLIBCXX_ZONEINFO_DIR

# For src/c++11/shared_ptr.cc alignment.
GLIBCXX_CHECK_ALIGNAS_CACHELINE

# For using init_priority in ios_init.cc
GLIBCXX_CHECK_INIT_PRIORITY

# For __basic_file::native_handle()
GLIBCXX_CHECK_FILEBUF_NATIVE_HANDLES

# For std::text_encoding
GLIBCXX_CHECK_TEXT_ENCODING

# Define documentation rules conditionally.

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER([MAKEINFO], [makeinfo], [--version],
		   [GNU texinfo.* \([0-9][0-9.]*\)],
		   [4.[4-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
AM_CONDITIONAL(BUILD_INFO, test $gcc_cv_prog_makeinfo_modern = "yes")

# Check for doxygen
AC_CHECK_PROG([DOXYGEN], doxygen, yes, no)
AC_CHECK_PROG([DOT], dot, yes, no)

# Check for docbook
AC_CHECK_PROG([XMLCATALOG], xmlcatalog, yes, no)
AC_CHECK_PROG([XSLTPROC], xsltproc, yes, no)
AC_CHECK_PROG([XMLLINT], xmllint, yes, no)
GLIBCXX_CONFIGURE_DOCBOOK

# Check for xml/html dependencies.
AM_CONDITIONAL(BUILD_XML,
	       test $ac_cv_prog_DOXYGEN = "yes" &&
	       test $ac_cv_prog_DOT = "yes" &&
	       test $ac_cv_prog_XSLTPROC = "yes" &&
	       test $ac_cv_prog_XMLLINT = "yes" &&
	       test $glibcxx_stylesheets = "yes")

AM_CONDITIONAL(BUILD_HTML,
	       test $ac_cv_prog_DOXYGEN = "yes" &&
	       test $ac_cv_prog_DOT = "yes" &&
	       test $ac_cv_prog_XSLTPROC = "yes" &&
	       test $ac_cv_prog_XMLLINT = "yes" &&
	       test $glibcxx_stylesheets = "yes")

# Check for man dependencies.
AM_CONDITIONAL(BUILD_MAN,
	       test $ac_cv_prog_DOXYGEN = "yes" &&
	       test $ac_cv_prog_DOT = "yes")

# Check for pdf dependencies.
AC_CHECK_PROG([DBLATEX], dblatex, yes, no)
AC_CHECK_PROG([PDFLATEX], pdflatex, yes, no)
AM_CONDITIONAL(BUILD_PDF,
	       test $ac_cv_prog_DOXYGEN = "yes" &&
	       test $ac_cv_prog_DOT = "yes" &&
	       test $ac_cv_prog_XSLTPROC = "yes" &&
	       test $ac_cv_prog_XMLLINT = "yes" &&
	       test $ac_cv_prog_DBLATEX = "yes" &&
	       test $ac_cv_prog_PDFLATEX = "yes")

case "$build" in
 *-*-darwin* ) glibcxx_include_dir_notparallel=yes ;;
 * ) glibcxx_include_dir_notparallel=no ;;
esac
AM_CONDITIONAL(INCLUDE_DIR_NOTPARALLEL,
               test $glibcxx_include_dir_notparallel = "yes")

# Propagate the target-specific source directories through the build chain.
ATOMICITY_SRCDIR=config/${atomicity_dir}
ATOMIC_WORD_SRCDIR=config/${atomic_word_dir}
ATOMIC_FLAGS=${atomic_flags}
CPU_DEFINES_SRCDIR=config/${cpu_defines_dir}
OS_INC_SRCDIR=config/${os_include_dir}
ERROR_CONSTANTS_SRCDIR=config/${error_constants_dir}
ABI_TWEAKS_SRCDIR=config/${abi_tweaks_dir}
CPU_OPT_EXT_RANDOM=config/${cpu_opt_ext_random}
CPU_OPT_BITS_RANDOM=config/${cpu_opt_bits_random}
AC_SUBST(ATOMICITY_SRCDIR)
AC_SUBST(ATOMIC_WORD_SRCDIR)
AC_SUBST(ATOMIC_FLAGS)
AC_SUBST(CPU_DEFINES_SRCDIR)
AC_SUBST(ABI_TWEAKS_SRCDIR)
AC_SUBST(OS_INC_SRCDIR)
AC_SUBST(ERROR_CONSTANTS_SRCDIR)
AC_SUBST(CPU_OPT_EXT_RANDOM)
AC_SUBST(CPU_OPT_BITS_RANDOM)

# Conditionalize the makefile for this target machine.
tmake_file_=
for f in ${tmake_file}
do
	if test -f ${srcdir}/config/$f
	then
		tmake_file_="${tmake_file_} \$(srcdir)/config/$f"
	fi
done
tmake_file="${tmake_file_}"
AC_SUBST(tmake_file)

# Add CET specific flags if Intel CET is enabled.
GCC_CET_FLAGS(CET_FLAGS)
EXTRA_CXX_FLAGS="$EXTRA_CXX_FLAGS $CET_FLAGS"
EXTRA_CFLAGS="$EXTRA_CFLAGS $CET_FLAGS"
AC_SUBST(EXTRA_CFLAGS)
AC_SUBST(EXTRA_CXX_FLAGS)

# Determine cross-compile flags and AM_CONDITIONALs.
#AC_SUBST(GLIBCXX_IS_NATIVE)
#AM_CONDITIONAL(CANADIAN, test $CANADIAN = yes)
GLIBCXX_EVALUATE_CONDITIONALS

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Export all the install information.
GLIBCXX_EXPORT_INSTALL_INFO

# Export all the include and flag information to Makefiles.
GLIBCXX_EXPORT_INCLUDES
GLIBCXX_EXPORT_FLAGS

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

dnl In autoconf 2.5x, AC_OUTPUT is replaced by four AC_CONFIG_* macros,
dnl which can all be called multiple times as needed, plus one (different)
dnl AC_OUTPUT macro.  This one lists the files to be created:
AC_CONFIG_FILES(Makefile)
AC_CONFIG_FILES([scripts/testsuite_flags],[chmod +x scripts/testsuite_flags])
AC_CONFIG_FILES([scripts/extract_symvers],[chmod +x scripts/extract_symvers])
AC_CONFIG_FILES([doc/xsl/customization.xsl])
AC_CONFIG_FILES([src/libbacktrace/backtrace-supported.h])

# Multilibs need MULTISUBDIR defined correctly in certain makefiles so
# that multilib installs will end up installed in the correct place.
# The testsuite needs it for multilib-aware ABI baseline files.
# To work around this not being passed down from config-ml.in ->
# srcdir/Makefile.am -> srcdir/{src,libsupc++,...}/Makefile.am, manually
# append it here.  Only modify Makefiles that have just been created.
#
# Also, get rid of this simulated-VPATH thing that automake does.
AC_CONFIG_FILES(AC_FOREACH([DIR], glibcxx_SUBDIRS, [DIR/Makefile ]),
  [cat > vpsed$$ << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   sed -f vpsed$$ $ac_file > tmp$$
   mv tmp$$ $ac_file
   rm vpsed$$
   echo 'MULTISUBDIR =' >> $ac_file
   ml_norecursion=yes
   . ${multi_basedir}/config-ml.in
   AS_UNSET([ml_norecursion])
])

AC_CONFIG_COMMANDS([generate-headers],
  [(cd include && ${MAKE-make} pch_build= )])

dnl And this actually makes things happen:
AC_OUTPUT
