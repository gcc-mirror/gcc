#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT(package-unused, version-unused, libsanitizer)
AC_CONFIG_SRCDIR([include/sanitizer/common_interface_defs.h])

AM_ENABLE_MULTILIB(, ..)

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

AC_USE_SYSTEM_EXTENSIONS

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)
GCC_LIBSTDCXX_RAW_CXX_FLAGS

AM_INIT_AUTOMAKE(foreign no-dist)
AM_MAINTAINER_MODE

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

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
AC_PROG_RANLIB

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL

AC_PROG_AWK
case "$AWK" in
"") AC_MSG_ERROR([can't build without awk]) ;;
esac

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AC_CHECK_SIZEOF([void *])

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Get target configury.
unset TSAN_SUPPORTED
unset LSAN_SUPPORTED
unset HWASAN_SUPPORTED
. ${srcdir}/configure.tgt
AM_CONDITIONAL(TSAN_SUPPORTED, [test "x$TSAN_SUPPORTED" = "xyes"])
AM_CONDITIONAL(LSAN_SUPPORTED, [test "x$LSAN_SUPPORTED" = "xyes"])
AM_CONDITIONAL(HWASAN_SUPPORTED, [test "x$HWASAN_SUPPORTED" = "xyes"])

# Check for functions needed.
AC_CHECK_FUNCS(clock_getres clock_gettime clock_settime lstat readlink)

# Common libraries that we need to link against for all sanitizer libs.
link_sanitizer_common='-lpthread -lm'

# At least for glibc, shm_open is in librt.  But don't pull that
# in if it still doesn't give us the function we want.  This
# test is copied from libgomp.
AC_CHECK_LIB(rt, shm_open,
  [link_sanitizer_common="-lrt $link_sanitizer_common"])

# Do a configure time check for -ldl
AC_CHECK_LIB(dl, dlsym,
  [link_sanitizer_common="-ldl $link_sanitizer_common"])

# Set up the set of additional libraries that we need to link against for libasan.
link_libasan=$link_sanitizer_common
AC_SUBST(link_libasan)

# Set up the set of additional libraries that we need to link against for libhwasan.
link_libhwasan=$link_sanitizer_common
AC_SUBST(link_libhwasan)

# Set up the set of additional libraries that we need to link against for libtsan.
link_libtsan=$link_sanitizer_common
AC_SUBST(link_libtsan)

# Set up the set of additional libraries that we need to link against for libubsan.
link_libubsan=$link_sanitizer_common
AC_SUBST(link_libubsan)

# Set up the set of additional libraries that we need to link against for liblsan.
link_liblsan=$link_sanitizer_common
AC_SUBST(link_liblsan)


# At least for glibc, clock_gettime is in librt.  But don't pull that
# in if it still doesn't give us the function we want.  This
# test is copied from libgomp.
AC_CHECK_LIB(rt, clock_gettime,
  [link_libasan="-lrt $link_libasan"
link_libtsan="-lrt $link_libtsan"
# Other sanitizers do not override clock_* API
])

case "$host" in
  *-*-darwin*) MAC_INTERPOSE=true ; enable_static=no ;;
  *) MAC_INTERPOSE=false ;;
esac
AM_CONDITIONAL(USING_MAC_INTERPOSE, $MAC_INTERPOSE)

backtrace_supported=yes

AC_MSG_CHECKING([for necessary platform features])
case "$target" in
  *-*-linux*)
    # Some old Linux distributions miss required syscalls.
    sanitizer_supported=no
    AC_TRY_COMPILE([#include <sys/syscall.h>
#include <unistd.h>],[
      syscall (__NR_gettid);
      syscall (__NR_futex);
      syscall (__NR_exit_group);
    ], [sanitizer_supported=yes])
    ;;
  *)
    sanitizer_supported=yes
    ;;
esac
AC_MSG_RESULT($sanitizer_supported)
AM_CONDITIONAL(SANITIZER_SUPPORTED, test "$sanitizer_supported" = yes)

# Test for __sync support.
AC_CACHE_CHECK([__sync extensions],
[libsanitizer_cv_sys_sync],
[if test -n "${with_target_subdir}"; then
   libsanitizer_cv_sys_sync=yes
 else
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM([int i;],
                      [__sync_bool_compare_and_swap (&i, i, i);
                       __sync_lock_test_and_set (&i, 1);
                       __sync_lock_release (&i);])],
     [libsanitizer_cv_sys_sync=yes],
     [libsanitizer_cv_sys_sync=no])
 fi])
if test "$libsanitizer_cv_sys_sync" = "yes"; then
  AC_DEFINE([HAVE_SYNC_FUNCTIONS], 1,
	    [Define to 1 if you have the __sync functions])
fi

# Test for __atomic support.
AC_CACHE_CHECK([__atomic extensions],
[libsanitizer_cv_sys_atomic],
[if test -n "${with_target_subdir}"; then
   libsanitizer_cv_sys_atomic=yes
 else
   AC_LINK_IFELSE(
     [AC_LANG_PROGRAM([int i;],
     		      [__atomic_load_n (&i, __ATOMIC_ACQUIRE);
		       __atomic_store_n (&i, 1, __ATOMIC_RELEASE);])],
     [libsanitizer_cv_sys_atomic=yes],
     [libsanitizer_cv_sys_atomic=no])
 fi])
if test "$libsanitizer_cv_sys_atomic" = "yes"; then
  AC_DEFINE([HAVE_ATOMIC_FUNCTIONS], 1,
	    [Define to 1 if you have the __atomic functions])
fi

# The library needs to be able to read the executable itself.  Compile
# a file to determine the executable format.  The awk script
# filetype.awk prints out the file type.
AC_CACHE_CHECK([output filetype],
[libsanitizer_cv_sys_filetype],
[filetype=
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([int i;], [int j;])],
  [filetype=`${AWK} -f $srcdir/../libbacktrace/filetype.awk conftest.$ac_objext`],
  [AC_MSG_FAILURE([compiler failed])])
libsanitizer_cv_sys_filetype=$filetype])

# Match the file type to decide what files to compile.
FORMAT_FILE=
case "$libsanitizer_cv_sys_filetype" in
elf*) FORMAT_FILE="elf.lo" ;;
*) AC_MSG_WARN([could not determine output file type])
   FORMAT_FILE="unknown.lo"
   backtrace_supported=no
   ;;
esac
AC_SUBST(FORMAT_FILE)

# ELF defines.
elfsize=
case "$libsanitizer_cv_sys_filetype" in
elf32) elfsize=32 ;;
elf64) elfsize=64 ;;
esac
AC_DEFINE_UNQUOTED([BACKTRACE_ELF_SIZE], [$elfsize], [ELF size: 32 or 64])

BACKTRACE_SUPPORTED=0
if test "$backtrace_supported" = "yes"; then
  BACKTRACE_SUPPORTED=1
fi
AC_SUBST(BACKTRACE_SUPPORTED)

GCC_HEADER_STDINT(gstdint.h)

AC_CHECK_HEADERS(sys/mman.h alloca.h)
if test "$ac_cv_header_sys_mman_h" = "no"; then
  have_mmap=no
else
  if test -n "${with_target_subdir}"; then
    # When built as a GCC target library, we can't do a link test.  We
    # simply assume that if we have mman.h, we have mmap.
    have_mmap=yes
  else
    AC_CHECK_FUNC(mmap, [have_mmap=yes], [have_mmap=no])
  fi
fi
if test "$have_mmap" = "no"; then
  VIEW_FILE=read.lo
  ALLOC_FILE=alloc.lo
else
  VIEW_FILE=mmapio.lo
  AC_PREPROC_IFELSE([AC_LANG_SOURCE([
#include <sys/mman.h>
#if !defined(MAP_ANONYMOUS) && !defined(MAP_ANON)
  #error no MAP_ANONYMOUS
#endif
])], [ALLOC_FILE=mmap.lo], [ALLOC_FILE=alloc.lo])
fi
AC_SUBST(VIEW_FILE)
AC_SUBST(ALLOC_FILE)

BACKTRACE_USES_MALLOC=0
if test "$ALLOC_FILE" = "alloc.lo"; then
  BACKTRACE_USES_MALLOC=1
fi
AC_SUBST(BACKTRACE_USES_MALLOC)

# Don't care about thread support
BACKTRACE_SUPPORTS_THREADS=0
AC_SUBST(BACKTRACE_SUPPORTS_THREADS)

# Check for dl_iterate_phdr.
AC_CHECK_HEADERS(link.h)
if test "$ac_cv_header_link_h" = "no"; then
  have_dl_iterate_phdr=no
else
  # When built as a GCC target library, we can't do a link test.
  AC_EGREP_HEADER([dl_iterate_phdr], [link.h], [have_dl_iterate_phdr=yes],
		  [have_dl_iterate_phdr=no])
fi
if test "$have_dl_iterate_phdr" = "yes"; then
  AC_DEFINE(HAVE_DL_ITERATE_PHDR, 1, [Define if dl_iterate_phdr is available.])
fi

# Check for the fcntl function.
if test -n "${with_target_subdir}"; then
   case "${host}" in
   *-*-mingw*) have_fcntl=no ;;
   *) have_fcntl=yes ;;
   esac
else
  AC_CHECK_FUNC(fcntl, [have_fcntl=yes], [have_fcntl=no])
fi
if test "$have_fcntl" = "yes"; then
  AC_DEFINE([HAVE_FCNTL], 1,
	    [Define to 1 if you have the fcntl function])
fi

AC_CHECK_DECLS(strnlen)

# Check for getexecname function.
if test -n "${with_target_subdir}"; then
   case "${host}" in
   *-*-solaris2*) have_getexecname=yes ;;
   *) have_getexecname=no ;;
   esac
else
  AC_CHECK_FUNC(getexecname, [have_getexecname=yes], [have_getexecname=no])
fi
if test "$have_getexecname" = "yes"; then
  AC_DEFINE(HAVE_GETEXECNAME, 1, [Define if getexecname is available.])
fi

# Check for rpc/xdr.h
AC_CHECK_HEADERS(rpc/xdr.h)
if test x"$ac_cv_header_rpc_xdr_h" = xyes; then
  rpc_defs="$rpc_defs -DHAVE_RPC_XDR_H=1"
else
  rpc_defs="$rpc_defs -DHAVE_RPC_XDR_H=0"
fi

# Check for tirpc/rpc/xdr.h
AC_CHECK_HEADERS(tirpc/rpc/xdr.h)
if test x"$ac_cv_header_tirpc_rpc_xdr_h" = xyes; then
  rpc_defs="$rpc_defs -DHAVE_TIRPC_RPC_XDR_H=1"
else
  rpc_defs="$rpc_defs -DHAVE_TIRPC_RPC_XDR_H=0"
fi

AC_SUBST([RPC_DEFS], [$rpc_defs])

AM_CONDITIONAL(LIBBACKTRACE_SUPPORTED,
	       [test "x${BACKTRACE_SUPPORTED}x${BACKTRACE_USES_MALLOC}" = "x1x0"])

AH_BOTTOM([#include "libbacktrace/backtrace-rename.h"])
AC_CONFIG_FILES([Makefile libsanitizer.spec libbacktrace/backtrace-supported.h])
AC_CONFIG_HEADER(config.h)

AC_CONFIG_FILES(AC_FOREACH([DIR], [interception sanitizer_common libbacktrace lsan asan ubsan], [DIR/Makefile ]),
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

if test "x$TSAN_SUPPORTED" = "xyes"; then
  AC_CONFIG_FILES(AC_FOREACH([DIR], [tsan], [DIR/Makefile ]), 
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
fi

if test "x$HWASAN_SUPPORTED" = "xyes"; then
  AC_CONFIG_FILES(AC_FOREACH([DIR], [hwasan], [DIR/Makefile ]), 
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
fi

AC_SUBST([TSAN_TARGET_DEPENDENT_OBJECTS])
AC_SUBST([SANITIZER_COMMON_TARGET_DEPENDENT_OBJECTS])

AC_ARG_ENABLE([werror],
  [AS_HELP_STRING([--disable-werror], [disable building with -Werror])])

ACX_PROG_CC_WARNING_OPTS([-Wextra -Wall -Wwrite-strings \
                          -Wmissing-format-attribute -Wcast-qual],
                          [WARN_FLAGS])
AS_IF([test "x$enable_werror" != "xno"],
  [WARN_FLAGS="$WARN_FLAGS -Werror"])
AC_SUBST([WARN_FLAGS])

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Add CET specific flags if Intel CET is enabled.
GCC_CET_FLAGS(CET_FLAGS)

# Always set -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 to sync with LLVM,
# and keep struct *stat* have the same size.
case "${host}" in
  mips*-*) FILE64_FLAGS="-D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64" ;;
  *) FILE64_FLAGS="" ;;
esac

EXTRA_CFLAGS="$EXTRA_CFLAGS $CET_FLAGS $FILE64_FLAGS"
EXTRA_CXXFLAGS="$EXTRA_CXXFLAGS $CET_FLAGS $FILE64_FLAGS"
EXTRA_ASFLAGS=$CET_FLAGS
AC_SUBST(EXTRA_ASFLAGS)
AC_SUBST(EXTRA_CFLAGS)
AC_SUBST(EXTRA_CXXFLAGS)

AC_OUTPUT
