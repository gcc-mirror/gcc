# configure.ac -- Go library configure script.

# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Process this file with autoreconf to produce configure.

AC_PREREQ(2.64)
AC_INIT(package-unused, version-unused,, libgo)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_HEADER(config.h)

libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

AM_INIT_AUTOMAKE([1.9.3 no-define foreign no-dist -Wall])
AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

m4_rename([_AC_ARG_VAR_PRECIOUS],[glibgo_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_GO
m4_rename_force([glibgo_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AM_MAINTAINER_MODE

AC_PROG_LD
AC_PROG_RANLIB
AC_CHECK_TOOL(OBJCOPY, objcopy, missing-objcopy)

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

WARN_FLAGS='-Wall -Wextra -Wwrite-strings -Wcast-qual'
AC_SUBST(WARN_FLAGS)

dnl FIXME: This should be controlled by --enable-maintainer-mode.
WERROR="-Werror"
AC_SUBST(WERROR)

glibgo_toolexecdir=no
glibgo_toolexeclibdir=no
glibgo_prefixdir=$prefix

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE([version-specific-runtime-libs],
  AC_HELP_STRING([--enable-version-specific-runtime-libs],
                 [Specify that runtime libraries should be installed in a compiler-specific directory]),
  [case "$enableval" in
    yes) version_specific_libs=yes ;;
    no)  version_specific_libs=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
   esac],
  [version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

# Version-specific runtime libs processing.
if test $version_specific_libs = yes; then
  glibgo_toolexecdir='${libdir}/gcc/${host_alias}'
  glibgo_toolexeclibdir='${toolexecdir}/${gcc_version}$(MULTISUBDIR)'
fi

# Calculate glibgo_toolexecdir, glibgo_toolexeclibdir
# Install a library built with a cross compiler in tooldir, not libdir.
if test x"$glibgo_toolexecdir" = x"no"; then
  if test -n "$with_cross_host" &&
     test x"$with_cross_host" != x"no"; then
    glibgo_toolexecdir='${exec_prefix}/${host_alias}'
    glibgo_toolexeclibdir='${toolexecdir}/lib'
  else
    glibgo_toolexecdir='${libdir}/gcc/${host_alias}'
    glibgo_toolexeclibdir='${libdir}'
  fi
  multi_os_directory=`$CC -print-multi-os-directory`
  case $multi_os_directory in
    .) ;; # Avoid trailing /.
    *) glibgo_toolexeclibdir=$glibgo_toolexeclibdir/$multi_os_directory ;;
  esac
fi

AC_SUBST(glibgo_prefixdir)
AC_SUBST(glibgo_toolexecdir)
AC_SUBST(glibgo_toolexeclibdir)

# See if the user wants to configure without libffi.  Some
# architectures don't support it.  FIXME: We should set a default
# based on the host.
AC_ARG_WITH(libffi,
  AS_HELP_STRING([--without-libffi],
                 [don't use libffi]),
  [:],
  [with_libffi=${with_libffi_default-yes}])

LIBFFI=
LIBFFIINCS=
if test "$with_libffi" != no; then
   AC_DEFINE(USE_LIBFFI, 1, [Define if we're to use libffi.])
   LIBFFI=../libffi/libffi_convenience.la
   LIBFFIINCS='-I$(top_srcdir)/../libffi/include -I../libffi/include'
fi
AC_SUBST(LIBFFI)
AC_SUBST(LIBFFIINCS)

is_darwin=no
is_freebsd=no
is_linux=no
is_rtems=no
is_solaris=no
GOOS=unknown
case ${host} in
  *-*-darwin*)   is_darwin=yes;  GOOS=darwin ;;
  *-*-freebsd*)  is_freebsd=yes; GOOS=freebsd ;;
  *-*-linux*)    is_linux=yes;   GOOS=linux ;;
  *-*-rtems*)    is_rtems=yes;   GOOS=rtems ;;
  *-*-solaris2*) is_solaris=yes; GOOS=solaris ;;
esac
AM_CONDITIONAL(LIBGO_IS_DARWIN, test $is_darwin = yes)
AM_CONDITIONAL(LIBGO_IS_FREEBSD, test $is_freebsd = yes)
AM_CONDITIONAL(LIBGO_IS_LINUX, test $is_linux = yes)
AM_CONDITIONAL(LIBGO_IS_RTEMS, test $is_rtems = yes)
AM_CONDITIONAL(LIBGO_IS_SOLARIS, test $is_solaris = yes)
AC_SUBST(GOOS)

dnl N.B. Keep in sync with gcc/testsuite/go.test/go-test.exp (go-set-goarch).
is_386=no
is_arm=no
is_m68k=no
is_mips=no
is_mips64=no
is_ppc=no
is_ppc64=no
is_sparc=no
is_sparc64=no
is_x86_64=no
GOARCH=unknown
case ${host} in
  arm*-*-* | strongarm*-*-* | ep9312*-*-* | xscale-*-*)
    is_arm=yes
    GOARCH=arm
    ;;
changequote(,)dnl
  i[34567]86-*-* | x86_64-*-*)
changequote([,])dnl
    AC_COMPILE_IFELSE([
#ifdef __x86_64__
#error 64-bit
#endif],
[is_386=yes], [is_x86_64=yes])
    if test "$is_386" = "yes"; then
      GOARCH=386
    else
      GOARCH=amd64
    fi
    ;;
  m68k*-*-*)
    is_m68k=yes
    GOARCH=m68k
    ;;
  mips*-*-*)
    AC_COMPILE_IFELSE([
#ifdef __mips64
#error 64-bit
#endif],
[is_mips=yes], [is_mips64=yes])
    if test "$is_mips" = "yes"; then
      GOARCH=mips
    else
      GOARCH=mips64
    fi
    ;;
  rs6000*-*-* | powerpc*-*-*)
    AC_COMPILE_IFELSE([
#ifdef _ARCH_PPC64
#error 64-bit
#endif],
[is_ppc=yes], [is_ppc64=yes])
    if test "$is_ppc" = "yes"; then
      GOARCH=ppc
    else
      GOARCH=ppc64
    fi
    ;;
  sparc*-*-*)
    AC_COMPILE_IFELSE([
#if defined(__sparcv9) || defined(__arch64__)
#error 64-bit
#endif],
[is_sparc=yes], [is_sparc64=yes])
    if test "$is_sparc" = "yes"; then
      GOARCH=sparc
    else
      GOARCH=sparc64
    fi
    ;;
esac
AM_CONDITIONAL(LIBGO_IS_386, test $is_386 = yes)
AM_CONDITIONAL(LIBGO_IS_ARM, test $is_arm = yes)
AM_CONDITIONAL(LIBGO_IS_M68K, test $is_m68k = yes)
AM_CONDITIONAL(LIBGO_IS_MIPS, test $is_mips = yes)
AM_CONDITIONAL(LIBGO_IS_MIPS64, test $is_mips64 = yes)
AM_CONDITIONAL(LIBGO_IS_PPC, test $is_ppc = yes)
AM_CONDITIONAL(LIBGO_IS_PPC64, test $is_ppc64 = yes)
AM_CONDITIONAL(LIBGO_IS_SPARC, test $is_sparc = yes)
AM_CONDITIONAL(LIBGO_IS_SPARC64, test $is_sparc64 = yes)
AM_CONDITIONAL(LIBGO_IS_X86_64, test $is_x86_64 = yes)
AC_SUBST(GOARCH)

dnl Some files are only present when needed for specific architectures.
GO_SYSCALLS_SYSCALL_OS_ARCH_FILE=
if test -f ${srcdir}/syscalls/syscall_${GOOS}_${GOARCH}.go; then
  GO_SYSCALLS_SYSCALL_OS_ARCH_FILE=syscalls/syscall_${GOOS}_${GOARCH}.go
fi
AC_SUBST(GO_SYSCALLS_SYSCALL_OS_ARCH_FILE)

GO_DEBUG_PROC_REGS_OS_ARCH_FILE=
if test -f ${srcdir}/go/debug/proc/regs_${GOOS}_${GOARCH}.go; then
  GO_DEBUG_PROC_REGS_OS_ARCH_FILE=go/debug/proc/regs_${GOOS}_${GOARCH}.go
fi
AC_SUBST(GO_DEBUG_PROC_REGS_OS_ARCH_FILE)

dnl Use -fsplit-stack when compiling C code if available.
AC_CACHE_CHECK([whether -fsplit-stack is supported],
[libgo_cv_c_split_stack_supported],
[CFLAGS_hold=$CFLAGS
CFLAGS="$CFLAGS -fsplit-stack"
AC_COMPILE_IFELSE([[int i;]],
[libgo_cv_c_split_stack_supported=yes],
[libgo_cv_c_split_stack_supported=no])
CFLAGS=$CFLAGS_hold])
if test "$libgo_cv_c_split_stack_supported" = yes; then
  SPLIT_STACK=-fsplit-stack
  AC_DEFINE(USING_SPLIT_STACK, 1,
		[Define if the compiler supports -fsplit-stack])
else
  SPLIT_STACK=
fi
AC_SUBST(SPLIT_STACK)
AM_CONDITIONAL(USING_SPLIT_STACK,
	test "$libgo_cv_c_split_stack_supported" = yes)

dnl Check whether the linker does stack munging when calling from
dnl split-stack into non-split-stack code.  We check this by looking
dnl at the --help output.  FIXME: This is only half right: it's
dnl possible for the linker to support this for some targets but not
dnl others.
AC_CACHE_CHECK([whether linker supports split stack],
[libgo_cv_c_linker_supports_split_stack],
libgo_cv_c_linker_supports_split_stack=no
if $LD --help 2>/dev/null | grep split-stack-adjust-size >/dev/null 2>&1; then
  libgo_cv_c_linker_supports_split_stack=yes
fi)
if test "$libgo_cv_c_linker_supports_split_stack" = yes; then
  AC_DEFINE(LINKER_SUPPORTS_SPLIT_STACK, 1,
	    [Define if the linker support split stack adjustments])
fi

dnl Test for the -lm library.
MATH_LIBS=
AC_CHECK_LIB([m], [sqrt], MATH_LIBS=-lm)
AC_SUBST(MATH_LIBS)

dnl Test for -lsocket and -lnsl.  Copied from libjava/configure.ac.
AC_CACHE_CHECK([for socket libraries], libgo_cv_lib_sockets,
  [libgo_cv_lib_sockets=
   libgo_check_both=no
   AC_CHECK_FUNC(connect, libgo_check_socket=no, libgo_check_socket=yes)
   if test "$libgo_check_socket" = "yes"; then
     unset ac_cv_func_connect
     AC_CHECK_LIB(socket, main, libgo_cv_lib_sockets="-lsocket",
     		  libgo_check_both=yes)
   fi
   if test "$libgo_check_both" = "yes"; then
     libgo_old_libs=$LIBS
     LIBS="$LIBS -lsocket -lnsl"
     unset ac_cv_func_accept
     AC_CHECK_FUNC(accept,
		   [libgo_check_nsl=no
		    libgo_cv_lib_sockets="-lsocket -lnsl"])
     unset ac_cv_func_accept
     LIBS=$libgo_old_libs
   fi
   unset ac_cv_func_gethostbyname
   libgo_old_libs="$LIBS"
   AC_CHECK_FUNC(gethostbyname, ,
		 [AC_CHECK_LIB(nsl, main,
		 	[libgo_cv_lib_sockets="$libgo_cv_lib_sockets -lnsl"])])
   unset ac_cv_func_gethostbyname
   LIBS=$libgo_old_libs
])
NET_LIBS="$libgo_cv_lib_sockets"
AC_SUBST(NET_LIBS)

dnl Test whether the compiler supports the -pthread option.
AC_CACHE_CHECK([whether -pthread is supported],
[libgo_cv_lib_pthread],
[CFLAGS_hold=$CFLAGS
CFLAGS="$CFLAGS -pthread"
AC_COMPILE_IFELSE([[int i;]],
[libgo_cv_lib_pthread=yes],
[libgo_cv_lib_pthread=no])
CFLAGS=$CFLAGS_hold])
PTHREAD_CFLAGS=
if test "$libgo_cv_lib_pthread" = yes; then
  PTHREAD_CFLAGS=-pthread
fi
AC_SUBST(PTHREAD_CFLAGS)

dnl Test for the -lpthread library.
PTHREAD_LIBS=
AC_CHECK_LIB([pthread], [pthread_create], PTHREAD_LIBS=-lpthread)
AC_SUBST(PTHREAD_LIBS)

dnl Test if -lrt is required for sched_yield.
AC_SEARCH_LIBS([sched_yield], [rt])

AC_C_BIGENDIAN

GCC_CHECK_UNWIND_GETIPINFO

AC_ARG_ENABLE(sjlj-exceptions,
  AC_HELP_STRING([--enable-sjlj-exceptions],
		 [force use of builtin_setjmp for exceptions]),
  [case "$enableval" in
   yes|no|auto) ;;
   *) AC_MSG_ERROR([unknown argument to --enable-sjlj-exceptions]) ;;
   esac],
  [enable_sjlj_exceptions=auto])

AC_CACHE_CHECK([whether to use setjmp/longjmp exceptions],
[libgo_cv_lib_sjlj_exceptions],
[AC_LANG_CONFTEST(
  [AC_LANG_SOURCE([
void bar ();
void clean (int *);
void foo ()
{
  int i __attribute__ ((cleanup (clean)));
  bar();
}
])])
CFLAGS_hold=$CFLAGS
CFLAGS="--save-temps -fexceptions"
libgo_cv_lib_sjlj_exceptions=unknown
AS_IF([ac_fn_c_try_compile],
  [if grep _Unwind_SjLj_Resume conftest.s >/dev/null 2>&1; then
    libgo_cv_lib_sjlj_exceptions=yes
  elif grep _Unwind_Resume conftest.s >/dev/null 2>&1; then
    libgo_cv_lib_sjlj_exceptions=no
  fi])
CFLAGS=$CFLAGS_hold
rm -f conftest*
])

if test "$enable_sjlj_exceptions" = "auto"; then
  enable_sjlj_exceptions=$libgo_cv_lib_sjlj_exceptions
fi

case $enable_sjlj_exceptions in
yes)
  AC_DEFINE(LIBGO_SJLJ_EXCEPTIONS, 1,
	[Define if the C++ compiler is configured for setjmp/longjmp exceptions.])
  ;;
no)
  ;;
*)
  AC_MSG_ERROR([unable to detect exception model])
  ;;
esac

AC_CHECK_HEADERS(sys/mman.h syscall.h sys/epoll.h sys/ptrace.h sys/syscall.h sys/user.h sys/utsname.h)
AM_CONDITIONAL(HAVE_SYS_MMAN_H, test "$ac_cv_header_sys_mman_h" = yes)
AC_CHECK_FUNCS(srandom random strsignal)

AC_CACHE_CHECK([for __sync_bool_compare_and_swap_4],
[libgo_cv_func___sync_bool_compare_and_swap_4],
[AC_LINK_IFELSE([
typedef unsigned int uint32  __attribute__ ((mode (SI)));
uint32 i;
int main() { return __sync_bool_compare_and_swap (&i, 0, 1); }
],
[libgo_cv_func___sync_bool_compare_and_swap_4=yes],
[libgo_cv_func___sync_bool_compare_and_swap_4=no])])
if test "$libgo_cv_func___sync_bool_compare_and_swap_4" = "yes"; then
  AC_DEFINE(HAVE_SYNC_BOOL_COMPARE_AND_SWAP_4, 1,
    [Define to 1 if the compiler provides the __sync_bool_compare_and_swap function for uint32])
fi

AC_CACHE_CHECK([for __sync_fetch_and_add_4],
[libgo_cv_func___sync_fetch_and_add_4],
[AC_LINK_IFELSE([
typedef unsigned int uint32  __attribute__ ((mode (SI)));
uint32 i;
int main() { return __sync_fetch_and_add (&i, 1); }
],
[libgo_cv_func___sync_fetch_and_add_4=yes],
[libgo_cv_func___sync_fetch_and_add_4=no])])
if test "$libgo_cv_func___sync_fetch_and_add_4" = "yes"; then
  AC_DEFINE(HAVE_SYNC_FETCH_AND_ADD_4, 1,
    [Define to 1 if the compiler provides the __sync_fetch_and_add function for uint32])
fi

dnl For x86 we want to use the -minline-all-stringops option to avoid
dnl forcing a stack split when calling memcpy and friends.
AC_CACHE_CHECK([whether compiler supports -minline-all-stringops],
[libgo_cv_c_stringops],
[CFLAGS_hold=$CFLAGS
CFLAGS="$CFLAGS -minline-all-stringops"
AC_COMPILE_IFELSE([int i;],
[libgo_cv_c_stringops=yes],
[libgo_cv_c_stringops=no])
CFLAGS=$CFLAGS_hold])
STRINGOPS_FLAG=
if test "$libgo_cv_c_stringops" = yes; then
  STRINGOPS_FLAG=-minline-all-stringops
fi
AC_SUBST(STRINGOPS_FLAG)

CFLAGS_hold=$CFLAGS
CFLAGS="$CFLAGS -D_LARGEFILE_SOURCE -D_LARGEFILE64_SOURCE"
AC_CHECK_TYPES(off64_t)
CFLAGS=$CFLAGS_hold

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES(Makefile testsuite/Makefile)

AC_CONFIG_COMMANDS([default],
[if test -n "$CONFIG_FILES"; then
   # Multilibs need MULTISUBDIR defined correctly in certain makefiles so
   # that multilib installs will end up installed in the correct place.
   # The testsuite needs it for multilib-aware ABI baseline files.
   # To work around this not being passed down from config-ml.in ->
   # srcdir/Makefile.am -> srcdir/{src,libsupc++,...}/Makefile.am, manually
   # append it here.  Only modify Makefiles that have just been created.
   #
   # Also, get rid of this simulated-VPATH thing that automake does.
   cat > vpsed << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   for i in $SUBDIRS; do
    case $CONFIG_FILES in
     *${i}/Makefile*)
       #echo "Adding MULTISUBDIR to $i/Makefile"
       sed -f vpsed $i/Makefile > tmp
       grep '^MULTISUBDIR =' Makefile >> tmp
       mv tmp $i/Makefile
       ;;
    esac
   done
   rm vpsed
 fi
],
[
# Variables needed in config.status (file generation) which aren't already
# passed by autoconf.
SUBDIRS="$SUBDIRS"
])

AC_OUTPUT
