dnl Process this with autoconf to create configure

AC_PREREQ(2.59)

AC_INIT([libffi], [2.1], [http://gcc.gnu.org/bugs.html])
AC_CONFIG_HEADERS([fficonfig.h])

AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

. ${srcdir}/configure.host

AM_INIT_AUTOMAKE

# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AM_PROG_AS
AM_PROG_CC_C_O
AC_PROG_LIBTOOL

AM_MAINTAINER_MODE

AC_CHECK_HEADERS(sys/mman.h)
AC_CHECK_FUNCS(mmap)
AC_FUNC_MMAP_BLACKLIST

dnl The -no-testsuite modules omit the test subdir.
AM_CONDITIONAL(TESTSUBDIR, test -d $srcdir/testsuite)

TARGETDIR="unknown"
case "$host" in
i*86-*-linux*) TARGET=X86; TARGETDIR=x86;;
i*86-*-solaris*) TARGET=X86; TARGETDIR=x86;;
i*86-*-beos*) TARGET=X86; TARGETDIR=x86;;
i*86-*-freebsd* | i*86-*-kfreebsd*-gnu) TARGET=X86; TARGETDIR=x86;;
i*86-*-netbsdelf* | i*86-*-knetbsd*-gnu) TARGET=X86; TARGETDIR=x86;;
i*86-*-win32*) TARGET=X86_WIN32; TARGETDIR=x86;;
i*86-*-cygwin*) TARGET=X86_WIN32; TARGETDIR=x86;;
i*86-*-mingw*) TARGET=X86_WIN32; TARGETDIR=x86;;
frv-*-*) TARGET=FRV; TARGETDIR=frv;;
sparc-sun-4*) TARGET=SPARC; TARGETDIR=sparc;;
sparc*-sun-*) TARGET=SPARC; TARGETDIR=sparc;;
sparc-*-linux* | sparc-*-netbsdelf* | sparc-*-knetbsd*-gnu) TARGET=SPARC; TARGETDIR=sparc;;
sparc64-*-linux* | sparc64-*-netbsd* | sparc64-*-knetbsd*-gnu) TARGET=SPARC; TARGETDIR=sparc;;
alpha*-*-linux* | alpha*-*-osf* | alpha*-*-freebsd* | alpha*-*-kfreebsd*-gnu | alpha*-*-netbsd* | alpha*-*-knetbsd*-gnu) TARGET=ALPHA; TARGETDIR=alpha;;
ia64*-*-*) TARGET=IA64; TARGETDIR=ia64;;
m32r*-*-linux* ) TARGET=M32R; TARGETDIR=m32r;;
m68k-*-linux*) TARGET=M68K; TARGETDIR=m68k;;
mips64*-*);;
mips-sgi-irix5.* | mips-sgi-irix6.*) TARGET=MIPS_IRIX; TARGETDIR=mips;;
mips*-*-linux*) TARGET=MIPS_LINUX; TARGETDIR=mips;;
powerpc*-*-linux* | powerpc-*-sysv*) TARGET=POWERPC; TARGETDIR=powerpc;;
powerpc-*-beos*) TARGET=POWERPC; TARGETDIR=powerpc;;
powerpc-*-darwin*) TARGET=POWERPC_DARWIN; TARGETDIR=powerpc;;
powerpc-*-aix*) TARGET=POWERPC_AIX; TARGETDIR=powerpc;;
rs6000-*-aix*) TARGET=POWERPC_AIX; TARGETDIR=powerpc;;
arm*-*-linux-*) TARGET=ARM; TARGETDIR=arm;;
arm*-*-netbsdelf* | arm*-*-knetbsd*-gnu) TARGET=ARM; TARGETDIR=arm;;
s390-*-linux-*) TARGET=S390; TARGETDIR=s390;;
s390x-*-linux-*) TARGET=S390; TARGETDIR=s390;;
x86_64-*-linux* | x86_64-*-freebsd* | x86_64-*-kfreebsd*-gnu) TARGET=X86_64; TARGETDIR=x86;;
sh-*-linux* | sh[[34]]*-*-linux*) TARGET=SH; TARGETDIR=sh;;
sh64-*-linux* | sh5*-*-linux*) TARGET=SH64; TARGETDIR=sh64;;
hppa-*-linux* | parisc-*-linux*) TARGET=PA; TARGETDIR=pa;;
esac

AC_SUBST(AM_RUNTESTFLAGS)

if test $TARGETDIR = unknown; then
  AC_MSG_ERROR(["libffi has not been ported to $host."])
fi

AM_CONDITIONAL(MIPS_IRIX, test x$TARGET = xMIPS_IRIX)
AM_CONDITIONAL(MIPS_LINUX, test x$TARGET = xMIPS_LINUX)
AM_CONDITIONAL(SPARC, test x$TARGET = xSPARC)
AM_CONDITIONAL(X86, test x$TARGET = xX86)
AM_CONDITIONAL(X86_WIN32, test x$TARGET = xX86_WIN32)
AM_CONDITIONAL(ALPHA, test x$TARGET = xALPHA)
AM_CONDITIONAL(IA64, test x$TARGET = xIA64)
AM_CONDITIONAL(M32R, test x$TARGET = xM32R)
AM_CONDITIONAL(M68K, test x$TARGET = xM68K)
AM_CONDITIONAL(POWERPC, test x$TARGET = xPOWERPC)
AM_CONDITIONAL(POWERPC_AIX, test x$TARGET = xPOWERPC_AIX)
AM_CONDITIONAL(POWERPC_DARWIN, test x$TARGET = xPOWERPC_DARWIN)
AM_CONDITIONAL(ARM, test x$TARGET = xARM)
AM_CONDITIONAL(FRV, test x$TARGET = xFRV)
AM_CONDITIONAL(S390, test x$TARGET = xS390)
AM_CONDITIONAL(X86_64, test x$TARGET = xX86_64)
AM_CONDITIONAL(SH, test x$TARGET = xSH)
AM_CONDITIONAL(SH64, test x$TARGET = xSH64)
AM_CONDITIONAL(PA, test x$TARGET = xPA)

case x$TARGET in
  xMIPS*) TARGET=MIPS ;;
  *) ;;
esac

AC_HEADER_STDC
AC_CHECK_FUNCS(memcpy)
AC_FUNC_ALLOCA

AC_COMPILE_CHECK_SIZEOF(double)
AC_COMPILE_CHECK_SIZEOF(long double)

# Also AC_SUBST this variable for ffi.h.
HAVE_LONG_DOUBLE=0
if test $ac_cv_sizeof_double != $ac_cv_sizeof_long_double; then
  if test $ac_cv_sizeof_long_double != 0; then
    HAVE_LONG_DOUBLE=1
    AC_DEFINE(HAVE_LONG_DOUBLE, 1, [Define if you have the long double type and it is bigger than a double])
  fi
fi
AC_SUBST(HAVE_LONG_DOUBLE)

AC_C_BIGENDIAN_CROSS

if test x$TARGET = xSPARC; then
    AC_CACHE_CHECK([assembler and linker support unaligned pc related relocs],
	libffi_cv_as_sparc_ua_pcrel, [
	save_CFLAGS="$CFLAGS"
	save_LDFLAGS="$LDFLAGS"
	CFLAGS="$CFLAGS -fpic"
	LDFLAGS="$LDFLAGS -shared"
	AC_TRY_LINK([asm (".text; foo: nop; .data; .align 4; .byte 0; .uaword %r_disp32(foo); .text");],,
		    [libffi_cv_as_sparc_ua_pcrel=yes],
		    [libffi_cv_as_sparc_ua_pcrel=no])
	CFLAGS="$save_CFLAGS"
	LDFLAGS="$save_LDFLAGS"])
    if test "x$libffi_cv_as_sparc_ua_pcrel" = xyes; then
	AC_DEFINE(HAVE_AS_SPARC_UA_PCREL, 1,
		  [Define if your assembler and linker support unaligned PC relative relocs.])
    fi

    AC_CACHE_CHECK([assembler .register pseudo-op support],
       libffi_cv_as_register_pseudo_op, [
       libffi_cv_as_register_pseudo_op=unknown
       # Check if we have .register
       AC_TRY_COMPILE([asm (".register %g2, #scratch");],,
		       [libffi_cv_as_register_pseudo_op=yes],
		       [libffi_cv_as_register_pseudo_op=no])
    ])
    if test "x$libffi_cv_as_register_pseudo_op" = xyes; then
       AC_DEFINE(HAVE_AS_REGISTER_PSEUDO_OP, 1,
	       [Define if your assembler supports .register.])
    fi
fi

AC_CACHE_CHECK([whether .eh_frame section should be read-only],
    libffi_cv_ro_eh_frame, [
	libffi_cv_ro_eh_frame=no
	echo 'extern void foo (void); void bar (void) { foo (); foo (); }' > conftest.c
	if $CC $CFLAGS -S -fpic -fexceptions -o conftest.s conftest.c > /dev/null 2>&1; then
	    if grep '.section.*eh_frame.*"a"' conftest.s > /dev/null; then
		libffi_cv_ro_eh_frame=yes
	    elif grep '.section.*eh_frame.*#alloc' conftest.c \
		 | grep -v '#write' > /dev/null; then
		libffi_cv_ro_eh_frame=yes
	    fi
	fi
	rm -f conftest.*
    ])
if test "x$libffi_cv_ro_eh_frame" = xyes; then
    AC_DEFINE(HAVE_RO_EH_FRAME, 1,
	      [Define if .eh_frame sections should be read-only.])
    AC_DEFINE(EH_FRAME_FLAGS, "a",
	      [Define to the flags needed for the .section .eh_frame directive.])
else
    AC_DEFINE(EH_FRAME_FLAGS, "aw",
	      [Define to the flags needed for the .section .eh_frame directive.])
fi

AC_SUBST(TARGET)
AC_SUBST(TARGETDIR)

AC_SUBST(SHELL)

AC_ARG_ENABLE(debug,
[  --enable-debug          debugging mode],
  if test "$enable_debug" = "yes"; then
    AC_DEFINE(FFI_DEBUG, 1, [Define this if you want extra debugging.])
  fi)

AC_ARG_ENABLE(structs,
[  --disable-structs       omit code for struct support],
  if test "$enable_structs" = "no"; then
    AC_DEFINE(FFI_NO_STRUCTS, 1, [Define this is you do not want support for aggregate types.])
  fi)

AC_ARG_ENABLE(raw-api,
[  --disable-raw-api       make the raw api unavailable],
  if test "$enable_raw_api" = "no"; then
    AC_DEFINE(FFI_NO_RAW_API, 1, [Define this is you do not want support for the raw API.])
  fi)

AC_ARG_ENABLE(purify-safety,
[  --enable-purify-safety  purify-safe mode],
  if test "$enable_purify_safety" = "yes"; then
    AC_DEFINE(USING_PURIFY, 1, [Define this if you are using Purify and want to suppress spurious messages.])
  fi)

if test -n "$with_cross_host" &&
   test x"$with_cross_host" != x"no"; then
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
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

TL_AC_GCC_VERSION([$srcdir/..])

#Figure out where generated headers like ffitarget.h get installed.
tool_include_dir='$(libdir)/gcc/$(target_alias)/'${gcc_version}/include
AC_SUBST(tool_include_dir)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_COMMANDS(include, [test -d include || mkdir include])
AC_CONFIG_COMMANDS(src, [
test -d src || mkdir src
test -d src/$TARGETDIR || mkdir src/$TARGETDIR
], [TARGETDIR="$TARGETDIR"])

AC_CONFIG_LINKS(include/ffitarget.h:src/$TARGETDIR/ffitarget.h)

AC_CONFIG_FILES(include/Makefile include/ffi.h Makefile testsuite/Makefile)

AC_OUTPUT
