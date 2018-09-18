dnl Process this file with autoconf to produce a configure script.

sinclude(../config/enable.m4)
sinclude(../config/tls.m4)
sinclude(../config/acx.m4)
sinclude(../config/no-executables.m4)
sinclude(../config/lib-ld.m4)
sinclude(../config/override.m4)
sinclude(../config/picflag.m4)
sinclude(../config/dfp.m4)
sinclude(../config/unwind_ipinfo.m4)
sinclude(../config/gthr.m4)
sinclude(../config/sjlj.m4)
sinclude(../config/cet.m4)

AC_PREREQ(2.64)
AC_INIT([GNU C Runtime Library], 1.0,,[libgcc])
AC_CONFIG_SRCDIR([static-object.mk])

# The libgcc should not depend on any header files
AC_DEFUN([_AC_INCLUDES_DEFAULT_REQUIREMENTS],
  [m4_divert_text([DEFAULTS],
    [ac_includes_default='/* none */'])])

AC_ARG_WITH(target-subdir,
[  --with-target-subdir=SUBDIR      Configuring in a subdirectory for target])
AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST           Configuring with a cross compiler])
AC_ARG_WITH(ld,
[  --with-ld               arrange to use the specified ld (full pathname)])

if test "${srcdir}" = "."; then
  if test -n "${with_build_subdir}"; then
    libgcc_topdir="${srcdir}/../.."
    with_target_subdir=
  elif test -z "${with_target_subdir}"; then
    libgcc_topdir="${srcdir}/.."
  else
    if test "${with_target_subdir}" != "."; then
      libgcc_topdir="${srcdir}/${with_multisrctop}../.."
    else
      libgcc_topdir="${srcdir}/${with_multisrctop}.."
    fi
  fi
else
  libgcc_topdir="${srcdir}/.."
fi
AC_SUBST(libgcc_topdir)
AC_CONFIG_AUX_DIR($libgcc_topdir)
AC_CONFIG_HEADER(auto-target.h:config.in)

AC_ARG_ENABLE(shared,
[  --disable-shared        don't provide a shared libgcc],
[
  case $enable_shared in
  yes | no) ;;
  *)
    enable_shared=no
    IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
    for pkg in $enableval; do
      if test "X$pkg" = "Xgcc" || test "X$pkg" = "Xlibgcc"; then
	enable_shared=yes
      fi
    done
    IFS="$ac_save_ifs"
    ;;
  esac
], [enable_shared=yes])
AC_SUBST(enable_shared)

AC_ARG_ENABLE(gcov,
[  --disable-gcov          don't provide libgcov and related host tools],
[], [enable_gcov=yes])
AC_SUBST(enable_gcov)

AC_ARG_ENABLE(vtable-verify,
[  --enable-vtable-verify    Enable vtable verification feature ],
[case "$enableval" in
 yes) enable_vtable_verify=yes ;;
 no)  enable_vtable_verify=no ;;
 *)   enable_vtable_verify=no;;
 esac],
[enable_vtable_verify=no])
AC_SUBST(enable_vtable_verify)

AC_ARG_WITH(aix-soname,
[AS_HELP_STRING([--with-aix-soname=aix|svr4|both],
    [shared library versioning (aka "SONAME") variant to provide on AIX])],
[case "${host}:${enable_shared}" in
 power*-*-aix[[5-9]]*:yes)
   AC_MSG_CHECKING([which variant of shared library versioning to provide for shared libgcc])
   case ${withval} in
     aix|svr4|both) ;;
     *) AC_MSG_ERROR([Unknown argument to --with-aix-soname]);;
   esac
   AC_MSG_RESULT($withval)
   ;;
 *) with_aix_soname=aix ;;
 esac
], [with_aix_soname=aix])
AC_SUBST(with_aix_soname)

GCC_PICFLAG
AC_SUBST(PICFLAG)

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

AC_ARG_WITH(slibdir,
[  --with-slibdir=DIR      shared libraries in DIR [LIBDIR]],
slibdir="$with_slibdir",
if test "${version_specific_libs}" = yes; then
  slibdir='$(libsubdir)'
elif test -n "$with_cross_host" && test x"$with_cross_host" != x"no"; then
  slibdir='$(exec_prefix)/$(host_noncanonical)/lib'
else
  slibdir='$(libdir)'
fi)
AC_SUBST(slibdir)

# Command-line options.
# Very limited version of AC_MAINTAINER_MODE.
AC_ARG_ENABLE([maintainer-mode],
  [AC_HELP_STRING([--enable-maintainer-mode],
                 [enable make rules and dependencies not useful (and
                  sometimes confusing) to the casual installer])],
  [case ${enable_maintainer_mode} in
     yes) MAINT='' ;;
     no) MAINT='#' ;;
     *) AC_MSG_ERROR([--enable-maintainer-mode must be yes or no]) ;;
   esac
   maintainer_mode=${enableval}],
  [MAINT='#'])
AC_SUBST([MAINT])dnl

AC_PROG_INSTALL

AC_PROG_AWK
# We need awk; bail out if it's missing.
case ${AWK} in
  "") AC_MSG_ERROR([can't build without awk, bailing out]) ;;
esac

AC_CANONICAL_HOST
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_noncanonical)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
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

dnl These must be called before AM_PROG_LIBTOOL, because it may want
dnl to call AC_CHECK_PROG.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(LIPO, lipo, :)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)
AC_PROG_LN_S

GCC_NO_EXECUTABLES
AC_PROG_CC
AC_PROG_CPP_WERROR

AC_SYS_LARGEFILE

AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([long double])
AS_VAR_ARITH([double_type_size], [$ac_cv_sizeof_double \* 8])
AS_VAR_ARITH([long_double_type_size], [$ac_cv_sizeof_long_double \* 8])
AC_SUBST(double_type_size)
AC_SUBST(long_double_type_size)

AC_CHECK_HEADERS(inttypes.h stdint.h stdlib.h ftw.h \
	unistd.h sys/stat.h sys/types.h \
	string.h strings.h memory.h)
AC_HEADER_STDC

# Check for decimal float support.
AC_CACHE_CHECK([whether decimal floating point is supported], [libgcc_cv_dfp],
	       [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <fenv.h>
]], [[
_Decimal32 x;
int fe_except =
  FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW|FE_UNDERFLOW|FE_INEXACT;
]])],
				  [libgcc_cv_dfp=yes],
				  [libgcc_cv_dfp=no])])
decimal_float=$libgcc_cv_dfp
AC_SUBST(decimal_float)

GCC_AC_ENABLE_DECIMAL_FLOAT([$host])

# Check for fixed-point support.
AC_CACHE_CHECK([whether fixed-point is supported], [libgcc_cv_fixed_point],
	       [AC_COMPILE_IFELSE([_Sat _Fract x;], [libgcc_cv_fixed_point=yes],
				  [libgcc_cv_fixed_point=no])])
fixed_point=$libgcc_cv_fixed_point
AC_SUBST(fixed_point)

# For platforms with the unwind ABI which includes an unwind library,
# libunwind, we can choose to use the system libunwind.
# config.gcc also contains tests of with_system_libunwind.
GCC_CHECK_UNWIND_GETIPINFO

# Check if the compiler is configured for setjmp/longjmp exceptions.
GCC_CHECK_SJLJ_EXCEPTIONS

GCC_CET_FLAGS(CET_FLAGS)
AC_SUBST(CET_FLAGS)

AC_ARG_ENABLE([explicit-exception-frame-registration],
  [AC_HELP_STRING([--enable-explicit-exception-frame-registration],
     [register exception tables explicitly at module start, for use
      e.g. for compatibility with installations without PT_GNU_EH_FRAME support])],
[
force_explicit_eh_registry=
if test "$enable_explicit_exception_frame_registration" = yes; then
  if test $ac_cv_sjlj_exceptions = yes; then
    AC_MSG_ERROR([Can't --enable-explicit-exception-frame-registration
                  with setjmp/longjmp exceptions])
  fi
  force_explicit_eh_registry=-DUSE_EH_FRAME_REGISTRY_ALWAYS
fi
])
AC_SUBST([force_explicit_eh_registry])

AC_LIB_PROG_LD_GNU

AC_MSG_CHECKING([for thread model used by GCC])
target_thread_file=`$CC -v 2>&1 | sed -n 's/^Thread model: //p'`
AC_MSG_RESULT([$target_thread_file]) 

# Check for assembler CFI support.
AC_CACHE_CHECK([whether assembler supports CFI directives], [libgcc_cv_cfi],
	       [AC_COMPILE_IFELSE(
[asm("\n\
	.text\n\
	.cfi_startproc\n\
	.cfi_personality 0, symbol\n\
	.cfi_endproc");],
  [libgcc_cv_cfi=yes],
  [libgcc_cv_cfi=no])])

# Check 32bit or 64bit.  In the case of MIPS, this really determines the
# word size rather than the address size.
cat > conftest.c <<EOF
#if defined(__x86_64__) || (!defined(__i386__) && defined(__LP64__)) \
    || defined(__mips64)
host_address=64
#else
host_address=32
#endif
EOF
eval `${CC-cc} -E conftest.c | grep host_address=`
rm -f conftest.c

case ${host} in
mips*-*-*)
  AC_CACHE_CHECK([whether the target is hard-float],
		 [libgcc_cv_mips_hard_float],
		 [AC_COMPILE_IFELSE(
    [#ifndef __mips_hard_float
     #error FOO
     #endif],
    [libgcc_cv_mips_hard_float=yes],
    [libgcc_cv_mips_hard_float=no])])
esac

case ${host} in
*-*-solaris2*)
  # Check for system-provided CRTs on Solaris 11.4.
  AC_CACHE_CHECK([system-provided CRTs on Solaris],
    		 [libgcc_cv_solaris_crts],
    [libgcc_cv_solaris_crts=no
     libgcc_sysroot="`${CC} -print-sysroot`"
     libgcc_libdir="$libgcc_sysroot/usr/lib"
     # At the time they were added, gcrt1.o became a symlink for backwards
     # compatibility on x86, while crt1.o was added on sparc, so check for that.
     case ${host} in
       i?86-*-solaris2* | x86_64-*-solaris2*)
         if test -h "$libgcc_libdir/gcrt1.o"; then libgcc_cv_solaris_crts=yes; fi
	 ;;
       sparc*-*-solaris2*)
         if test -f "$libgcc_libdir/crt1.o"; then libgcc_cv_solaris_crts=yes; fi
	 ;;
     esac])
  if test $libgcc_cv_solaris_crts = yes; then
    AC_DEFINE(HAVE_SOLARIS_CRTS, 1,
      [Define if the system-provided CRTs are present on Solaris.])
  fi
  ;;
esac

# Determine the version of glibc, if any, used on the target.
AC_MSG_CHECKING([for target glibc version])
AC_ARG_WITH([glibc-version],
  [AS_HELP_STRING([--with-glibc-version=M.N],
    [assume GCC used with glibc version M.N or later])], [
if [echo "$with_glibc_version" | grep '^[0-9][0-9]*\.[0-9][0-9]*$']; then
  glibc_version_major=`echo "$with_glibc_version" | sed -e 's/\..*//'`
  glibc_version_minor=`echo "$with_glibc_version" | sed -e 's/.*\.//'`
else
  AC_MSG_ERROR([option --with-glibc-version requires a version number M.N])
fi], [
AC_COMPUTE_INT([glibc_version_major], [__GLIBC__],
				      [#include <features.h>],
				      [glibc_version_major=0])
AC_COMPUTE_INT([glibc_version_minor], [__GLIBC_MINOR__],
				      [#include <features.h>],
				      [glibc_version_minor=0])])
AC_MSG_RESULT([$glibc_version_major.$glibc_version_minor])

# Determine floating-point type for powerpc*-*-linux*.
# Single-precision-only FPRs are not a supported configuration for
# this target, so are not allowed for in this test.
case ${host} in
powerpc*-*-linux*)
  cat > conftest.c <<EOF
#ifdef __powerpc64__
ppc_fp_type=64
#elif defined _SOFT_FLOAT
ppc_fp_type=soft
#elif defined _SOFT_DOUBLE
ppc_fp_type=e500v1
#elif defined __NO_FPRS__
ppc_fp_type=e500v2
#else
ppc_fp_type=hard
#endif
EOF
eval `${CC-cc} -E conftest.c | grep ppc_fp_type=`
rm -f conftest.c
# glibc 2.19 and later provide all the soft-fp functions, with proper
# interactions with <fenv.h> exception and rounding mode handling, so
# make libgcc's versions into compat symbols if a recent enough glibc
# version is being used.
ppc_fp_compat=
case ${ppc_fp_type} in
soft|e500v1|e500v2)
  if test $glibc_version_major -gt 2 \
    || ( test $glibc_version_major -eq 2 \
        && test $glibc_version_minor -ge 19 ); then
    ppc_fp_compat="t-softfp-compat"
  fi
  ;;
esac
;;
esac

case ${host} in
# At present, we cannot turn -mfloat128 on via #pragma GCC target, so just
# check if we have VSX (ISA 2.06) support to build the software libraries, and
# whether the assembler can handle xsaddqp for hardware support.  Also check if
# a new glibc is being used so that __builtin_cpu_supports can be used.
powerpc*-*-linux*)
  saved_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -mabi=altivec -mvsx -mfloat128"
  AC_CACHE_CHECK([for PowerPC ISA 2.06 to build __float128 libraries],
		 [libgcc_cv_powerpc_float128],
		 [AC_COMPILE_IFELSE(
    [vector double dadd (vector double a, vector double b) { return a + b; }],
    [libgcc_cv_powerpc_float128=yes],
    [libgcc_cv_powerpc_float128=no])])

  CFLAGS="$CFLAGS -mpower9-vector -mfloat128-hardware"
  AC_CACHE_CHECK([for PowerPC ISA 3.0 to build hardware __float128 libraries],
		 [libgcc_cv_powerpc_float128_hw],
		 [AC_COMPILE_IFELSE(
    [#include <sys/auxv.h>
     #ifndef AT_PLATFORM
     #error "AT_PLATFORM is not defined"
     #endif
     #ifndef __BUILTIN_CPU_SUPPORTS__
     #error "__builtin_cpu_supports is not available"
     #endif
     vector unsigned char add (vector unsigned char a, vector unsigned char b)
     {
       vector unsigned char ret;
       __asm__ ("xsaddqp %0,%1,%2" : "=v" (ret) : "v" (a), "v" (b));
       return ret;
     }
     void *add_resolver (void) { return (void *) add; }
     __float128 add_ifunc (__float128, __float128)
	__attribute__ ((__ifunc__ ("add_resolver")));],
    [libgcc_cv_powerpc_float128_hw=yes],
    [libgcc_cv_powerpc_float128_hw=no])])
  CFLAGS="$saved_CFLAGS"
esac

# Collect host-machine-specific information.
. ${srcdir}/config.host

# Used for constructing correct paths for offload compilers.
accel_dir_suffix=
real_host_noncanonical=${host_noncanonical}
if test x"$enable_as_accelerator_for" != x; then
  accel_dir_suffix=/accel/${target_noncanonical}
  real_host_noncanonical=${enable_as_accelerator_for}
fi
AC_SUBST(accel_dir_suffix)
AC_SUBST(real_host_noncanonical)

if test x"$enable_offload_targets" != x; then
  extra_parts="${extra_parts} crtoffloadbegin.o crtoffloadend.o crtoffloadtable.o"
fi

# Check if Solaris/x86 linker supports ZERO terminator unwind entries.
# This is after config.host so we can augment tmake_file.
# Link with -nostartfiles -nodefaultlibs since neither are present while
# building libgcc.
case ${host} in
i?86-*-solaris2* | x86_64-*-solaris2.1[[0-9]]*)
  cat > conftest.s <<EOF
	.section	.eh_frame,"a",@unwind
	.zero	4
	.section	.jcr,"aw",@progbits
	.zero	8
EOF
  if AC_TRY_COMMAND(${CC-cc} -shared -nostartfiles -nodefaultlibs -o conftest.so conftest.s 1>&AS_MESSAGE_LOG_FD); then
      tmake_file="${tmake_file} i386/t-crtstuff"
  fi
  ;;
esac

# Check if Solaris linker support v2 linker mapfile syntax.
# Link with -nostartfiles -nodefaultlibs since neither are present while
# building libgcc.
case ${host} in
*-*-solaris2*)
  solaris_ld_v2_maps=no
  echo 'int main(void) {return 0;}' > conftest.c
  echo '$mapfile_version 2' > conftest.map
  if AC_TRY_COMMAND([${CC-cc} -nostartfiles -nodefaultlibs -Wl,-M,conftest.map -o conftest conftest.c 1>&AS_MESSAGE_LOG_FD]); then
      solaris_ld_v2_maps=yes
  fi
  ;;
esac
AC_SUBST(solaris_ld_v2_maps)

# Check if xtensa target is configured for windowed ABI and thus needs to use
# custom unwind code.
# This is after config.host so we can augment tmake_file.
case ${host} in
xtensa*-*)
  cat > conftest.c <<EOF
	#ifdef __XTENSA_CALL0_ABI__
	#error
	#endif
EOF
  if AC_TRY_COMMAND(${CC-cc} -E -o conftest.i conftest.c 1>&AS_MESSAGE_LOG_FD); then
      tmake_file="${tmake_file} xtensa/t-windowed"
  fi
  ;;
esac

# Check for visibility support.  This is after config.host so that
# we can check for asm_hidden_op.
AC_CACHE_CHECK([for __attribute__((visibility("hidden")))],
    libgcc_cv_hidden_visibility_attribute, [
	echo 'int __attribute__ ((visibility ("hidden"))) foo (void) { return 1; }' > conftest.c
	libgcc_cv_hidden_visibility_attribute=no
	if AC_TRY_COMMAND(${CC-cc} -Werror -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
	    if grep "\\$asm_hidden_op.*foo" conftest.s >/dev/null; then
		libgcc_cv_hidden_visibility_attribute=yes
	    fi
	fi
	rm -f conftest.*
    ])

if test $libgcc_cv_hidden_visibility_attribute = yes; then
    vis_hide='-fvisibility=hidden -DHIDE_EXPORTS'
    AC_DEFINE_UNQUOTED(AS_HIDDEN_DIRECTIVE, $asm_hidden_op, [Define to the .hidden-like directive if it exists.])
else
    vis_hide=
fi
AC_SUBST(vis_hide)

# Check for .cfi_sections .debug_frame support.
AC_CACHE_CHECK([for .cfi_sections .debug_frame],
    libgcc_cv_cfi_sections_directive, [
	echo 'int foo (int, char *);' > conftest.c
	echo 'int bar (int x) { char *y = __builtin_alloca (x); return foo (x + 1, y) + 1; }' >> conftest.c
	libgcc_cv_cfi_sections_directive=no
	if AC_TRY_COMMAND(${CC-cc} -Werror -g -fno-asynchronous-unwind-tables -fno-unwind-tables -fno-exceptions -S conftest.c -o conftest.s 1>&AS_MESSAGE_LOG_FD); then
	    if grep "\\.cfi_sections.*\\.debug_frame" conftest.s >/dev/null; then
		libgcc_cv_cfi_sections_directive=yes
	    fi
	fi
	rm -f conftest.*
    ])
if test $libgcc_cv_cfi_sections_directive = yes; then
    AC_DEFINE(HAVE_AS_CFI_SECTIONS, 1, [Define to 1 if the assembler supports .cfi_sections .debug_frame directive.])
fi

# See if we have thread-local storage.  We can only test assembler
# since link-time and run-time tests require the newly built
# gcc, which can't be used to build executable due to that libgcc
# is yet to be built here.
GCC_CHECK_CC_TLS
set_have_cc_tls=
if test "$enable_tls $gcc_cv_have_cc_tls" = "yes yes"; then
  set_have_cc_tls="-DHAVE_CC_TLS"
fi
AC_SUBST(set_have_cc_tls)

# See if we have emulated thread-local storage.
GCC_CHECK_EMUTLS
set_use_emutls=
if test "$enable_tls $gcc_cv_use_emutls" = "yes yes"; then
  set_use_emutls="-DUSE_EMUTLS"
fi
AC_SUBST(set_use_emutls)

dnl Check if as supports AVX instructions.
AC_DEFUN([LIBGCC_CHECK_AS_AVX], [
case "${target}" in
i[[34567]]86-*-* | x86_64-*-*)
  AC_CACHE_CHECK([if the assembler supports AVX], libgcc_cv_as_avx, [
    AC_TRY_COMPILE([], [asm("vzeroupper");],
		   [libgcc_cv_as_avx=yes], [libgcc_cv_as_avx=no])
  ])
  if test x$libgcc_cv_as_avx = xyes; then
    AC_DEFINE(HAVE_AS_AVX, 1, [Define to 1 if the assembler supports AVX.])
  fi
  ;;
esac])
LIBGCC_CHECK_AS_AVX

dnl Check if as supports RTM instructions.
AC_CACHE_CHECK(for init priority support, libgcc_cv_init_priority, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,
  [[void ip (void) __attribute__ ((constructor (1)));]])],
  [libgcc_cv_init_priority=yes],[libgcc_cv_init_priority=no])])
if test $libgcc_cv_init_priority = yes; then
  AC_DEFINE(HAVE_INIT_PRIORITY, 1,
  [Define if the compiler supports init priority.])
fi

# Conditionalize the sfp-machine.h header for this target machine.
if test -z "${sfp_machine_header}"; then
	sfp_machine_header=$cpu_type/sfp-machine.h
	if test -f ${srcdir}/config/${sfp_machine_header}; then
		:
	else
		sfp_machine_header=no-sfp-machine.h
	fi
fi
AC_SUBST(sfp_machine_header)

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

# Likewise export definitions for libgcc_tm.h
tm_file_=
for f in ${tm_file}
do
	tm_file_="${tm_file_} \$(srcdir)/config/$f"
done
tm_file="${tm_file_}"
AC_SUBST(tm_file)
AC_SUBST(tm_defines)

# Map from thread model to thread header.
GCC_AC_THREAD_HEADER([$target_thread_file])

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Substitute configuration variables
AC_SUBST(cpu_type)
AC_SUBST(extra_parts)
AC_SUBST(asm_hidden_op)
AC_SUBST(enable_execute_stack)
AC_SUBST(unwind_header)
AC_SUBST(md_unwind_header)
AC_SUBST(sfp_machine_header)
AC_SUBST(thread_header)

# We need multilib support.
AC_CONFIG_FILES([Makefile])
AC_CONFIG_COMMANDS([default],
  [[test -z "$CONFIG_HEADERS" || echo timestamp > stamp-h
if test -n "$CONFIG_FILES"; then
  # FIXME: We shouldn't need to set ac_file
  ac_file=Makefile
  . ${libgcc_topdir}/config-ml.in
fi]],
[[srcdir=${srcdir}
host=${host}
with_target_subdir=${with_target_subdir}
with_multisubdir=${with_multisubdir}
ac_configure_args="--enable-multilib ${ac_configure_args}"
CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
libgcc_topdir=${libgcc_topdir}
CC="${CC}"
]])
AC_OUTPUT
