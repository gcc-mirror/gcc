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

AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([long double])
AS_VAR_ARITH([double_type_size], [$ac_cv_sizeof_double \* 8])
AS_VAR_ARITH([long_double_type_size], [$ac_cv_sizeof_long_double \* 8])
AC_SUBST(double_type_size)
AC_SUBST(long_double_type_size)

# Check for decimal float support.
AC_CACHE_CHECK([whether decimal floating point is supported], [libgcc_cv_dfp],
	       [AC_COMPILE_IFELSE([_Decimal32 x;], [libgcc_cv_dfp=yes],
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

# The sjlj test is almost duplicated here and in libgo/configure.ac (for C),
# libstdc++-v3/acinclude.m4 and libjava/configure.ac (for C++), and
# libobjc/configure.ac (for Objective-C).
# FIXME: This should be centralized in config/sjlj.m4.
AC_ARG_ENABLE(sjlj-exceptions,
  AC_HELP_STRING([--enable-sjlj-exceptions],
		 [force use of builtin_setjmp for exceptions]),
  [case "$enableval" in
   yes|no|auto) ;;
   *) AC_MSG_ERROR([unknown argument to --enable-sjlj-exceptions]) ;;
   esac],
  [enable_sjlj_exceptions=auto])

AC_CACHE_CHECK([whether to use setjmp/longjmp exceptions],
[libgcc_cv_lib_sjlj_exceptions],
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
libgcc_cv_lib_sjlj_exceptions=unknown
AS_IF([ac_fn_c_try_compile],
  [if grep _Unwind_SjLj_Resume conftest.s >/dev/null 2>&1; then
    libgcc_cv_lib_sjlj_exceptions=yes
  elif grep _Unwind_Resume conftest.s >/dev/null 2>&1; then
    libgcc_cv_lib_sjlj_exceptions=no
  fi])
CFLAGS=$CFLAGS_hold
rm -f conftest*
])

if test "$enable_sjlj_exceptions" = "auto"; then
  enable_sjlj_exceptions=$libgcc_cv_lib_sjlj_exceptions
fi

case $enable_sjlj_exceptions in
yes)
  AC_DEFINE(LIBGCC_SJLJ_EXCEPTIONS, 1,
	[Define if the C compiler is configured for setjmp/longjmp exceptions.])
  ;;
no)
  ;;
*)
  AC_MSG_ERROR([unable to detect exception model])
  ;;
esac

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

# Check 32bit or 64bit
cat > conftest.c <<EOF
#if defined(__x86_64__) || (!defined(__i386__) && defined(__LP64__))
host_address=64
#else
host_address=32
#endif
EOF
eval `${CC-cc} -E conftest.c | grep host_address=`
rm -f conftest.c

# Collect host-machine-specific information.
. ${srcdir}/config.host

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
else
    vis_hide=
fi
AC_SUBST(vis_hide)

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
case $target_thread_file in
    aix)	thread_header=config/rs6000/gthr-aix.h ;;
    dce)	thread_header=config/pa/gthr-dce.h ;;
    lynx)	thread_header=config/gthr-lynx.h ;;
    mipssde)	thread_header=config/mips/gthr-mipssde.h ;;
    posix)	thread_header=gthr-posix.h ;;
    rtems)	thread_header=config/gthr-rtems.h ;;
    single)	thread_header=gthr-single.h ;;
    tpf)	thread_header=config/s390/gthr-tpf.h ;;
    vxworks)	thread_header=config/gthr-vxworks.h ;;
    win32)	thread_header=config/i386/gthr-win32.h ;;
esac

# Substitute configuration variables
AC_SUBST(cpu_type)
AC_SUBST(extra_parts)
AC_SUBST(asm_hidden_op)
AC_CONFIG_LINKS([enable-execute-stack.c:$enable_execute_stack])
AC_CONFIG_LINKS([unwind.h:$unwind_header])
AC_CONFIG_LINKS([md-unwind-support.h:config/$md_unwind_header])
AC_CONFIG_LINKS([sfp-machine.h:config/$sfp_machine_header])
AC_CONFIG_LINKS([gthr-default.h:$thread_header])

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
