dnl Process this file with autoconf to produce a configure script.

sinclude(../config/enable.m4)
sinclude(../config/tls.m4)
sinclude(../config/acx.m4)
sinclude(../config/no-executables.m4)
sinclude(../config/override.m4)

AC_PREREQ(2.59)
AC_INIT([GNU C Runtime Library], 1.0,,[libgcc])
AC_CONFIG_SRCDIR([static-object.mk])

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
GCC_TOPLEV_SUBDIRS

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

# Check for decimal float support.
AC_CACHE_CHECK([whether decimal floating point is supported], [libgcc_cv_dfp],
	       [AC_COMPILE_IFELSE([_Decimal32 x;], [libgcc_cv_dfp=yes],
				  [libgcc_cv_dfp=no])])
decimal_float=$libgcc_cv_dfp
AC_SUBST(decimal_float)

AC_ARG_ENABLE(decimal-float,
[  --enable-decimal-float={no,yes,bid,dpd}
			enable decimal float extension to C.  Selecting 'bid'
			or 'dpd' choses which decimal floating point format
			to use],
[
  case $enable_decimal_float in
    yes | no | bid | dpd) ;;
    *) AC_MSG_ERROR(['$enable_decimal_float' is an invalid value for --enable-decimal-float.
Valid choices are 'yes', 'bid', 'dpd', and 'no'.]) ;;
  esac
],
[
  case $host in
    powerpc*-*-linux* | i?86*-*-linux* | x86_64*-*-linux*)
      enable_decimal_float=yes
      ;;
    *)
      enable_decimal_float=no
      ;;
  esac
])

# x86's use BID format instead of DPD
if test x$enable_decimal_float = xyes; then
  case $host in
    i?86*-*-linux* | x86_64*-*-linux*)
      enable_decimal_float=bid
      ;;
    *)
      enable_decimal_float=dpd
      ;;
  esac
fi
AC_SUBST(enable_decimal_float)

# Check for fixed-point support.
AC_CACHE_CHECK([whether fixed-point is supported], [libgcc_cv_fixed_point],
	       [AC_COMPILE_IFELSE([_Sat _Fract x;], [libgcc_cv_fixed_point=yes],
				  [libgcc_cv_fixed_point=no])])
fixed_point=$libgcc_cv_fixed_point
AC_SUBST(fixed_point)

# Check 32bit or 64bit for x86.
case ${host} in
i?86*-*-* | x86_64*-*-*)
  cat > conftest.c <<EOF
#ifdef __x86_64__
host_address=64
#else
host_address=32
#endif
EOF
    eval `${CC-cc} -E conftest.c | grep host_address=`
    rm -f conftest.c
    ;;
esac

# Collect host-machine-specific information.
. ${srcdir}/config.host

# Check if Solaris/x86 linker supports ZERO terminator unwind entries.
# This is after config.host so we can augment tmake_file.
# Link with -nostartfiles -nodefaultlibs since neither are present while
# building libgcc.
case ${host} in
i?86-*-solaris2.1[[0-9]]*)
  cat > conftest.s <<EOF
	.section	.eh_frame,"a",@unwind
	.zero	4
	.section	.jcr,"aw",@progbits
	.zero	8
EOF
  if AC_TRY_COMMAND(${CC-cc} -shared -nostartfiles -nodefaultlibs -o conftest.so conftest.s 1>&AS_MESSAGE_LOG_FD); then
      # configure expects config files in libgcc/config, so need a relative
      # path here.
      tmake_file="${tmake_file} ../../gcc/config/i386/t-crtstuff"
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
# sicne link-time and run-time tests require the newly built
# gcc, which can't be used to build executable due to that libgcc
# is yet to be built here.
GCC_CHECK_CC_TLS
set_have_cc_tls=
if test "$enable_tls $gcc_cv_have_cc_tls" = "yes yes"; then
  set_have_cc_tls="-DHAVE_CC_TLS"
fi
AC_SUBST(set_have_cc_tls)

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

# Substitute configuration variables
AC_SUBST(extra_parts)
AC_SUBST(asm_hidden_op)

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
