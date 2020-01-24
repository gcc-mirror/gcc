# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([GNU Vtable Verification Runtime Library], 1.0,,[libvtv])
#AC_INIT(package-unused, version-unused, libvtv)
AC_CONFIG_SRCDIR([vtv_rts.h])

AM_ENABLE_MULTILIB(, ..)

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

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)
GCC_LIBSTDCXX_RAW_CXX_FLAGS

AC_USE_SYSTEM_EXTENSIONS

# Use same top-level configure hooks in libgcc/libstdc++/libvtv.
AC_MSG_CHECKING([for --enable-vtable-verify])
AC_ARG_ENABLE(vtable-verify,
[  --enable-vtable-verify    Enable vtable verification feature ],
[case "$enableval" in
 yes) enable_vtable_verify=yes ;;
 no)  enable_vtable_verify=no ;;
 *)   enable_vtable_verify=no;;
 esac],
[enable_vtable_verify=no])
AC_MSG_RESULT($enable_vtable_verify)

# See if supported.
unset VTV_SUPPORTED
AC_MSG_CHECKING([for host support for vtable verification])
. ${srcdir}/configure.tgt
case ${host} in
  *-*-solaris2*)
    # libvtv requires init priority support, which depends on the linker
    # used on Solaris.
    AC_CACHE_CHECK(for init priority support, libvtv_cv_init_priority, [
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,
      [[void ip (void) __attribute__ ((constructor (1)));]])],
      [libgcc_cv_init_priority=yes],[libgcc_cv_init_priority=no])])
    if test x$libvtv_cv_init_priority = xno; then
      VTV_SUPPORTED=no
    fi
    # FIXME: Maybe check for dl_iterate_phdr, too?  Should be covered by
    # configure.tgt restricting to libvtv to Solaris 11+.
    ;;
esac
AC_MSG_RESULT($VTV_SUPPORTED)

# Decide if it's usable.
use_vtable_verify=no
if test "x$VTV_SUPPORTED" = "xyes"; then
  if test "x$enable_vtable_verify" = "xyes"; then
    use_vtable_verify=yes
    AC_MSG_NOTICE(using vtable verification)
  fi
fi 
AM_CONDITIONAL(ENABLE_VTABLE_VERIFY, test $use_vtable_verify = yes)

AM_INIT_AUTOMAKE(foreign no-dist)
AM_MAINTAINER_MODE

LIBVTV_CONFIGURE

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

AC_GNU_SOURCE
AC_CHECK_FUNCS([__secure_getenv])

AC_GNU_SOURCE
AC_CHECK_FUNCS([secure_getenv])

AC_CHECK_FUNCS([getexecname __fortify_fail])

# Check for programs.
m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AM_PROG_CC_C_O

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)

# Newer automakes demand CCAS and CCASFLAGS.
: ${CCAS='$(CC)'}
: ${CCASFLAGS='$(CFLAGS)'}
AC_SUBST(CCAS)
AC_SUBST(CCASFLAGS)

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

# Configure libtool
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

XCFLAGS="-D_GNU_SOURCE -Wall -Wextra -fno-exceptions"

# Add CET specific flags if Intel CET is enabled.
GCC_CET_FLAGS(CET_FLAGS)
XCFLAGS="$XCFLAGS $CET_FLAGS"

AC_SUBST(XCFLAGS)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES([Makefile])

if test "x$VTV_SUPPORTED" = "xyes"; then
  AC_CONFIG_FILES(AC_FOREACH([DIR], [testsuite], [DIR/Makefile ]), 
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

case "$target_os" in
  cygwin*|mingw32*)
    vtv_cygmin="yes"
    ;;
  *)
    vtv_cygmin="no"
    ;;
esac

AM_CONDITIONAL(VTV_CYGMIN, test $vtv_cygmin = yes)

AC_CHECK_FUNCS([_obstack_begin])
AM_CONDITIONAL(VTV_NO_OBSTACK, test $ac_cv_func__obstack_begin = no)

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_OUTPUT
