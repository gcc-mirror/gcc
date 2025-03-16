# Configure script for libgcobol.
# Adapted by James K. Lowden from configure script for libalg68.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

# Configure looks for the existence of this file to auto-config each language.
# We define several parameters used by configure:

# Process this file with autoreconf to produce a configure script.

AC_INIT(package-unused, version-unused,,libgcobol)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_HEADER(config.h)

AM_ENABLE_MULTILIB(, ..)

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

GCC_NO_EXECUTABLES

AC_USE_SYSTEM_EXTENSIONS

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_INIT_AUTOMAKE # ([1.15.1 no-define foreign no-dist -Wall -Wno-portability])

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST           Configuring with a cross compiler])

# Checks for header files.
AC_CHECK_HEADERS(malloc.h)

AC_CANONICAL_HOST
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

AC_CHECK_SIZEOF([void *])

# Get target configury.
unset LIBGCOBOL_SUPPORTED
. ${srcdir}/configure.tgt

# -----------------
# __int128 support
# -----------------

AC_CACHE_CHECK([whether __int128 is supported], [libgcobol_cv_have_int128],
  [GCC_TRY_COMPILE_OR_LINK([
    __int128 foo (__int128 )
    {
    __int128 aaa;
     return (__int128) aaa;
    }

    __int128 bar (__int128 )
    {
    __int128 aaa;
     return (__int128) aaa;
    }
  ],[
    foo (1);
    bar (1);
  ],[
    libgcobol_cv_have_int128=yes
  ],[
    libgcobol_cv_have_int128=no
])])
# The following conditional is useful when this creates a Makefile.am file that
# is subsequently processed into a Makefile.in file.  At the present time,
# however the libgcobol build uses a hardcoded Makefile.in file.
AM_CONDITIONAL(BUILD_LIBGCOBOL, [test "x$LIBGCOBOL_SUPPORTED" = xyes && test "x$libgcobol_cv_have_int128" = xyes])

GCC_WITH_TOOLEXECLIBDIR

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

toolexecdir=no
toolexeclibdir=no

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

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AM_MAINTAINER_MODE

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

# In order to override CFLAGS_FOR_TARGET, all of our special flags go
# in XCFLAGS.  But we need them in CFLAGS during configury.  So put them
# in both places for now and restore CFLAGS at the end of config.
save_CFLAGS="$CFLAGS"

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(NM, nm)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

AM_PROG_LIBTOOL
LT_INIT

AC_LIBTOOL_DLOPEN

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

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

CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)

AC_SEARCH_LIBS([malloc], [c])
AC_SEARCH_LIBS([cosf], [m])
AC_SEARCH_LIBS([clock_gettime], [rt])

# Add dependencies for libgcobol.spec file
SPEC_LIBGCOBOL_DEPS="$LIBS"
AC_SUBST(SPEC_LIBGCOBOL_DEPS)

# libgcobol soname version
LIBGCOBOL_VERSION=1:0:0
AC_SUBST(LIBGCOBOL_VERSION)

## added
VERSION_SUFFIX=$(echo $LIBGCOBOL_VERSION | tr  ':' '.' )
AC_SUBST(VERSION_SUFFIX)
## end added

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

extra_darwin_ldflags_libgcobol=
case $host in
  *-*-darwin*)
    extra_darwin_ldflags_libgcobol=-Wl,-U,___cobol_main ;;
  *) ;;
esac
AC_SUBST(extra_darwin_ldflags_libgcobol)

# For iconv support.
AM_ICONV

AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_FILES([Makefile])
####AC_CONFIG_FILES(libgcobol.spec)

AC_MSG_NOTICE([libgcobol has been configured.])

AC_OUTPUT
