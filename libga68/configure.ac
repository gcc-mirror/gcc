# Configure script for libga68.
# Copyright (C) 2025 Jose E. Marchesi.

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

# Process this file with autoreconf to produce a configure script.

AC_INIT(package-unused, version-unused,,libga68)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_HEADER(config.h)

# Add --enable-multilib to configure, which defaults to enable.
AM_ENABLE_MULTILIB(, ..)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Cope with compilers, in subsequent tests, that at this stage may not
# be able to build executables.
GCC_NO_EXECUTABLES

# Enable extensions to C or POSIX on hosts that normally disable the
# extensions.  This should be called before any macros that run the C
# compiler.
AC_USE_SYSTEM_EXTENSIONS

# Determine the system type and set output variabls to the names of
# the canonical system types.
#
# Do not delete or change the following two lines.  For why, and for
# why we are using the obsolete AC_CANONICAL_SYSTEM as ell as the
# recommended AC_CANONICAL_HOST, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)
AC_CANONICAL_HOST

# These macro are for supporting canadian crosses, in which build /=
# host /= target.
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

# Initialize Automake with proper options.
AM_INIT_AUTOMAKE([1.15.1 no-define foreign no-dist -Wall -Wno-portability])

# Tell Autoheader to include templates for PACKAGE and VERSION in
# config.h.in.
AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

# Command-line options.

# Enable large-file support.
AC_SYS_LARGEFILE

AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST           Configuring with a cross compiler])

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

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
toolexecdir=no
toolexeclibdir=no
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

# Add support for --enable-maintainer-mode={yes,no} to configure.
AM_MAINTAINER_MODE

# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.
m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AM_PROG_AS
AC_PROG_A68
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

# Initialize libtool.
LT_INIT
ACX_LT_HOST_FLAGS

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

# Do compilation tests using the C compiler and preprocessor.
AC_LANG([C])

# Allow the user to set CC_FOR_BUILD in the environment.
CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)

# Check whether the target supports hidden visibility.
LIBGA68_CHECK_ATTRIBUTE_VISIBILITY

# Search for needed functions in host libraries.
AC_SEARCH_LIBS([malloc], [c])
AC_SEARCH_LIBS([cosf], [m])

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Add dependencies for libga68.spec file
SPEC_LIBGA68_DEPS="$LIBS"
AC_SUBST(SPEC_LIBGA68_DEPS)

AC_MSG_CHECKING([whether symbol versioning is supported])
AC_ARG_ENABLE(symvers,
AS_HELP_STRING([--disable-symvers],
  [disable symbol versioning for libga68]),
ga68_use_symver=$enableval,
ga68_use_symver=yes)
if test "x$ga68_use_symver" != xno; then
  if test x$gcc_no_link = xyes; then
    # If we cannot link, we cannot build shared libraries, so do not use
    # symbol versioning.
    ga68_use_symver=no
  else
    save_LDFLAGS="$LDFLAGS"
    LDFLAGS="$LDFLAGS -fPIC -shared -Wl,--version-script,./conftest.map"
    cat > conftest.map <<EOF
FOO_1.0 {
  global: *foo*; bar; local: *;
};
EOF
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[int foo;]], [[]])],
		   [ga68_use_symver=gnu],[ga68_use_symver=no])
    if test x$ga68_use_symver = xno; then
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
          AC_LINK_IFELSE([AC_LANG_PROGRAM([[int foo;]], [[]])],
			 [ga68_use_symver=sun],[ga68_use_symver=no])
	  ;;
      esac
    fi
    LDFLAGS="$save_LDFLAGS"
  fi
fi

AC_MSG_RESULT($ga68_use_symver)
AM_CONDITIONAL(LIBGA68_USE_SYMVER, [test "x$ga68_use_symver" != xno])
AM_CONDITIONAL(LIBGA68_USE_SYMVER_GNU, [test "x$ga68_use_symver" = xgnu])
AM_CONDITIONAL(LIBGA68_USE_SYMVER_SUN, [test "x$ga68_use_symver" = xsun])

# The Boehm GC

AC_ARG_ENABLE(algol68-gc,
[AS_HELP_STRING([--enable-algol68-gc],
		[enable use of Boehm's garbage collector with the
		 GNU Algol runtime])],,enable_algol68_gc=auto)
AC_ARG_WITH([target-bdw-gc],
[AS_HELP_STRING([--with-target-bdw-gc=PATHLIST],
		[specify prefix directory for installed bdw-gc package.
		 Equivalent to --with-target-bdw-gc-include=PATH/include
		 plus --with-target-bdw-gc-lib=PATH/lib])])
AC_ARG_WITH([target-bdw-gc-include],
[AS_HELP_STRING([--with-target-bdw-gc-include=PATHLIST],
		[specify directories for installed bdw-gc include files])])
AC_ARG_WITH([target-bdw-gc-lib],
[AS_HELP_STRING([--with-target-bdw-gc-lib=PATHLIST],
		[specify directories for installed bdw-gc library])])

AC_ARG_ENABLE(algol68-gc-debug,
[AS_HELP_STRING([--enable-algol68-gc-debug],
		[enable use of Boehm's garbage collector debug functions
		 with the GNU Algol runtime])],,enable_algol68_gc_debug=no)

bdw_lib_dir=
case "$enable_algol68_gc" in
no)
  use_bdw_gc=no
  ;;
*)
  AC_MSG_CHECKING([for bdw garbage collector])
  if test "x$with_target_bdw_gc$with_target_bdw_gc_include$with_target_bdw_gc_lib" = x; then
    dnl no bdw-gw options, assuming bdw-gc in default locations
    BDW_GC_CFLAGS=
    BDW_GC_LIBS="-lgc"
  else
    dnl bdw-gw options passed by configure flags
    if test "x$with_target_bdw_gc_include" = x && test "x$with_target_bdw_gc_lib" != x; then
      AC_MSG_ERROR([found --with-target-bdw-gc-lib but --with-target-bdw-gc-include missing])
    elif test "x$with_target_bdw_gc_include" != x && test "x$with_target_bdw_gc_lib" = x; then
      AC_MSG_ERROR([found --with-target-bdw-gc-include but --with-target-bdw-gc-lib missing])
    else
      AC_MSG_RESULT([using paths configured with --with-target-bdw-gc options])
    fi
    mldir=`${CC-gcc} --print-multi-directory 2>/dev/null`
    bdw_val=
    if test "x$with_target_bdw_gc" != x; then
      for i in `echo $with_target_bdw_gc | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          bdw_val=$d
        fi
      done
      if test "x$bdw_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-bdw-gc])
      fi
      bdw_inc_dir="$bdw_val/include"
      bdw_lib_dir="$bdw_val/lib"
    fi
    bdw_val=
    if test "x$with_target_bdw_gc_include" != x; then
      for i in `echo $with_target_bdw_gc_include | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i; fallback=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          bdw_val=$d
        fi
      done
      if test "x$bdw_val" = x && test "x$bdw_inc_dir" = x && test "x$fallback" != x; then
        bdw_inc_dir="$fallback"
      elif test "x$bdw_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-bdw-gc-include])
      else
        bdw_inc_dir="$bdw_val"
      fi
    fi
    bdw_val=
    if test "x$with_target_bdw_gc_lib" != x; then
      for i in `echo $with_target_bdw_gc_lib | tr ',' ' '`; do
        case "$i" in
          *=*) sd=${i%%=*}; d=${i#*=} ;;
          *) sd=.; d=$i ;;
        esac
        if test "$mldir" = "$sd"; then
          bdw_val=$d
        fi
      done
      if test "x$bdw_val" = x; then
        AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-bdw-gc-lib])
      fi
      bdw_lib_dir="$bdw_val"
    fi
    if test "x$bdw_inc_dir" = x; then
      AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-bdw-gc-include])
    fi
    if test "x$bdw_lib_dir" = x; then
      AC_MSG_ERROR([no multilib path ($mldir) found in --with-target-bdw-gc-lib])
    fi
    BDW_GC_CFLAGS="-I$bdw_inc_dir"
    if test -f $bdw_lib_dir/libgc.la; then
      BDW_GC_LIBS="$bdw_lib_dir/libgc.la"
    else
      BDW_GC_LIBS="-L$bdw_lib_dir -lgc"
    fi
    AC_MSG_RESULT([found])
  fi

  case "$BDW_GC_LIBS" in
  *libgc.la)
    use_bdw_gc=yes
    ;;
  *)
    AC_MSG_CHECKING([for system boehm-gc])
    save_CFLAGS=$CFLAGS
    save_LIBS=$LIBS
    CFLAGS="$CFLAGS $BDW_GC_CFLAGS"
    LIBS="$LIBS $BDW_GC_LIBS"
    dnl the link test is not good enough for ARM32 multilib detection,
    dnl first check to link, then to run
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM([#include <gc/gc.h>],[GC_init()])],
      [
        AC_RUN_IFELSE([AC_LANG_SOURCE([[
          #include <gc/gc.h>
          int main() {
            GC_init();
            return 0;
          }
          ]])],
          [system_bdw_gc_found=yes],
          [system_bdw_gc_found=no],
          dnl assume no system boehm-gc for cross builds ...
          [system_bdw_gc_found=no]
        )
      ],
      [system_bdw_gc_found=no])
    CFLAGS=$save_CFLAGS
    LIBS=$save_LIBS
    if test x$enable_algol68_gc = xauto && test x$system_bdw_gc_found = xno; then
      AC_MSG_WARN([system bdw-gc not found, building libga68 with no GC support])
      use_bdw_gc=no
    elif test x$enable_algol68_gc = xyes && test x$system_bdw_gc_found = xno; then
      AC_MSG_ERROR([system bdw-gc required but not found])
    else
      use_bdw_gc=yes
      AC_MSG_RESULT([found])
    fi
  esac
esac

if test "$use_bdw_gc" = no; then
  LIBGA68_GCFLAGS=''
  LIBGA68_BOEHM_GC_INCLUDES=''
  LIBGA68_BOEHM_GC_LIBS=''
else
  LIBGA68_GCFLAGS='-DLIBGA68_WITH_GC=1'
  LIBGA68_BOEHM_GC_INCLUDES=$BDW_GC_CFLAGS
  LIBGA68_BOEHM_GC_LIBS=$BDW_GC_LIBS
  SPEC_LIBGA68_DEPS="$SPEC_LIBGA68_DEPS $BDW_GC_LIBS"
fi

extra_darwin_ldflags_libga68=
case $host in
  *-*-darwin*)
     extra_darwin_ldflags_libga68=-Wl,-U,___algol68_main
     if test -f $bdw_lib_dir/libgc.a; then
       # Darwin wants to link this statically into the library
       LIBGA68_BOEHM_GC_LIBS="$bdw_lib_dir/libgc.a"
       # No spec entry.
       BDW_GC_LIBS=
     fi
     ;;
  *) ;;
esac
AC_SUBST(extra_darwin_ldflags_libga68)

case "$enable_algol68_gc_debug" in
  no) ;;
  *)
    if test "$use_bdw_gc" = no; then
      AC_MSG_ERROR([GC debugging enabled but GC is not being used])
    else
      LIBGA68_GCFLAGS="$LIBGA68_GCFLAGS -DGC_DEBUG"
    fi
    ;;
esac

# Subst some variables used in Makefile.am
AC_SUBST(LIBGA68_GCFLAGS)
AC_SUBST(LIBGA68_BOEHM_GC_INCLUDES)
AC_SUBST(LIBGA68_BOEHM_GC_LIBS)

# Output files and be done.
AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES(libga68.spec)
AC_MSG_NOTICE([libga68 has been configured.])
AC_OUTPUT
