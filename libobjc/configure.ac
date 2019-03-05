# Process this file with autoconf to produce a configure script.
#   Copyright (C) 1994-2019 Free Software Foundation, Inc.
#   Originally contributed by Dave Love (d.love@dl.ac.uk).
#
#This file is part of GCC.
#
#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 3, or (at your option)
#any later version.
#
#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

AC_INIT(package-unused, version-unused,, libobjc)
AC_CONFIG_SRCDIR([objc/objc.h])
GCC_TOPLEV_SUBDIRS

# We need the following definitions because AC_PROG_LIBTOOL relies on them
PACKAGE=libobjc
# Version is pulled out to make it a bit easier to change using sed.
VERSION=4:0:0
AC_SUBST(VERSION)

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

# -------
# Options
# -------

# We use these options to decide which functions to include.
AC_ARG_WITH(target-subdir,
[  --with-target-subdir=SUBDIR
                           configuring in a subdirectory])
AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST  configuring with a cross compiler])

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

# -----------
# Directories
# -----------

# Find the rest of the source tree framework.
AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_SYSTEM
ACX_NONCANONICAL_TARGET

# Export source directory.
# These need to be absolute paths, yet at the same time need to
# canonicalize only relative paths, because then amd will not unmount
# drives. Thus the use of PWDCMD: set it to 'pawd' or 'amq -w' if using amd.
case $srcdir in
  [\\/$]* | ?:[\\/]*) glibcpp_srcdir=${srcdir} ;;
  *) glibcpp_srcdir=`cd "$srcdir" && ${PWDCMD-pwd} || echo "$srcdir"` ;;
esac
AC_SUBST(glibcpp_srcdir)

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

# Figure out if we want to name the include directory and the
#  library name changes differently.
includedirname=include
libsuffix=
case "${host}" in
  *-darwin*)
    # Darwin is the only target so far that needs a different include directory.
    includedirname=include-gnu-runtime
    libsuffix=-gnu
    ;;
esac
AC_SUBST(includedirname)
AC_SUBST(libsuffix)

AC_CONFIG_HEADERS(config.h)

# --------
# Programs
# --------

GCC_NO_EXECUTABLES

# We must force CC to /not/ be a precious variable; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.
m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

# extra LD Flags which are required for targets
ACX_LT_HOST_FLAGS
case "${host}" in
  *-darwin*)
    # Darwin needs -single_module when linking libobjc
    extra_ldflags_libobjc='$(lt_host_flags) -Wl,-single_module'
    ;;
  *-cygwin*|*-mingw*)
    # Tell libtool to build DLLs on Windows
    extra_ldflags_libobjc='$(lt_host_flags)'
    ;;
esac
AC_SUBST(extra_ldflags_libobjc)

# Add CET specific flags if CET is enabled
GCC_CET_FLAGS(CET_FLAGS)
XCFLAGS="$XCFLAGS $CET_FLAGS"

AC_SUBST(CFLAGS)
AC_SUBST(XCFLAGS)

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_PROG_INSTALL

AM_MAINTAINER_MODE

# Enable Win32 DLL on MS Windows - FIXME
AC_LIBTOOL_WIN32_DLL

AC_PROG_LIBTOOL

AM_PROG_CC_C_O

AC_PROG_MAKE_SET

# -------
# Headers
# -------

# Sanity check for the cross-compilation case:
AC_CHECK_HEADER(stdio.h,:,
  [AC_MSG_ERROR([Can't find stdio.h.
You must have a usable C system for the target already installed, at least
including headers and, preferably, the library, before you can configure
the Objective C runtime system.  If necessary, install gcc now with 
\`LANGUAGES=c', then the target library, then build with \`LANGUAGES=objc'.])])

AC_HEADER_STDC

AC_CHECK_HEADERS(sched.h)

# -----------
# Miscellanea
# -----------

# Check if we have thread-local storage
GCC_CHECK_TLS

gt_BITFIELD_TYPE_MATTERS

# -----------
# boehm-gc
# -----------

AC_ARG_ENABLE(objc-gc,
[AS_HELP_STRING([--enable-objc-gc],
		[enable use of Boehm's garbage collector with the
		 GNU Objective-C runtime])],,enable_objc_gc=no)
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
		  
case "$enable_objc_gc" in
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
    if test x$enable_objc_gc = xauto && test x$system_bdw_gc_found = xno; then
      AC_MSG_WARN([system bdw-gc not found, not building libobjc_gc])
      use_bdw_gc=no
    elif test x$enable_objc_gc = xyes && test x$system_bdw_gc_found = xno; then
      AC_MSG_ERROR([system bdw-gc required but not found])
    else
      use_bdw_gc=yes
      AC_MSG_RESULT([found])
    fi
  esac
esac

if test "$use_bdw_gc" = no; then
  OBJC_GCFLAGS=''
  OBJC_BOEHM_GC=''
  OBJC_BOEHM_GC_INCLUDES=''
  OBJC_BOEHM_GC_LIBS=''
else
  OBJC_GCFLAGS='-DOBJC_WITH_GC=1'
  OBJC_BOEHM_GC='libobjc_gc$(libsuffix).la'
  OBJC_BOEHM_GC_INCLUDES=$BDW_GC_CFLAGS
  OBJC_BOEHM_GC_LIBS=$BDW_GC_LIBS
fi
AC_SUBST(OBJC_GCFLAGS)
AC_SUBST(OBJC_BOEHM_GC)
AC_SUBST(OBJC_BOEHM_GC_INCLUDES)
AC_SUBST(OBJC_BOEHM_GC_LIBS)

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# ------
# Output
# ------

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
