# Process this file with autoconf to produce a configure script.
#   Copyright 1994, 1995, 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004
#   Free Software Foundation, Inc.
#   Originally contributed by Dave Love (d.love@dl.ac.uk).
#
#This file is part of GCC.
#
#GCC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2, or (at your option)
#any later version.
#
#GCC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING.  If not, write to
#the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#02111-1307, USA.

AC_PREREQ(2.59)
AC_INIT(package-unused, version-unused,, libobjc)
AC_CONFIG_SRCDIR([objc/objc.h])

# We need the following definitions because AC_PROG_LIBTOOL relies on them
PACKAGE=libobjc
# Version is pulled out to make it a bit easier to change using sed.
VERSION=1:0:0
AC_SUBST(VERSION)

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

# -------
# Options
# -------

# Default to --enable-multilib
AC_ARG_ENABLE(multilib,
  [  --enable-multilib       build hella library versions (default)],
  [case "${enableval}" in
    yes) multilib=yes ;;
    no)  multilib=no ;;
    *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
   esac],
  [multilib=yes])

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

AC_ARG_ENABLE(objc-gc,
[  --enable-objc-gc       enable the use of Boehm's garbage collector with
                          the GNU Objective-C runtime.],
[case $enable_objc_gc in
  no) OBJC_BOEHM_GC='' ;;
  *) OBJC_BOEHM_GC=libobjc_gc.la ;;
esac],
[OBJC_BOEHM_GC=''])
AC_SUBST(OBJC_BOEHM_GC)

# -----------
# Directories
# -----------

# When building with srcdir == objdir, links to the source files will
# be created in directories within the target_subdir.  We have to
# adjust toplevel_srcdir accordingly, so that configure finds
# install-sh and other auxiliary files that live in the top-level
# source directory.
if test "${srcdir}" = "."; then
  if test -z "${with_target_subdir}"; then
    toprel=".."
  else
    if test "${with_target_subdir}" != "."; then
      toprel="${with_multisrctop}../.."
    else
      toprel="${with_multisrctop}.."
    fi
  fi
else
  toprel=".."
fi
AC_CONFIG_AUX_DIR(${srcdir}/$toprel)
toplevel_srcdir=\${srcdir}/$toprel
AC_SUBST(toplevel_srcdir)

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

TL_AC_GCC_VERSION([$srcdir/..])

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexeclibdir='$(toolexecdir)/'${gcc_version}'$(MULTISUBDIR)'
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
libext=
case "${host}" in
  *-darwin*)
    # Darwin is the only target so far that needs a different include directory.
    includedirname=include-gnu-runtime
    libext=-gnu
    ;;
esac
AC_SUBST(includedirname)
AC_SUBST(libext)

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
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

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

# Determine CFLAGS for gthread.
# FIXME: the current implementation is dependent on the 'r' variable
# passed down from the top level
AC_CACHE_CHECK([for gthread cflags],objc_cv_gthread_flags,
[if test -f "$r"/gcc/Makefile
then
  objc_cv_gthread_flags=`grep \^GTHREAD_FLAGS "$r"/gcc/Makefile | awk -F= '{ print $2 }'`
else
  AC_MSG_ERROR([not found])
fi])
GTHREAD_FLAGS=$objc_cv_gthread_flags
AC_SUBST(GTHREAD_FLAGS)

AC_MSG_CHECKING([for exception model to use])
AC_LANG_PUSH(C)
AC_ARG_ENABLE(sjlj-exceptions,
  AS_HELP_STRING([--enable-sjlj-exceptions],
                 [force use of builtin_setjmp for exceptions]),
[:],
[dnl Botheration.  Now we've got to detect the exception model.
dnl Link tests against libgcc.a are problematic since -- at least
dnl as of this writing -- we've not been given proper -L bits for
dnl single-tree newlib and libgloss.
dnl
dnl This is what AC_TRY_COMPILE would do if it didn't delete the
dnl conftest files before we got a change to grep them first.
cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
@interface Frob
@end
@implementation Frob
@end
int proc();
int foo()
{
  @try {
   return proc();
  }
  @catch (Frob* ex) {
    return 0;
  }
}
EOF
old_CFLAGS="$CFLAGS" 
dnl work around that we don't have Objective-C support in autoconf
CFLAGS="-x objective-c -fgnu-runtime -fobjc-exceptions -S"
if AC_TRY_EVAL(ac_compile); then
  if grep _Unwind_SjLj_Resume conftest.s >/dev/null 2>&1 ; then
    enable_sjlj_exceptions=yes
  elif grep _Unwind_Resume conftest.s >/dev/null 2>&1 ; then
    enable_sjlj_exceptions=no
  fi
fi
CFLAGS="$old_CFLAGS"
rm -f conftest*])
if test x$enable_sjlj_exceptions = xyes; then
  AC_DEFINE(SJLJ_EXCEPTIONS, 1,
	[Define if the compiler is configured for setjmp/longjmp exceptions.])
  ac_exception_model_name=sjlj
elif test x$enable_sjlj_exceptions = xno; then
  ac_exception_model_name="call frame"
else
  AC_MSG_ERROR([unable to detect exception model])
fi
AC_LANG_POP(C)
AC_MSG_RESULT($ac_exception_model_name)

# ------
# Output
# ------

AC_CONFIG_FILES([Makefile])

AC_CONFIG_COMMANDS([default],
[[if test -n "$CONFIG_FILES"; then
  if test -n "${with_target_subdir}"; then
    # FIXME: We shouldn't need to set ac_file
    ac_file=Makefile
    LD="${ORIGINAL_LD_FOR_MULTILIBS}"
    . ${toplevel_srcdir}/config-ml.in
  fi
fi]],
[[srcdir=${srcdir}
host=${host}
target=${target}
with_target_subdir=${with_target_subdir}
with_multisubdir=${with_multisubdir}
ac_configure_args="--enable-multilib ${ac_configure_args}"
toplevel_srcdir=${toplevel_srcdir}
CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
ORIGINAL_LD_FOR_MULTILIBS="${ORIGINAL_LD_FOR_MULTILIBS}"
]])

AC_OUTPUT
