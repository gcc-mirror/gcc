dnl aclocal.m4 generated automatically by aclocal 1.4

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

AC_DEFUN(LIBGCJ_CONFIGURE,
[
dnl Default to --enable-multilib
AC_ARG_ENABLE(multilib,
[  --enable-multilib       build many library versions (default)],
[case "${enableval}" in
  yes) multilib=yes ;;
  no)  multilib=no ;;
  *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
 esac], [multilib=yes])dnl

dnl We may get other options which we dont document:
dnl --with-target-subdir, --with-multisrctop, --with-multisubdir

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

libgcj_basedir=$srcdir/$toprel/$1/libjava
AC_SUBST(libgcj_basedir)

AC_CONFIG_AUX_DIR(${srcdir}/$toprel)
if :; then :; else
  # This overrides the previous occurrence for automake, but not for
  # autoconf, which is exactly what we want.
  AC_CONFIG_AUX_DIR(..)
fi

# This works around an automake problem.
mkinstalldirs="`cd $ac_aux_dir && pwd`/mkinstalldirs"
AC_SUBST(mkinstalldirs)

AC_CANONICAL_SYSTEM

dnl This shouldn't be needed, as long as top-level dependencies are
dnl defined correctly and shared-library paths are set up so that
dnl execution tests succeed.  FIXME.
define([AC_PROG_CC_WORKS],[])
define([AC_PROG_CXX_WORKS],[])

AC_PROG_CC
AC_PROG_CXX

dnl version is pulled out to make it a bit easier to change using sed.
version=0.0.7
dnl Still use "libjava" here to placate dejagnu.
AM_INIT_AUTOMAKE(libjava, $version)

# AC_CHECK_TOOL does AC_REQUIRE (AC_CANONICAL_BUILD).  If we dont
# run it explicitly here, it will be run implicitly before
# LIBGCJ_CONFIGURE, which doesn't work because that means that it will
# be run before AC_CANONICAL_HOST.
AC_CANONICAL_BUILD

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

# We need AC_EXEEXT to keep automake happy in cygnus mode.  However,
# at least currently, we never actually build a program, so we never
# need to use $(EXEEXT).  Moreover, the test for EXEEXT normally
# fails, because we are probably configuring with a cross compiler
# which cant create executables.  So we include AC_EXEEXT to keep
# automake happy, but we dont execute it, since we dont care about
# the result.
if false; then
  # autoconf 2.50 runs AC_EXEEXT by default, and the macro expands
  # to nothing, so nothing would remain between `then' and `fi' if it
  # were not for the `:' below.
  :
  AC_EXEEXT
fi

# configure.host sets the following important variables
#	libgcj_cflags    - host specific C compiler flags
#	libgcj_cxxflags  - host specific C++ compiler flags
#	libgcj_javaflags - host specific Java compiler flags

libgcj_cflags=
libgcj_cxxflags=
libgcj_javaflags=

. [$]{srcdir}/configure.host

LIBGCJ_CFLAGS="[$]{libgcj_cflags}"
LIBGCJ_CXXFLAGS="[$]{libgcj_cxxflags}"
LIBGCJ_JAVAFLAGS="[$]{libgcj_javaflags}"
AC_SUBST(LIBGCJ_CFLAGS)
AC_SUBST(LIBGCJ_CXXFLAGS)
AC_SUBST(LIBGCJ_JAVAFLAGS)
])dnl

sinclude(../libtool.m4)
dnl The lines below arrange for aclocal not to bring libtool.m4
dnl AM_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[
AC_DEFUN([AM_PROG_LIBTOOL],)
AC_DEFUN([AC_LIBTOOL_DLOPEN],)
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],)
AC_DEFUN([LT_AC_PROG_GCJ],)
AC_SUBST(GCJ)
AC_SUBST(LIBTOOL)
])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" && test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
dnl FIXME This is truly gross.
missing_dir=`cd $ac_aux_dir && pwd`
AM_MISSING_PROG(ACLOCAL, aclocal, $missing_dir)
AM_MISSING_PROG(AUTOCONF, autoconf, $missing_dir)
AM_MISSING_PROG(AUTOMAKE, automake, $missing_dir)
AM_MISSING_PROG(AUTOHEADER, autoheader, $missing_dir)
AM_MISSING_PROG(MAKEINFO, makeinfo, $missing_dir)
AC_REQUIRE([AC_PROG_MAKE_SET])])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM, DIRECTORY)
dnl The program must properly implement --version.
AC_DEFUN(AM_MISSING_PROG,
[AC_MSG_CHECKING(for working $2)
# Run test in a subshell; some versions of sh will print an error if
# an executable is not found, even if stderr is redirected.
# Redirect stdin to placate older versions of autoconf.  Sigh.
if ($2 --version) < /dev/null > /dev/null 2>&1; then
   $1=$2
   AC_MSG_RESULT(found)
else
   $1="$3/missing $2"
   AC_MSG_RESULT(missing)
fi
AC_SUBST($1)])

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# serial 1

AC_DEFUN(AM_MAINTAINER_MODE,
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT($USE_MAINTAINER_MODE)
  AM_CONDITIONAL(MAINTAINER_MODE, test $USE_MAINTAINER_MODE = yes)
  MAINT=$MAINTAINER_MODE_TRUE
  AC_SUBST(MAINT)dnl
]
)

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])

