# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(LIBGCJ_CONFIGURE,
[
dnl Default to --enable-multilib
AC_ARG_ENABLE(multilib,
[  --enable-multilib         build many library versions (default)],
[case "${enableval}" in
  yes) multilib=yes ;;
  no)  multilib=no ;;
  *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
 esac], [multilib=yes])dnl

dnl We may get other options which we dont document:
dnl --with-target-subdir, --with-multisrctop, --with-multisubdir

if test "[$]{srcdir}" = "."; then
  if test "[$]{with_target_subdir}" != "."; then
    libgcj_basedir="[$]{srcdir}/[$]{with_multisrctop}../$1"
  else
    libgcj_basedir="[$]{srcdir}/[$]{with_multisrctop}$1"
  fi
else
  libgcj_basedir="[$]{srcdir}/$1"
fi
AC_SUBST(libgcj_basedir)

AC_CANONICAL_HOST

dnl version is pulled out to make it a bit easier to change using sed.
version=0.0.7
dnl Still use "libjava" here to placate dejagnu.
AM_INIT_AUTOMAKE(libjava, $version)

# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(LIB_AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])dnl
AC_CHECK_PROG(CC, gcc, gcc)
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
  test -z "$CC" && AC_MSG_ERROR([no acceptable cc found in \$PATH])
fi

AC_PROG_CC_GNU

if test $ac_cv_prog_gcc = yes; then
  GCC=yes
dnl Check whether -g works, even if CFLAGS is set, in case the package
dnl plays around with CFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CFLAGS="${CFLAGS+set}"
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=
  AC_PROG_CC_G
  if test "$ac_test_CFLAGS" = set; then
    CFLAGS="$ac_save_CFLAGS"
  elif test $ac_cv_prog_cc_g = yes; then
    CFLAGS="-g -O2"
  else
    CFLAGS="-O2"
  fi
else
  GCC=
  test "${CFLAGS+set}" = set || CFLAGS="-g"
fi
])

LIB_AC_PROG_CC

# Likewise for AC_PROG_CXX.
AC_DEFUN(LIB_AC_PROG_CXX,
[AC_BEFORE([$0], [AC_PROG_CXXCPP])dnl
AC_CHECK_PROGS(CXX, $CCC c++ g++ gcc CC cxx cc++, gcc)
test -z "$CXX" && AC_MSG_ERROR([no acceptable c++ found in \$PATH])

AC_PROG_CXX_GNU

if test $ac_cv_prog_gxx = yes; then
  GXX=yes
dnl Check whether -g works, even if CXXFLAGS is set, in case the package
dnl plays around with CXXFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS=
  AC_PROG_CXX_G
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  elif test $ac_cv_prog_cxx_g = yes; then
    CXXFLAGS="-g -O2"
  else
    CXXFLAGS="-O2"
  fi
else
  GXX=
  test "${CXXFLAGS+set}" = set || CXXFLAGS="-g"
fi
])

LIB_AC_PROG_CXX

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
  AC_EXEEXT
fi

# configure.host sets the following important variables
#	libgcj_cflags    - host specific C compiler flags
#	libgcj_cxxflags  - host specific C++ compiler flags
#	libgcj_javaflags - host specific Java compiler flags

libgcj_cflags=
libgcj_cxxflags=
libgcj_javaflags=

. [$]{libgcj_basedir}/configure.host

case [$]{libgcj_basedir} in
/* | [A-Za-z]:[/\\]*) libgcj_flagbasedir=[$]{libgcj_basedir} ;;
*) libgcj_flagbasedir='[$](top_builddir)/'[$]{libgcj_basedir} ;;
esac

LIBGCJ_CFLAGS="[$]{libgcj_cflags}"
LIBGCJ_CXXFLAGS="[$]{libgcj_cxxflags}"
LIBGCJ_JAVAFLAGS="[$]{libgcj_javaflags}"
AC_SUBST(LIBGCJ_CFLAGS)
AC_SUBST(LIBGCJ_CXXFLAGS)
AC_SUBST(LIBGCJ_JAVAFLAGS)
])dnl
