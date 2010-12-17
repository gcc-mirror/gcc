AC_PREREQ(2.64)
AC_INIT([LTO plugin for ld], 0.1,,[lto-plugin])
AC_CANONICAL_SYSTEM
GCC_TOPLEV_SUBDIRS
AM_INIT_AUTOMAKE([foreign no-dist])
AM_MAINTAINER_MODE
AC_PROG_CC
AC_SYS_LARGEFILE
AM_PROG_LIBTOOL
AC_SUBST(target_noncanonical)
# Trying to get this information from gcc's config is tricky.
case $target in
  x86_64*-mingw*)
    AC_DEFINE([SYM_STYLE], [ss_none], [Default symbol style])
    ;;
  *-cygwin* | i?86*-mingw* )
    AC_DEFINE([SYM_STYLE], [ss_win32], [Default symbol style])
    ;;
  *)
    AC_DEFINE([SYM_STYLE], [ss_none], [Default symbol style])
    ;;
esac
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_CONFIG_FILES(Makefile)
AC_CONFIG_HEADERS(config.h)
AC_OUTPUT
