# CODYlib  -*- mode:autoconf -*-
# Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
# License: Apache v2.0

AC_INIT([codylib],[0.0],[github.com/urnathan/libcody])
AC_CONFIG_SRCDIR(cody.hh)
m4_include(config.m4)

AC_CONFIG_AUX_DIR(..)
AC_SUBST(PACKAGE_VERSION)

AC_CANONICAL_HOST

NMS_MAINTAINER_MODE
NMS_CXX_COMPILER
AC_LANG(C++)
AC_PROG_CXX
NMS_CXX_11
NMS_LINK_OPT([-Wl,--no-undefined])

# Enable expensive internal checks
is_release=
if test -d $srcdir/../gcc \
   && test -f $srcdir/../gcc/DEV-PHASE \
   && test x"`cat $srcdir/../gcc/DEV-PHASE`" != xexperimental; then
  is_release=yes
fi

dnl NMS_ENABLE_CHECKING
dnl cloned from ../libcpp/configure.ac
AC_ARG_ENABLE(checking,
[AS_HELP_STRING([[--enable-checking[=LIST]]],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: yes,no,all,none,release.
		 Flags are: misc,valgrind or other strings])],
[ac_checking_flags="${enableval}"],[
# Determine the default checks.
if test x$is_release = x ; then
  ac_checking_flags=yes
else
  ac_checking_flags=release
fi])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	yes|all|misc) ac_checking=1 ;;
	no|none|release) ac_checking= ;;
	# accept
	*) ;;
	esac
done
IFS="$ac_save_IFS"
                
if test x$ac_checking != x ; then
  AC_DEFINE(NMS_CHECKING, 1,
[Define to 1 if you want more run-time sanity checks.])
else
  AC_DEFINE(NMS_CHECKING, 0)
fi

# Enable --enable-host-shared.
AC_ARG_ENABLE(host-shared,
[AS_HELP_STRING([--enable-host-shared],
		[build host code as shared libraries])],
[PICFLAG=-fPIC], [PICFLAG=])
AC_SUBST(PICFLAG)

NMS_ENABLE_EXCEPTIONS

AC_PROG_RANLIB
AC_CHECK_TOOL([AR],[ar])
AH_VERBATIM([_GNU_SOURCE],[#define _GNU_SOURCE 1])
AH_VERBATIM([_FORTIFY_SOURCE],[#undef _FORTIFY_SOURCE])
AC_CONFIG_HEADERS([config.h])

AC_CONFIG_FILES([Makefile])
AC_SUBST(configure_args,[$ac_configure_args])

AC_OUTPUT
