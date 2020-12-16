# CODYlib  -*- mode:autoconf -*-
# Copyright (C) 2020 Nathan Sidwell, nathan@acm.org
# License: Apache v2.0

AC_INIT([codylib],[0.0],[github.com/urnathan/libcody])
AC_CONFIG_SRCDIR(cody.hh)
m4_include(config.m4)

AC_CONFIG_AUX_DIR(build-aux)
AC_SUBST(PACKAGE_VERSION)

NMS_NOT_IN_SOURCE
AC_CANONICAL_HOST

NMS_TOOLS
NMS_NUM_CPUS
NMS_MAINTAINER_MODE
NMS_CXX_COMPILER
AC_LANG(C++)
AC_PROG_CXX
NMS_CXX_11
NMS_TOOL_DIRS
NMS_LINK_OPT([-Wl,--no-undefined])
NMS_CONFIG_FILES([gdbinit dox.cfg])

NMS_BUGURL
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
	# these set all the flags to specific states
	yes|all) ac_checking=1 ; ac_assert_checking=1 ; ac_valgrind_checking= ;;
	no|none) ac_checking= ; ac_assert_checking= ; ac_valgrind_checking= ;;
	release) ac_checking= ; ac_assert_checking=1 ; ac_valgrind_checking= ;;
	# these enable particular checks
	assert) ac_assert_checking=1 ;;
	misc) ac_checking=1 ;;
	valgrind) ac_valgrind_checking=1 ;;
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


NMS_ENABLE_EXCEPTIONS

AC_CONFIG_HEADERS([config.h])
AC_CHECK_TOOL([AR],[ar])
AC_CHECK_PROG([DOXYGEN],[doxygen],[doxygen],[: NOTdoxygen])
AC_CHECK_PROG([ALOY],[aloy],[aloy],[: Joust testsuite missing])
AH_VERBATIM([_GNU_SOURCE],[#define _GNU_SOURCE 1])
AH_VERBATIM([_FORTIFY_SOURCE],[#undef _FORTIFY_SOURCE])

AC_OUTPUT
