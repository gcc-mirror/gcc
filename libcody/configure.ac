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
NMS_ENABLE_CHECKING
NMS_ENABLE_EXCEPTIONS

AC_CONFIG_HEADERS([config.h])
AC_CHECK_TOOL([AR],[ar])
AC_CHECK_PROG([DOXYGEN],[doxygen],[doxygen],[: NOTdoxygen])
AC_CHECK_PROG([ALOY],[aloy],[aloy],[: Joust testsuite missing])
AH_VERBATIM([_GNU_SOURCE],[#define _GNU_SOURCE 1])
AH_VERBATIM([_FORTIFY_SOURCE],[#undef _FORTIFY_SOURCE])

AC_OUTPUT
