#   Copyright (C) 2013 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

##############################################################################
### WARNING: this file contains embedded tabs.  Do not run untabify on this file.
# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_PREREQ(2.64)
AC_INIT(GCC-Rust-runtime, 0.1, http://gcc.gnu.org, libgrust,
		       https://github.com/redbrain/gccrs)
AC_CONFIG_SRCDIR(Makefile.am)
AC_CONFIG_HEADER(config.h)

libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}


AM_INIT_AUTOMAKE([1.9.3 no-define foreign no-dist -Wall -Wno-portability])
AH_TEMPLATE(PACKAGE, [GCC Rust Runtime])
AH_TEMPLATE(VERSION, [0.1])

m4_rename([_AC_ARG_VAR_PRECIOUS],[glibgrust_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([glibgrust_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AM_MAINTAINER_MODE

AC_PROG_LD
AC_CHECK_TOOL(OBJCOPY, objcopy, missing-objcopy)

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)

AC_PROG_AWK

WARN_FLAGS='-Wall -Wextra -Wwrite-strings -Wcast-qual'
AC_SUBST(WARN_FLAGS)

dnl FIXME: This should be controlled by --enable-maintainer-mode.
WERROR="-Werror"
AC_SUBST(WERROR)

# See if the user wants to configure without libffi.  Some
# architectures don't support it.  FIXME: We should set a default
# based on the host.
AC_ARG_WITH(libffi,
  AS_HELP_STRING([--without-libffi],
                 [don't use libffi]),
  [:],
  [with_libffi=${with_libffi_default-yes}])

LIBFFI=
LIBFFIINCS=
if test "$with_libffi" != no; then
   AC_DEFINE(USE_LIBFFI, 1, [Define if we're to use libffi.])
   LIBFFI=../libffi/libffi_convenience.la
   LIBFFIINCS='-I$(top_srcdir)/../libffi/include -I../libffi/include'
fi
AC_SUBST(LIBFFI)
AC_SUBST(LIBFFIINCS)

dnl Test for the -lm library.
MATH_LIBS=
AC_CHECK_LIB([m], [sqrt], MATH_LIBS=-lm)
AC_SUBST(MATH_LIBS)

dnl Test whether the compiler supports the -pthread option.
AC_CACHE_CHECK([whether -pthread is supported],
[libgpython_cv_lib_pthread],
[CFLAGS_hold=$CFLAGS
CFLAGS="$CFLAGS -pthread"
AC_COMPILE_IFELSE([[int i;]],
[libgpython_cv_lib_pthread=yes],
[libgpython_cv_lib_pthread=no])
CFLAGS=$CFLAGS_hold])
PTHREAD_CFLAGS=
if test "$libgpython_cv_lib_pthread" = yes; then
  PTHREAD_CFLAGS=-pthread
fi
AC_SUBST(PTHREAD_CFLAGS)

dnl Test for the -lpthread library.
PTHREAD_LIBS=
AC_CHECK_LIB([pthread], [pthread_create], PTHREAD_LIBS=-lpthread)
AC_SUBST(PTHREAD_LIBS)

AC_C_BIGENDIAN

GCC_CHECK_UNWIND_GETIPINFO

AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)

# Checks for header files.
AC_HEADER_STDBOOL
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h stdarg.h unistd.h \
   	      stdint.h stdio.h getopt.h \
	      assert.h sys/types.h signal.h \
	      fcntl.h pthread.h sys/wait.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_FUNCS([popen fopen fclose sprintf fprintf strdup \
		      strlen strcpy strcmp getopt_long \
	   	      memcpy calloc system sysconf atoi \
	   	      getpid execl fork wait exit atof \
	  	      vfprintf memcmp getc fgets pipe \
		      waitpid fdopen])

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_UTIME_NULL
AC_FUNC_VPRINTF

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
