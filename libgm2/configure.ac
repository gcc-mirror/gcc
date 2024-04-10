# Configure script for libgm2.
# Copyright (C) 2013-2023 Free Software Foundation, Inc.

# This file is part of GCC.

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

# Configure looks for the existence of this file to auto-config each language.
# We define several parameters used by configure:

# Process this file with autoreconf to produce a configure script.

AC_INIT(package-unused, version-unused,,libgm2)
AC_CONFIG_SRCDIR(Makefile.am)
# AC_CONFIG_MACRO_DIR([config])
AC_CONFIG_HEADER(config.h)

libtool_VERSION=19:0:0
AC_SUBST(libtool_VERSION)

AM_ENABLE_MULTILIB(, ..)

GCC_NO_EXECUTABLES

AC_USE_SYSTEM_EXTENSIONS

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

if test "$build" != "$host"; then
  # We are being configured with some form of cross compiler.
  GLIBCXX_IS_NATIVE=false
  case "$host","$target" in
    # Darwin crosses can use the host system's libraries and headers,
    # because of the fat library support.  Of course, it must be the
    # same version of Darwin on both sides.  Allow the user to
    # just say --target=foo-darwin without a version number to mean
    # "the version on this system".
      *-*-darwin*,*-*-darwin*)
	hostos=`echo $host | sed 's/.*-darwin/darwin/'`
	targetos=`echo $target | sed 's/.*-darwin/darwin/'`
	if test $hostos = $targetos || test $targetos = darwin ; then
	  GLIBCXX_IS_NATIVE=true
	fi
	;;

      *)
	GCC_NO_EXECUTABLES
	;;
  esac
else
  GLIBCXX_IS_NATIVE=true
fi

# Runs configure.host, and assorted other critical bits.  Sets
# up critical shell variables.
GLIBCXX_CONFIGURE

# For gettimeofday support.
GLIBCXX_CHECK_GETTIMEOFDAY

# For clock_gettime, nanosleep and sched_yield support.
GLIBCXX_ENABLE_LIBSTDCXX_TIME

AM_INIT_AUTOMAKE([1.15.1 no-define foreign no-dist -Wall -Wno-portability])

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AC_ARG_WITH(cross-host,
[  --with-cross-host=HOST           Configuring with a cross compiler])

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADER([math.h],
	       [AC_DEFINE([HAVE_MATH_H], [1], [have math.h])])

AC_CHECK_HEADERS(getopt.h limits.h stddef.h string.h strings.h \
                 stdlib.h time.h \
		 fcntl.h unistd.h sys/file.h sys/time.h sys/mman.h \
		 sys/resource.h sys/param.h sys/times.h sys/stat.h \
                 sys/socket.h \
                 sys/wait.h sys/ioctl.h errno.h sys/errno.h \
		 pwd.h direct.h dirent.h signal.h malloc.h langinfo.h \
                 pthread.h stdarg.h stdio.h sys/types.h termios.h \
                 netinet/in.h netdb.h sys/uio.h sys/stat.h wchar.h)

AC_CANONICAL_HOST
ACX_NONCANONICAL_HOST
ACX_NONCANONICAL_TARGET
GCC_TOPLEV_SUBDIRS

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

# Command-line options.
# Very limited version of AC_MAINTAINER_MODE.
AC_ARG_ENABLE([maintainer-mode],
  [AC_HELP_STRING([--enable-maintainer-mode],
                 [enable make rules and dependencies not useful (and
                  sometimes confusing) to the casual installer])],
  [case ${enable_maintainer_mode} in
     yes) MAINT='' ;;
     no) MAINT='#' ;;
     *) AC_MSG_ERROR([--enable-maintainer-mode must be yes or no]) ;;
   esac
   maintainer_mode=${enableval}],
  [MAINT='#'])
AC_SUBST([MAINT])dnl

toolexecdir=no
toolexeclibdir=no

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

AH_TEMPLATE(PACKAGE, [Name of package])
AH_TEMPLATE(VERSION, [Version number of package])

AM_MAINTAINER_MODE

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS
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

AM_PROG_LIBTOOL
LT_INIT
AC_LIBTOOL_DLOPEN

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CHECK_TYPES([struct timezone, struct stat, struct timespec, struct timeval, struct tm])

# Check if struct tm contains the tm_gmtoff field.
AC_CACHE_CHECK(for tm_gmtoff in struct tm, ac_cv_struct_tm_gmtoff,
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
      #include <time.h>
    ], [
      struct tm tm;
      tm.tm_gmtoff = 1;
    ])],
    [ac_cv_struct_tm_gmtoff=yes],
    [ac_cv_struct_tm_gmtoff=no]
  )
)

if (test "$ac_cv_struct_tm_gmtoff" = "yes"); then
  AC_MSG_CHECKING([m2 front end detects struct tm with the tm_gmtoff field.])
  AC_DEFINE(HAVE_TM_TM_GMTOFF, 1, [Define if struct tm has a tm_gmtoff field.])
fi

AC_LANG_C
# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AC_DEFUN([GM2_UNDEF],[
  $as_echo "#undef HAVE_$1" >>confdefs.h
])

AC_DEFUN([GM2_CHECK_LIB],[
  AC_MSG_CHECKING([m2 front end checking $1 library for $2])
  if test x$gcc_no_link != xyes; then
    AC_CHECK_LIB([$1],[$2],[AC_DEFINE([HAVE_$3],[1],[found $2])],[GM2_UNDEF([$3],[$2])])
  else
    if test "x$[ac_cv_lib_$1_$2]" = xyes; then
      AC_DEFINE([HAVE_$3],[1],[lib$1 includes $2])
    elif test "x$[ac_cv_func_$2]" = xyes; then
      AC_DEFINE([HAVE_$3],[1],[function $2 exists])
    else
      GM2_UNDEF([$3],[$2])
    fi
  fi
])

GM2_CHECK_LIB([c],[access],[ACCESS])
GM2_CHECK_LIB([c],[brk],[BRK])
GM2_CHECK_LIB([c],[cfmakeraw],[CFMAKERAW])
GM2_CHECK_LIB([c],[clock_gettime],[CLOCK_GETTIME])
GM2_CHECK_LIB([c],[clock_settime],[CLOCK_SETTIME])
GM2_CHECK_LIB([c],[close],[CLOSE])
GM2_CHECK_LIB([c],[ctime],[CTIME])
GM2_CHECK_LIB([c],[creat],[CREAT])
GM2_CHECK_LIB([c],[daylight],[DAYLIGHT])
GM2_CHECK_LIB([c],[dup],[DUP])
GM2_CHECK_LIB([c],[execve],[EXECVE])
GM2_CHECK_LIB([c],[exit],[EXIT])
GM2_CHECK_LIB([c],[fcntl],[FCNTL])
GM2_CHECK_LIB([c],[fstat],[FSTAT])
GM2_CHECK_LIB([c],[getdents],[GETDENTS])
GM2_CHECK_LIB([c],[getgid],[GETGID])
GM2_CHECK_LIB([c],[getopt_long],[GETOPT_LONG])
GM2_CHECK_LIB([c],[getopt_long_only],[GETOPT_LONG_ONLY])
GM2_CHECK_LIB([c],[getpid],[GETPID])
GM2_CHECK_LIB([c],[gettimeofday],[GETTIMEOFDAY])
GM2_CHECK_LIB([c],[getuid],[GETUID])
GM2_CHECK_LIB([c],[ioctl],[IOCTL])
GM2_CHECK_LIB([c],[kill],[KILL])
GM2_CHECK_LIB([c],[link],[LINK])
GM2_CHECK_LIB([c],[lseek],[LSEEK])
GM2_CHECK_LIB([c],[open],[OPEN])
GM2_CHECK_LIB([c],[pause],[PAUSE])
GM2_CHECK_LIB([c],[pipe],[PIPE])
GM2_CHECK_LIB([c],[rand],[RAND])
GM2_CHECK_LIB([c],[read],[READ])
GM2_CHECK_LIB([c],[select],[SELECT])
GM2_CHECK_LIB([c],[setitimer],[SETITIMER])
GM2_CHECK_LIB([c],[setgid],[SETGID])
GM2_CHECK_LIB([c],[settimeofday],[SETTIMEOFDAY])
GM2_CHECK_LIB([c],[setuid],[SETUID])
GM2_CHECK_LIB([c],[stat],[STAT])
GM2_CHECK_LIB([c],[strsignal],[STRSIGNAL])
GM2_CHECK_LIB([c],[strtod],[STRTOD])
GM2_CHECK_LIB([c],[strtold],[STRTOLD])
GM2_CHECK_LIB([c],[times],[TIMES])
GM2_CHECK_LIB([c],[timezone],[TIMEZONE])
GM2_CHECK_LIB([c],[tzname],[TZNAME])
GM2_CHECK_LIB([c],[unlink],[UNLINK])
GM2_CHECK_LIB([c],[wait],[WAIT])
GM2_CHECK_LIB([c],[write],[WRITE])

GM2_CHECK_LIB([m],[exp10],[EXP10])
GM2_CHECK_LIB([m],[exp10f],[EXP10F])
GM2_CHECK_LIB([m],[exp10l],[EXP10L])
GM2_CHECK_LIB([m],[signbit],[SIGNBIT])
GM2_CHECK_LIB([m],[signbitf],[SIGNBITF])
GM2_CHECK_LIB([m],[signbitl],[SIGNBITL])

AC_CHECK_DECLS([getenv])

AC_MSG_NOTICE([libgm2 has finished checking target libc and libm contents.])

# We test the host here and later on check the target.

# All known M2_HOST_OS values.  This is the union of all host operating systems
# supported by gm2.

M2_SUPPORTED_HOST_OS="aix freebsd hurd linux netbsd openbsd solaris windows darwin"

M2_HOST_OS=unknown

case ${host} in
  *-*-darwin*)   M2_HOST_OS=darwin ;;
  *-*-freebsd*)  M2_HOST_OS=freebsd ;;
  *-*-linux*)    M2_HOST_OS=linux ;;
  *-*-netbsd*)	 M2_HOST_OS=netbsd ;;
  *-*-openbsd*)  M2_HOST_OS=openbsd ;;
  *-*-solaris2*) M2_HOST_OS=solaris ;;
  *-*-aix*)      M2_HOST_OS=aix ;;
  *-*-gnu*)      M2_HOST_OS=hurd ;;
esac

M2_TARGET_OS=unknown

case ${target} in
  *-*-darwin*)   M2_TARGET_OS=darwin ;;
  *-*-freebsd*)  M2_TARGET_OS=freebsd ;;
  *-*-linux*)    M2_TARGET_OS=linux ;;
  *-*-netbsd*)	 M2_TARGET_OS=netbsd ;;
  *-*-openbsd*)  M2_TARGET_OS=openbsd ;;
  *-*-solaris2*) M2_TARGET_OS=solaris ;;
  *-*-aix*)      M2_TARGET_OS=aix ;;
  *-*-gnu*)      M2_TARGET_OS=hurd ;;
esac

# M2_HOST_OS=unknown
if test x${M2_HOST_OS} = xunknown; then
   AC_MSG_NOTICE([unsupported host, will build a minimal m2 library])
   BUILD_PIMLIB=false
   BUILD_ISOLIB=false
   BUILD_CORLIB=false
   BUILD_LOGLIB=false
else
   AC_MSG_NOTICE([m2 library will be built on ${M2_HOST_OS}])
   BUILD_PIMLIB=true
   BUILD_ISOLIB=true
   BUILD_CORLIB=true
   BUILD_LOGLIB=true
fi

CC_FOR_BUILD=${CC_FOR_BUILD:-gcc}
AC_SUBST(CC_FOR_BUILD)

# Propagate GM2_FOR_TARGET into Makefiles
GM2_FOR_TARGET=${GM2_FOR_TARGET:-gcc}
AC_SUBST(GM2_FOR_TARGET)

# Now we check the target as long as it is a supported host.
# For some embedded targets we choose minimal runtime system which is
# just enough to satisfy the linker targetting raw metal.
if test x${M2_HOST_OS} != xunknown; then
AC_MSG_NOTICE([m2 library building for target ${target}])
case "$target" in

  avr25*-*-* | avr31*-*-* | avr35*-*-* | avr4*-*-* | avr5*-*-* | avr51*-*-* | avr6*-*-*)
  BUILD_PIMLIB=false
  BUILD_ISOLIB=false
  BUILD_CORLIB=false
  BUILD_LOGLIB=false
  ;;

  avrxmega2*-*-* | avrxmega4*-*-* | avrxmega5*-*-* | avrxmega6*-*-* | avrxmega7*-*-*)
  BUILD_PIMLIB=false
  BUILD_ISOLIB=false
  BUILD_CORLIB=false
  BUILD_LOGLIB=false
  ;;

  avr3-*-*)
  BUILD_PIMLIB=true
  BUILD_ISOLIB=true
  BUILD_CORLIB=true
  BUILD_LOGLIB=true
  ;;
  esp32-*-*)
  BUILD_PIMLIB=false
  BUILD_ISOLIB=false
  BUILD_CORLIB=false
  BUILD_LOGLIB=false
  ;;

esac
fi

# GM2_MSG_RESULT issue a query message from the first parameter and a boolean result
# in the second parameter is printed as a "yes" or "no".

AC_DEFUN([GM2_MSG_RESULT],[
   AC_MSG_CHECKING([$1])
   if test x${$2} = xtrue; then
      AC_MSG_RESULT([yes])
   else
      AC_MSG_RESULT([no])
   fi
])

if test x${M2_HOST_OS} = xunknown; then
   AC_MSG_NOTICE([m2 front end will only build minimal Modula-2 runtime library on this host])
else
   GM2_MSG_RESULT([m2 front end will build PIM libraries:],[BUILD_PIMLIB])
   GM2_MSG_RESULT([m2 front end will build ISO libraries:],[BUILD_ISOLIB])
   GM2_MSG_RESULT([m2 front end will build coroutine libraries:],[BUILD_CORLIB])
   GM2_MSG_RESULT([m2 front end will build Logitech compatability libraries:],[BUILD_LOGLIB])
fi

AM_CONDITIONAL([BUILD_PIMLIB], [test x$BUILD_PIMLIB = xtrue])
AM_CONDITIONAL([BUILD_ISOLIB], [test x$BUILD_ISOLIB = xtrue])
AM_CONDITIONAL([BUILD_CORLIB], [test x$BUILD_CORLIB = xtrue])
AM_CONDITIONAL([BUILD_LOGLIB], [test x$BUILD_LOGLIB = xtrue])
AM_CONDITIONAL([TARGET_DARWIN], [test x$M2_TARGET_OS = xdarwin])

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_CONFIG_SRCDIR([Makefile.am])
AC_CONFIG_FILES([Makefile])

AC_CONFIG_FILES(AC_FOREACH([DIR], [libm2min libm2pim libm2iso libm2cor libm2log], [DIR/Makefile ]),
 [ cat > vpsed$$ << \_EOF
s!`test -f '$<' || echo '$(srcdir)/'`!!
_EOF
   sed -f vpsed$$ $ac_file > tmp$$
   mv tmp$$ $ac_file
   rm vpsed$$
   echo 'MULTISUBDIR =' >> $ac_file
   ml_norecursion=yes
   . ${multi_basedir}/config-ml.in
   AS_UNSET([ml_norecursion])
])

AC_MSG_NOTICE([libgm2 has been configured.])

AC_OUTPUT
