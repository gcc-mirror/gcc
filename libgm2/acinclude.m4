dnl acinclude.m4
dnl
dnl This file was derived from libstdc++-v3/acinclude.m4 and heavily pruned.
dnl Its purpose is to check for glibc time, gettimeofday and
dnl float128 availability.
dnl
dnl GLIBCXX_CONDITIONAL (NAME, SHELL-TEST)
dnl
dnl Exactly like AM_CONDITIONAL, but delays evaluation of the test until the
dnl end of configure.  This lets tested variables be reassigned, and the
dnl conditional will depend on the final state of the variable.  For a simple
dnl example of why this is needed, see GLIBCXX_ENABLE_HOSTED.
dnl
m4_define([_m4_divert(glibcxx_diversion)], 8000)dnl
AC_DEFUN([GLIBCXX_CONDITIONAL], [dnl
  m4_divert_text([glibcxx_diversion],dnl
   AM_CONDITIONAL([$1],[$2])
  )dnl
])dnl
AC_DEFUN([GLIBCXX_EVALUATE_CONDITIONALS], [m4_undivert([glibcxx_diversion])])dnl


dnl
dnl Check to see what architecture and operating system we are compiling
dnl for.  Also, if architecture- or OS-specific flags are required for
dnl compilation, pick them up here.
dnl
AC_DEFUN([GLIBCXX_CHECK_HOST], [
  . $glibcxx_srcdir/configure.host
  AC_MSG_NOTICE([CPU config directory is $cpu_include_dir])
  AC_MSG_NOTICE([OS config directory is $os_include_dir])
])


dnl
dnl Initialize the rest of the library configury.  At this point we have
dnl variables like $host.
dnl
dnl Sets:
dnl  SUBDIRS
dnl Substs:
dnl  glibcxx_builddir     (absolute path)
dnl  glibcxx_srcdir       (absolute path)
dnl  toplevel_builddir    (absolute path)
dnl  toplevel_srcdir      (absolute path)
dnl  with_cross_host
dnl  with_newlib
dnl  with_target_subdir
dnl plus
dnl  - the variables in GLIBCXX_CHECK_HOST / configure.host
dnl  - default settings for all AM_CONFITIONAL test variables
dnl  - lots of tools, like CC and CXX
dnl
AC_DEFUN([GLIBCXX_CONFIGURE], [
  # Keep these sync'd with the list in Makefile.am.  The first provides an
  # expandable list at autoconf time; the second provides an expandable list
  # (i.e., shell variable) at configure time.
  m4_define([glibcxx_SUBDIRS],[])
  SUBDIRS='glibcxx_SUBDIRS'

  # These need to be absolute paths, yet at the same time need to
  # canonicalize only relative paths, because then amd will not unmount
  # drives. Thus the use of PWDCMD: set it to 'pawd' or 'amq -w' if using amd.
  glibcxx_builddir=`${PWDCMD-pwd}`
  case $srcdir in
    [\\/$]* | ?:[\\/]*) glibcxx_srcdir=${srcdir} ;;
    *) glibcxx_srcdir=`cd "$srcdir" && ${PWDCMD-pwd} || echo "$srcdir"` ;;
  esac
  toplevel_builddir=${glibcxx_builddir}/..
  toplevel_srcdir=${glibcxx_srcdir}/..
  AC_SUBST(glibcxx_builddir)
  AC_SUBST(glibcxx_srcdir)
  AC_SUBST(toplevel_builddir)
  AC_SUBST(toplevel_srcdir)

  # We use these options to decide which functions to include.  They are
  # set from the top level.
  AC_ARG_WITH([target-subdir],
    AC_HELP_STRING([--with-target-subdir=SUBDIR],
		   [configuring in a subdirectory]))

  AC_ARG_WITH([cross-host],
    AC_HELP_STRING([--with-cross-host=HOST],
		   [configuring with a cross compiler]))

  AC_ARG_WITH([newlib],
    AC_HELP_STRING([--with-newlib],
		   [assume newlib as a system C library]))

  # Will set LN_S to either 'ln -s', 'ln', or 'cp -p' (if linking isn't
  # available).  Uncomment the next line to force a particular method.
  AC_PROG_LN_S
  #LN_S='cp -p'

  AC_CHECK_TOOL(AS, as)
  AC_CHECK_TOOL(AR, ar)
  AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)

  AM_MAINTAINER_MODE

  # Set up safe default values for all subsequent AM_CONDITIONAL tests
  # which are themselves conditionally expanded.
  ## (Right now, this only matters for enable_wchar_t, but nothing prevents
  ## other macros from doing the same.  This should be automated.)  -pme

  # Check for C library flavor since GNU/Linux platforms use different
  # configuration directories depending on the C library in use.
  AC_EGREP_CPP([_using_uclibc], [
  #include <stdio.h>
  #if __UCLIBC__
    _using_uclibc
  #endif
  ], uclibc=yes, uclibc=no)

  AC_EGREP_CPP([_using_bionic], [
  #include <stdio.h>
  #if __BIONIC__
    _using_bionic
  #endif
  ], bionic=yes, bionic=no)

  # Find platform-specific directories containing configuration info.
  # Also possibly modify flags used elsewhere, as needed by the platform.
  GLIBCXX_CHECK_HOST
])


dnl
dnl GLIBCXX_ENABLE
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, permit a|b|c)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, SHELL-CODE-HANDLER)
dnl
dnl See manual/appendix_porting.html#appendix.porting.build_hacking for
dnl documentation.
dnl
m4_define([GLIBCXX_ENABLE],[dnl
m4_define([_g_switch],[--enable-$1])dnl
m4_define([_g_help],[AC_HELP_STRING([_g_switch$3],[$4 @<:@default=$2@:>@])])dnl
 AC_ARG_ENABLE([$1],m4_dquote(_g_help),
  m4_bmatch([$5],
   [^permit ],
     [[
      case "$enableval" in
       m4_bpatsubst([$5],[permit ])) ;;
       *) AC_MSG_ERROR(Unknown argument to enable/disable $1) ;;
	  dnl Idea for future:  generate a URL pointing to
	  dnl "onlinedocs/configopts.html#whatever"
      esac
     ]],
   [^$],
     [[
      case "$enableval" in
       yes|no) ;;
       *) AC_MSG_ERROR(Argument to enable/disable $1 must be yes or no) ;;
      esac
     ]],
   [[$5]]),
  [enable_]m4_bpatsubst([$1],-,_)[=][$2])
m4_undefine([_g_switch])dnl
m4_undefine([_g_help])dnl
])


dnl
dnl Check for clock_gettime, nanosleep and sched_yield, used in the
dnl implementation of 20.11.7 [time.clock], and 30.3.2 [thread.thread.this]
dnl in the C++11 standard.
dnl
dnl --enable-libstdcxx-time
dnl --enable-libstdcxx-time=yes
dnl        checks for the availability of monotonic and realtime clocks,
dnl        nanosleep and sched_yield in libc.
dnl --enable-libstdcxx-time=rt
dnl        also searches (and, if needed, links) librt.  Note that this is
dnl        not always desirable because, in glibc 2.16 and earlier, for
dnl        example, in turn it triggers the linking of libpthread too,
dnl        which activates locking,
dnl        a large overhead for single-thread programs.
dnl --enable-libstdcxx-time=no
dnl --disable-libstdcxx-time
dnl        disables the checks completely
dnl
dnl N.B. Darwin provides nanosleep but doesn't support the whole POSIX
dnl Timers option, so doesn't define _POSIX_TIMERS. Because the test
dnl below fails Darwin unconditionally defines _GLIBCXX_USE_NANOSLEEP in
dnl os_defines.h and also defines _GLIBCXX_USE_SCHED_YIELD.
dnl
dnl needed
AC_DEFUN([GLIBCXX_ENABLE_LIBSTDCXX_TIME], [

  GLIBCXX_ENABLE(libstdcxx-time,auto,[[[=KIND]]],
    [use KIND for check type],
    [permit yes|no|rt])

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
  ac_save_LIBS="$LIBS"

  ac_has_clock_monotonic=no
  ac_has_clock_realtime=no
  ac_has_nanosleep=no
  ac_has_sched_yield=no

  if test x"$enable_libstdcxx_time" = x"auto"; then

    case "${target_os}" in
      cygwin*)
        ac_has_nanosleep=yes
        ;;
      mingw*)
        ac_has_win32_sleep=yes
        ac_has_sched_yield=yes
        ;;
      darwin*)
        ac_has_nanosleep=yes
        ac_has_sched_yield=yes
        ;;
      # VxWorks has nanosleep as soon as the kernel is configured with
      # INCLUDE_POSIX_TIMERS, which is normally/most-often the case.
      vxworks*)
        ac_has_nanosleep=yes
        ;;
      gnu* | linux* | kfreebsd*-gnu | knetbsd*-gnu)
        # Don't use link test for freestanding library, in case gcc_no_link=yes
        if test x"$is_hosted" = xyes; then
          # Versions of glibc before 2.17 needed -lrt for clock_gettime.
          AC_SEARCH_LIBS(clock_gettime, [rt])
          if test x"$ac_cv_search_clock_gettime" = x"none required"; then
            ac_has_clock_monotonic=yes
            ac_has_clock_realtime=yes
          fi
        fi
        ac_has_nanosleep=yes
        ac_has_sched_yield=yes
        ;;
      freebsd*|netbsd*|dragonfly*|rtems*)
        ac_has_clock_monotonic=yes
        ac_has_clock_realtime=yes
        ac_has_nanosleep=yes
        ac_has_sched_yield=yes
        ;;
      openbsd*)
        ac_has_clock_monotonic=yes
        ac_has_clock_realtime=yes
        ac_has_nanosleep=yes
        ;;
      solaris*)
        ac_has_clock_monotonic=yes
        ac_has_clock_realtime=yes
        ac_has_nanosleep=yes
        ac_has_sched_yield=yes
        ;;
      uclinux*)
        ac_has_nanosleep=yes
        ac_has_sched_yield=yes
    esac

  elif test x"$enable_libstdcxx_time" != x"no"; then

    if test x"$enable_libstdcxx_time" = x"rt"; then
      AC_SEARCH_LIBS(clock_gettime, [rt])
      AC_SEARCH_LIBS(nanosleep, [rt])
    else
      AC_CHECK_FUNC(clock_gettime)
      AC_CHECK_FUNC(nanosleep)
    fi

    case "$ac_cv_search_clock_gettime" in
      -l*) GLIBCXX_LIBS=$ac_cv_search_clock_gettime
      ;;
    esac
    case "$ac_cv_search_nanosleep" in
      -l*) GLIBCXX_LIBS="$GLIBCXX_LIBS $ac_cv_search_nanosleep"
      ;;
    esac

    AC_SEARCH_LIBS(sched_yield, [rt])

    case "$ac_cv_search_sched_yield" in
      -lrt*)
      if test x"$enable_libstdcxx_time" = x"rt"; then
	GLIBCXX_LIBS="$GLIBCXX_LIBS $ac_cv_search_sched_yield"
        ac_has_sched_yield=yes
      fi
      ;;
      *)
      ac_has_sched_yield=yes
      ;;
    esac

    AC_CHECK_HEADERS(unistd.h, ac_has_unistd_h=yes, ac_has_unistd_h=no)

    if test x"$ac_has_unistd_h" = x"yes"; then
      AC_MSG_CHECKING([for monotonic clock])
      AC_TRY_LINK(
	[#include <unistd.h>
	 #include <time.h>
	],
	[#if _POSIX_TIMERS > 0 && defined(_POSIX_MONOTONIC_CLOCK)
	  timespec tp;
	 #endif
	  clock_gettime(CLOCK_MONOTONIC, &tp);
	], [ac_has_clock_monotonic=yes], [ac_has_clock_monotonic=no])

      AC_MSG_RESULT($ac_has_clock_monotonic)

      AC_MSG_CHECKING([for realtime clock])
      AC_TRY_LINK(
	[#include <unistd.h>
	 #include <time.h>
	],
	[#if _POSIX_TIMERS > 0
	  timespec tp;
	 #endif
	  clock_gettime(CLOCK_REALTIME, &tp);
	], [ac_has_clock_realtime=yes], [ac_has_clock_realtime=no])

      AC_MSG_RESULT($ac_has_clock_realtime)

      AC_MSG_CHECKING([for nanosleep])
      AC_TRY_LINK(
	[#include <unistd.h>
	 #include <time.h>
	],
	[#if _POSIX_TIMERS > 0
	  timespec tp;
	 #endif
	  nanosleep(&tp, 0);
	], [ac_has_nanosleep=yes], [ac_has_nanosleep=no])

      AC_MSG_RESULT($ac_has_nanosleep)
    fi
  fi

  if test x"$ac_has_clock_monotonic" != x"yes"; then
    case ${target_os} in
      linux* | uclinux*)
	AC_MSG_CHECKING([for clock_gettime syscall])
	AC_TRY_COMPILE(
	  [#include <unistd.h>
	   #include <time.h>
	   #include <sys/syscall.h>
	  ],
	  [#if _POSIX_TIMERS > 0 && defined(_POSIX_MONOTONIC_CLOCK)
	    timespec tp;
	   #endif
	   syscall(SYS_clock_gettime, CLOCK_MONOTONIC, &tp);
	   syscall(SYS_clock_gettime, CLOCK_REALTIME, &tp);
	  ], [ac_has_clock_gettime_syscall=yes], [ac_has_clock_gettime_syscall=no])
	AC_MSG_RESULT($ac_has_clock_gettime_syscall)
	if test x"$ac_has_clock_gettime_syscall" = x"yes"; then
	  AC_DEFINE(_GLIBCXX_USE_CLOCK_GETTIME_SYSCALL, 1,
	  [Defined if clock_gettime syscall has monotonic and realtime clock support. ])
	  ac_has_clock_monotonic=yes
	  ac_has_clock_realtime=yes
	  AC_MSG_CHECKING([for struct timespec that matches syscall])
	  AC_TRY_COMPILE(
	    [#include <time.h>
	     #include <sys/syscall.h>
	    ],
	    [#ifdef SYS_clock_gettime64
	     #if SYS_clock_gettime64 != SYS_clock_gettime
	     // We need to use SYS_clock_gettime and libc appears to
	     // also know about the SYS_clock_gettime64 syscall.
	     // Check that userspace doesn't use time64 version of timespec.
	     static_assert(sizeof(timespec::tv_sec) == sizeof(long),
	       "struct timespec must be compatible with SYS_clock_gettime");
	     #endif
	     #endif
	    ],
	    [ac_timespec_matches_syscall=yes],
	    [ac_timespec_matches_syscall=no])
	  AC_MSG_RESULT($ac_timespec_matches_syscall)
	  if test x"$ac_timespec_matches_syscall" = no; then
	    AC_MSG_ERROR([struct timespec is not compatible with SYS_clock_gettime, please report a bug to http://gcc.gnu.org/bugzilla])
	  fi
	fi;;
    esac
  fi

  if test x"$ac_has_clock_monotonic" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_CLOCK_MONOTONIC, 1,
      [ Defined if clock_gettime has monotonic clock support. ])
  fi

  if test x"$ac_has_clock_realtime" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_CLOCK_REALTIME, 1,
      [ Defined if clock_gettime has realtime clock support. ])
  fi

  if test x"$ac_has_sched_yield" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_SCHED_YIELD, 1,
              [ Defined if sched_yield is available. ])
  fi

  if test x"$ac_has_nanosleep" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_NANOSLEEP, 1,
      [ Defined if nanosleep is available. ])
  elif test x"$ac_has_win32_sleep" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_WIN32_SLEEP, 1,
      [Defined if Sleep exists.])
  else
      AC_MSG_CHECKING([for sleep])
      AC_TRY_COMPILE([#include <unistd.h>],
                     [sleep(1)],
                     [ac_has_sleep=yes],[ac_has_sleep=no])
      if test x"$ac_has_sleep" = x"yes"; then
        AC_DEFINE(HAVE_SLEEP,1, [Defined if sleep exists.])
      fi
      AC_MSG_RESULT($ac_has_sleep)
      AC_MSG_CHECKING([for usleep])
      AC_TRY_COMPILE([#include <unistd.h>],
                     [sleep(1);
                      usleep(100);],
                     [ac_has_usleep=yes],[ac_has_usleep=no])
      if test x"$ac_has_usleep" = x"yes"; then
        AC_DEFINE(HAVE_USLEEP,1, [Defined if usleep exists.])
      fi
      AC_MSG_RESULT($ac_has_usleep)
  fi

  if test x"$ac_has_nanosleep$ac_has_win32_sleep$ac_has_sleep" = x"nonono"; then
    AC_DEFINE(_GLIBCXX_NO_SLEEP,1, [Defined if no way to sleep is available.])
  fi

  AC_SUBST(GLIBCXX_LIBS)

  CXXFLAGS="$ac_save_CXXFLAGS"
  LIBS="$ac_save_LIBS"
  AC_LANG_RESTORE
])

dnl
dnl Check for gettimeofday, used in the implementation of 20.11.7
dnl [time.clock] in the C++11 standard.
dnl
dnl needed
AC_DEFUN([GLIBCXX_CHECK_GETTIMEOFDAY], [

  AC_MSG_CHECKING([for gettimeofday])

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  ac_has_gettimeofday=no;
  AC_CHECK_HEADERS(sys/time.h, ac_has_sys_time_h=yes, ac_has_sys_time_h=no)
  if test x"$ac_has_sys_time_h" = x"yes"; then
    AC_MSG_CHECKING([for gettimeofday])
    GCC_TRY_COMPILE_OR_LINK([#include <sys/time.h>],
      [timeval tv; gettimeofday(&tv, 0);],
      [ac_has_gettimeofday=yes], [ac_has_gettimeofday=no])

    AC_MSG_RESULT($ac_has_gettimeofday)
  fi

  if test x"$ac_has_gettimeofday" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_GETTIMEOFDAY, 1,
      [ Defined if gettimeofday is available. ])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check for which locale library to use.  The choice is mapped to
dnl a subdirectory of config/locale.
dnl
dnl Default is generic.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CLOCALE], [
  GLIBCXX_ENABLE(clocale,auto,[[[=MODEL]]],
    [use MODEL for target locale package],
    [permit generic|gnu|ieee_1003.1-2001|newlib|yes|no|auto])

  # Deal with gettext issues.  Default to not using it (=no) until we detect
  # support for it later.  Let the user turn it off via --e/d, but let that
  # default to on for easier handling.
  USE_NLS=no
  AC_ARG_ENABLE(nls,
    AC_HELP_STRING([--enable-nls],[use Native Language Support (default)]),
    [],
    [enable_nls=yes])

  # Either a known package, or "auto"
  if test $enable_clocale = no || test $enable_clocale = yes; then
     enable_clocale=auto
  fi
  enable_clocale_flag=$enable_clocale

  # Probe for locale model to use if none specified.
  # Default to "generic".
  if test $enable_clocale_flag = auto; then
    case ${target_os} in
      linux* | gnu* | kfreebsd*-gnu | knetbsd*-gnu)
	enable_clocale_flag=gnu
	;;
      darwin*)
	enable_clocale_flag=darwin
	;;
      vxworks*)
	enable_clocale_flag=vxworks
	;;
      dragonfly* | freebsd*)
	enable_clocale_flag=dragonfly
	;;
      openbsd*)
	enable_clocale_flag=newlib
	;;
      *)
	if test x"$with_newlib" = x"yes"; then
	  enable_clocale_flag=newlib
	else
	  enable_clocale_flag=generic
	fi
	;;
    esac
  fi

  # Sanity check model, and test for special functionality.
  if test $enable_clocale_flag = gnu; then
    AC_EGREP_CPP([_GLIBCXX_ok], [
    #include <features.h>
    #if (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 3)) && !defined(__UCLIBC__)
      _GLIBCXX_ok
    #endif
    ], enable_clocale_flag=gnu, enable_clocale_flag=generic)

    # Set it to scream when it hurts.
    ac_save_CFLAGS="$CFLAGS"
    CFLAGS="-Wimplicit-function-declaration -Werror"

    # Use strxfrm_l if available.
    AC_TRY_COMPILE([#define _GNU_SOURCE 1
     		    #include <string.h>
		    #include <locale.h>],
		    [char s[128]; __locale_t loc; strxfrm_l(s, "C", 5, loc);],
		    AC_DEFINE(HAVE_STRXFRM_L, 1,
		    [Define if strxfrm_l is available in <string.h>.]),)

    # Use strerror_l if available.
    AC_TRY_COMPILE([#define _GNU_SOURCE 1
		    #include <string.h>
		    #include <locale.h>],
		    [__locale_t loc; strerror_l(5, loc);],
		    AC_DEFINE(HAVE_STRERROR_L, 1,
		    [Define if strerror_l is available in <string.h>.]),)

    CFLAGS="$ac_save_CFLAGS"
  fi

  # Perhaps use strerror_r if available, and strerror_l isn't.
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS="-Wimplicit-function-declaration -Werror"
  AC_TRY_COMPILE([#define _GNU_SOURCE 1
	     	  #include <string.h>
		  #include <locale.h>],
		  [char s[128]; strerror_r(5, s, 128);],
		  AC_DEFINE(HAVE_STRERROR_R, 1,
		  [Define if strerror_r is available in <string.h>.]),)
  CFLAGS="$ac_save_CFLAGS"

  # Set configure bits for specified locale package
  AC_MSG_CHECKING([for C locale to use])
  case ${enable_clocale_flag} in
    generic)
      AC_MSG_RESULT(generic)

      CLOCALE_H=config/locale/generic/c_locale.h
      CLOCALE_CC=config/locale/generic/c_locale.cc
      CCODECVT_CC=config/locale/generic/codecvt_members.cc
      CCOLLATE_CC=config/locale/generic/collate_members.cc
      CCTYPE_CC=config/locale/generic/ctype_members.cc
      CMESSAGES_H=config/locale/generic/messages_members.h
      CMESSAGES_CC=config/locale/generic/messages_members.cc
      CMONEY_CC=config/locale/generic/monetary_members.cc
      CNUMERIC_CC=config/locale/generic/numeric_members.cc
      CTIME_H=config/locale/generic/time_members.h
      CTIME_CC=config/locale/generic/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;
    darwin)
      AC_MSG_RESULT(darwin)

      CLOCALE_H=config/locale/generic/c_locale.h
      CLOCALE_CC=config/locale/generic/c_locale.cc
      CCODECVT_CC=config/locale/generic/codecvt_members.cc
      CCOLLATE_CC=config/locale/generic/collate_members.cc
      CCTYPE_CC=config/locale/darwin/ctype_members.cc
      CMESSAGES_H=config/locale/generic/messages_members.h
      CMESSAGES_CC=config/locale/generic/messages_members.cc
      CMONEY_CC=config/locale/generic/monetary_members.cc
      CNUMERIC_CC=config/locale/generic/numeric_members.cc
      CTIME_H=config/locale/generic/time_members.h
      CTIME_CC=config/locale/generic/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;
    vxworks)
      AC_MSG_RESULT(vxworks)

      CLOCALE_H=config/locale/generic/c_locale.h
      CLOCALE_CC=config/locale/generic/c_locale.cc
      CCODECVT_CC=config/locale/generic/codecvt_members.cc
      CCOLLATE_CC=config/locale/generic/collate_members.cc
      CCTYPE_CC=config/locale/vxworks/ctype_members.cc
      CMESSAGES_H=config/locale/generic/messages_members.h
      CMESSAGES_CC=config/locale/generic/messages_members.cc
      CMONEY_CC=config/locale/generic/monetary_members.cc
      CNUMERIC_CC=config/locale/generic/numeric_members.cc
      CTIME_H=config/locale/generic/time_members.h
      CTIME_CC=config/locale/generic/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;
    dragonfly)
      AC_MSG_RESULT(dragonfly or freebsd)

      CLOCALE_H=config/locale/dragonfly/c_locale.h
      CLOCALE_CC=config/locale/dragonfly/c_locale.cc
      CCODECVT_CC=config/locale/dragonfly/codecvt_members.cc
      CCOLLATE_CC=config/locale/dragonfly/collate_members.cc
      CCTYPE_CC=config/locale/dragonfly/ctype_members.cc
      CMESSAGES_H=config/locale/generic/messages_members.h
      CMESSAGES_CC=config/locale/generic/messages_members.cc
      CMONEY_CC=config/locale/dragonfly/monetary_members.cc
      CNUMERIC_CC=config/locale/dragonfly/numeric_members.cc
      CTIME_H=config/locale/dragonfly/time_members.h
      CTIME_CC=config/locale/dragonfly/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;

    gnu)
      AC_MSG_RESULT(gnu)

      # Declare intention to use gettext, and add support for specific
      # languages.
      # For some reason, ALL_LINGUAS has to be before AM-GNU-GETTEXT
      ALL_LINGUAS="de fr"

      # Don't call AM-GNU-GETTEXT here. Instead, assume glibc.
      AC_CHECK_PROG(check_msgfmt, msgfmt, yes, no)
      if test x"$check_msgfmt" = x"yes" && test x"$enable_nls" = x"yes"; then
	USE_NLS=yes
      fi
      # Export the build objects.
      for ling in $ALL_LINGUAS; do \
	glibcxx_MOFILES="$glibcxx_MOFILES $ling.mo"; \
	glibcxx_POFILES="$glibcxx_POFILES $ling.po"; \
      done
      AC_SUBST(glibcxx_MOFILES)
      AC_SUBST(glibcxx_POFILES)

      CLOCALE_H=config/locale/gnu/c_locale.h
      CLOCALE_CC=config/locale/gnu/c_locale.cc
      CCODECVT_CC=config/locale/gnu/codecvt_members.cc
      CCOLLATE_CC=config/locale/gnu/collate_members.cc
      CCTYPE_CC=config/locale/gnu/ctype_members.cc
      CMESSAGES_H=config/locale/gnu/messages_members.h
      CMESSAGES_CC=config/locale/gnu/messages_members.cc
      CMONEY_CC=config/locale/gnu/monetary_members.cc
      CNUMERIC_CC=config/locale/gnu/numeric_members.cc
      CTIME_H=config/locale/gnu/time_members.h
      CTIME_CC=config/locale/gnu/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/gnu/c++locale_internal.h
      ;;
    ieee_1003.1-2001)
      AC_MSG_RESULT(IEEE 1003.1)

      CLOCALE_H=config/locale/ieee_1003.1-2001/c_locale.h
      CLOCALE_CC=config/locale/ieee_1003.1-2001/c_locale.cc
      CCODECVT_CC=config/locale/generic/codecvt_members.cc
      CCOLLATE_CC=config/locale/generic/collate_members.cc
      CCTYPE_CC=config/locale/generic/ctype_members.cc
      CMESSAGES_H=config/locale/ieee_1003.1-2001/messages_members.h
      CMESSAGES_CC=config/locale/ieee_1003.1-2001/messages_members.cc
      CMONEY_CC=config/locale/generic/monetary_members.cc
      CNUMERIC_CC=config/locale/generic/numeric_members.cc
      CTIME_H=config/locale/generic/time_members.h
      CTIME_CC=config/locale/generic/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;
    newlib)
      AC_MSG_RESULT(newlib)

      CLOCALE_H=config/locale/generic/c_locale.h
      CLOCALE_CC=config/locale/generic/c_locale.cc
      CCODECVT_CC=config/locale/generic/codecvt_members.cc
      CCOLLATE_CC=config/locale/generic/collate_members.cc
      CCTYPE_CC=config/locale/newlib/ctype_members.cc
      CMESSAGES_H=config/locale/generic/messages_members.h
      CMESSAGES_CC=config/locale/generic/messages_members.cc
      CMONEY_CC=config/locale/generic/monetary_members.cc
      CNUMERIC_CC=config/locale/generic/numeric_members.cc
      CTIME_H=config/locale/generic/time_members.h
      CTIME_CC=config/locale/generic/time_members.cc
      CLOCALE_INTERNAL_H=config/locale/generic/c++locale_internal.h
      ;;
  esac

  # This is where the testsuite looks for locale catalogs, using the
  # -DLOCALEDIR define during testsuite compilation.
  glibcxx_localedir=${glibcxx_builddir}/po/share/locale
  AC_SUBST(glibcxx_localedir)

  # A standalone libintl (e.g., GNU libintl) may be in use.
  if test $USE_NLS = yes; then
    AC_CHECK_HEADERS([libintl.h], [], USE_NLS=no)
    AC_SEARCH_LIBS(gettext, intl, [], USE_NLS=no)
  fi
  if test $USE_NLS = yes; then
    AC_DEFINE(_GLIBCXX_USE_NLS, 1,
	      [Define if NLS translations are to be used.])
  fi

  AC_SUBST(USE_NLS)
  AC_SUBST(CLOCALE_H)
  AC_SUBST(CMESSAGES_H)
  AC_SUBST(CCODECVT_CC)
  AC_SUBST(CCOLLATE_CC)
  AC_SUBST(CCTYPE_CC)
  AC_SUBST(CMESSAGES_CC)
  AC_SUBST(CMONEY_CC)
  AC_SUBST(CNUMERIC_CC)
  AC_SUBST(CTIME_H)
  AC_SUBST(CTIME_CC)
  AC_SUBST(CLOCALE_CC)
  AC_SUBST(CLOCALE_INTERNAL_H)
])


dnl
dnl Check for GNU 128-bit floating point type.
dnl
dnl Note: also checks that the type isn't a standard types.
dnl
dnl Defines:
dnl  ENABLE_FLOAT128
dnl
AC_DEFUN([GLIBCXX_ENABLE_FLOAT128], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS

  # Fake what AC_TRY_COMPILE does, without linking as this is
  # unnecessary for this test.

  cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
template<typename T1, typename T2>
  struct same
  { typedef T2 type; };

template<typename T>
  struct same<T, T>;

int main()
{
  typename same<double, __float128>::type      f1;
  typename same<long double, __float128>::type f2;
}
EOF

    AC_MSG_CHECKING([for __float128])
    if AC_TRY_EVAL(ac_compile); then
      enable_float128=yes
    else
      enable_float128=no
    fi
    AC_MSG_RESULT($enable_float128)
    GLIBCXX_CONDITIONAL(ENABLE_FLOAT128, test $enable_float128 = yes)
    rm -f conftest*

  AC_LANG_RESTORE
])

# Macros from the top-level gcc directory.
m4_include([../config/gc++filt.m4])
m4_include([../config/tls.m4])
m4_include([../config/gthr.m4])
m4_include([../config/cet.m4])
