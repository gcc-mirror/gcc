# generated automatically by aclocal 1.7.8 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002
# Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.


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
dnl Initialize basic configure bits.
dnl
dnl Substs:
dnl  multi_basedir
dnl
AC_DEFUN([GLIBCXX_TOPREL_CONFIGURE], [
  # Sets up multi_basedir, which is srcdir/.. plus the usual
  # "multi_source_toprel_bottom_adjust" lunacy as needed.
  AM_ENABLE_MULTILIB(, ..)

  # The generated code is exactly the same, except that automake's looks in
  # ".. $srcdir/.." and autoconf's looks in multi_basedir.  Apparently other
  # things are triggered on the presence of the two ...AUX_DIR[S], but I don't
  # know what they are or what the other differences might be (and they keep
  # changing anyhow).
  #
  # Looking in multi_basedir seems smarter, so actually execute that branch.
  if false; then
    # this is for automake
    AC_CONFIG_AUX_DIR(..)
  else
    # this is for autoconf
    AC_CONFIG_AUX_DIRS(${multi_basedir})
  fi

  dnl XXX Turn this on.
  dnl AC_LANG_CPLUSPLUS
])


dnl
dnl Initialize the rest of the library configury.  At this point we have
dnl variables like $host.
dnl
dnl Sets:
dnl  gcc_version          (x.y.z format)
dnl  SUBDIRS
dnl Substs:
dnl  glibcxx_builddir     (absolute path)
dnl  glibcxx_srcdir       (absolute path)
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
  m4_define([glibcxx_SUBDIRS],[include libmath libsupc++ src po testsuite])
  SUBDIRS='glibcxx_SUBDIRS'

  # These need to be absolute paths, yet at the same time need to
  # canonicalize only relative paths, because then amd will not unmount
  # drives. Thus the use of PWDCMD: set it to 'pawd' or 'amq -w' if using amd.
  glibcxx_builddir=`${PWDCMD-pwd}`
  case $srcdir in
    [\\/$]* | ?:[\\/]*) glibcxx_srcdir=${srcdir} ;;
    *) glibcxx_srcdir=`cd "$srcdir" && ${PWDCMD-pwd} || echo "$srcdir"` ;;
  esac
  toplevel_srcdir=${glibcxx_srcdir}/..
  AC_SUBST(glibcxx_builddir)
  AC_SUBST(glibcxx_srcdir)
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

  # We're almost certainly being configured before anything else which uses
  # C++, so all of our AC_PROG_* discoveries will be cached.  It's vital that
  # we not cache the value of CXX that we "discover" here, because it's set
  # to something unique for us and libjava.  Other target libraries need to
  # find CXX for themselves.  We yank the rug out from under the normal AC_*
  # process by sneakily renaming the cache variable.  This also lets us debug
  # the value of "our" CXX in postmortems.
  #
  # We must also force CXX to /not/ be a precious variable, otherwise the
  # wrong (non-multilib-adjusted) value will be used in multilibs.  This
  # little trick also affects CPPFLAGS, CXXFLAGS, and LDFLAGS.  And as a side
  # effect, CXXFLAGS is no longer automagically subst'd, so we have to do
  # that ourselves.  Un-preciousing AC_PROG_CC also affects CC and CFLAGS.
  #
  # -fno-builtin must be present here so that a non-conflicting form of
  # std::exit can be guessed by AC_PROG_CXX, and used in later tests.

  m4_define([ac_cv_prog_CXX],[glibcxx_cv_prog_CXX])
  m4_rename([_AC_ARG_VAR_PRECIOUS],[glibcxx_PRECIOUS])
  m4_define([_AC_ARG_VAR_PRECIOUS],[])
  save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-builtin"
  AC_PROG_CC
  AC_PROG_CXX
  CXXFLAGS="$save_CXXFLAGS"
  m4_rename([glibcxx_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])
  AC_SUBST(CFLAGS)
  AC_SUBST(CXXFLAGS)

  # For directory versioning (e.g., headers) and other variables.
  AC_MSG_CHECKING([for GCC version number])
  gcc_version=`$CXX -dumpversion`
  AC_MSG_RESULT($gcc_version)

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
  need_libmath=no
  enable_wchar_t=no
  #enable_libstdcxx_debug=no
  #enable_libstdcxx_pch=no
  #enable_cheaders=c
  #c_compatibility=no
  #enable_abi_check=no
  #enable_symvers=no
  #enable_hosted_libstdcxx=yes

  # Find platform-specific directories containing configuration info.
  # Also possibly modify flags used elsewhere, as needed by the platform.
  GLIBCXX_CHECK_HOST
])


m4_include([linkage.m4])
m4_include([../config/no-executables.m4])


dnl
dnl Tests for newer compiler features, or features that are present in newer
dnl compiler versions but not older compiler versions still in use, should
dnl be placed here.
dnl
dnl Defines:
dnl  WERROR='-Werror' if requested and possible; g++'s that lack the
dnl   new inlining code or the new system_header pragma will die on -Werror.
dnl   Leave it out by default and use maint-mode to use it.
dnl  SECTION_FLAGS='-ffunction-sections -fdata-sections' if
dnl   compiler supports it and the user has not requested debug mode.
dnl
AC_DEFUN([GLIBCXX_CHECK_COMPILER_FEATURES], [
  # All these tests are for C++; save the language and the compiler flags.
  # The CXXFLAGS thing is suspicious, but based on similar bits previously
  # found in GLIBCXX_CONFIGURE.
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_test_CXXFLAGS="${CXXFLAGS+set}"
  ac_save_CXXFLAGS="$CXXFLAGS"

  # Check for maintainer-mode bits.
  if test x"$USE_MAINTAINER_MODE" = xno; then
    WERROR=''
  else
    WERROR='-Werror'
  fi

  # Check for -ffunction-sections -fdata-sections
  AC_MSG_CHECKING([for g++ that supports -ffunction-sections -fdata-sections])
  CXXFLAGS='-Werror -ffunction-sections -fdata-sections'
  AC_TRY_COMPILE(, [int foo;], [ac_fdsections=yes], [ac_fdsections=no])
  if test "$ac_test_CXXFLAGS" = set; then
    CXXFLAGS="$ac_save_CXXFLAGS"
  else
    # this is the suspicious part
    CXXFLAGS=''
  fi
  if test x"$ac_fdsections" = x"yes"; then
    SECTION_FLAGS='-ffunction-sections -fdata-sections'
  fi
  AC_MSG_RESULT($ac_fdsections)

  AC_LANG_RESTORE
  AC_SUBST(WERROR)
  AC_SUBST(SECTION_FLAGS)
])


dnl
dnl If GNU ld is in use, check to see if tricky linker opts can be used.  If
dnl the native linker is in use, all variables will be defined to something
dnl safe (like an empty string).
dnl
dnl Defines:
dnl  SECTION_LDFLAGS='-Wl,--gc-sections' if possible
dnl  OPT_LDFLAGS='-Wl,-O1' if possible
dnl  LD (as a side effect of testing)
dnl Sets:
dnl  with_gnu_ld
dnl  glibcxx_gnu_ld_version (possibly)
dnl
dnl The last will be a single integer, e.g., version 1.23.45.0.67.89 will
dnl set glibcxx_gnu_ld_version to 12345.  Zeros cause problems.
dnl
AC_DEFUN([GLIBCXX_CHECK_LINKER_FEATURES], [
  # If we're not using GNU ld, then there's no point in even trying these
  # tests.  Check for that first.  We should have already tested for gld
  # by now (in libtool), but require it now just to be safe...
  test -z "$SECTION_LDFLAGS" && SECTION_LDFLAGS=''
  test -z "$OPT_LDFLAGS" && OPT_LDFLAGS=''
  AC_REQUIRE([AC_PROG_LD])
  AC_REQUIRE([AC_PROG_AWK])

  # The name set by libtool depends on the version of libtool.  Shame on us
  # for depending on an impl detail, but c'est la vie.  Older versions used
  # ac_cv_prog_gnu_ld, but now it's lt_cv_prog_gnu_ld, and is copied back on
  # top of with_gnu_ld (which is also set by --with-gnu-ld, so that actually
  # makes sense).  We'll test with_gnu_ld everywhere else, so if that isn't
  # set (hence we're using an older libtool), then set it.
  if test x${with_gnu_ld+set} != xset; then
    if test x${ac_cv_prog_gnu_ld+set} != xset; then
      # We got through "ac_require(ac_prog_ld)" and still not set?  Huh?
      with_gnu_ld=no
    else
      with_gnu_ld=$ac_cv_prog_gnu_ld
    fi
  fi

  # Start by getting the version number.  I think the libtool test already
  # does some of this, but throws away the result.
  changequote(,)
  ldver=`$LD --version 2>/dev/null | head -1 | \
         sed -e 's/GNU ld version \([0-9.][0-9.]*\).*/\1/'`
  changequote([,])
  glibcxx_gnu_ld_version=`echo $ldver | \
         $AWK -F. '{ if (NF<3) [$]3=0; print ([$]1*100+[$]2)*100+[$]3 }'`

  # Set --gc-sections.
  if test "$with_gnu_ld" = "notbroken"; then
    # GNU ld it is!  Joy and bunny rabbits!

    # All these tests are for C++; save the language and the compiler flags.
    # Need to do this so that g++ won't try to link in libstdc++
    ac_test_CFLAGS="${CFLAGS+set}"
    ac_save_CFLAGS="$CFLAGS"
    CFLAGS='-x c++  -Wl,--gc-sections'

    # Check for -Wl,--gc-sections
    # XXX This test is broken at the moment, as symbols required for linking
    # are now in libsupc++ (not built yet).  In addition, this test has
    # cored on solaris in the past.  In addition, --gc-sections doesn't
    # really work at the moment (keeps on discarding used sections, first
    # .eh_frame and now some of the glibc sections for iconv).
    # Bzzzzt.  Thanks for playing, maybe next time.
    AC_MSG_CHECKING([for ld that supports -Wl,--gc-sections])
    AC_TRY_RUN([
     int main(void)
     {
       try { throw 1; }
       catch (...) { };
       return 0;
     }
    ], [ac_sectionLDflags=yes],[ac_sectionLDflags=no], [ac_sectionLDflags=yes])
    if test "$ac_test_CFLAGS" = set; then
      CFLAGS="$ac_save_CFLAGS"
    else
      # this is the suspicious part
      CFLAGS=''
    fi
    if test "$ac_sectionLDflags" = "yes"; then
      SECTION_LDFLAGS="-Wl,--gc-sections $SECTION_LDFLAGS"
    fi
    AC_MSG_RESULT($ac_sectionLDflags)
  fi

  # Set linker optimization flags.
  if test x"$with_gnu_ld" = x"yes"; then
    OPT_LDFLAGS="-Wl,-O1 $OPT_LDFLAGS"
  fi

  AC_SUBST(SECTION_LDFLAGS)
  AC_SUBST(OPT_LDFLAGS)
])


dnl
dnl Check to see if this target can enable the wchar_t parts.
dnl If --disable-c-mbchar was given, no wchar_t stuff is enabled.  (This
dnl must have been previously checked.)  By default, wide characters are
dnl disabled.
dnl
dnl Defines:
dnl  HAVE_MBSTATE_T if mbstate_t is not in wchar.h
dnl  _GLIBCXX_USE_WCHAR_T if all the bits are found.
dnl
AC_DEFUN([GLIBCXX_CHECK_WCHAR_T_SUPPORT], [
  # Test wchar.h for mbstate_t, which is needed for char_traits and
  # others even if wchar_t support is not on.
  AC_MSG_CHECKING([for mbstate_t])
  AC_TRY_COMPILE([#include <wchar.h>],
  [mbstate_t teststate;],
  have_mbstate_t=yes, have_mbstate_t=no)
  AC_MSG_RESULT($have_mbstate_t)
  if test x"$have_mbstate_t" = xyes; then
    AC_DEFINE(HAVE_MBSTATE_T)
  fi

  # Sanity check for existence of ISO C99 headers for extended encoding.
  AC_CHECK_HEADERS(wchar.h, ac_has_wchar_h=yes, ac_has_wchar_h=no)
  AC_CHECK_HEADERS(wctype.h, ac_has_wctype_h=yes, ac_has_wctype_h=no)

  # Only continue checking if the ISO C99 headers exist and support is on.
  if test x"$ac_has_wchar_h" = xyes &&
     test x"$ac_has_wctype_h" = xyes &&
     test x"$enable_c_mbchar" != xno; then

    # Test wchar.h for WCHAR_MIN, WCHAR_MAX, which is needed before
    # numeric_limits can instantiate type_traits<wchar_t>
    AC_MSG_CHECKING([for WCHAR_MIN and WCHAR_MAX])
    AC_TRY_COMPILE([#include <wchar.h>],
    [int i = WCHAR_MIN; int j = WCHAR_MAX;],
    has_wchar_minmax=yes, has_wchar_minmax=no)
    AC_MSG_RESULT($has_wchar_minmax)

    # Test wchar.h for WEOF, which is what we use to determine whether
    # to specialize for char_traits<wchar_t> or not.
    AC_MSG_CHECKING([for WEOF])
    AC_TRY_COMPILE([
      #include <wchar.h>
      #include <stddef.h>],
    [wint_t i = WEOF;],
    has_weof=yes, has_weof=no)
    AC_MSG_RESULT($has_weof)

    # Tests for wide character functions used in char_traits<wchar_t>.
    ac_wfuncs=yes
    AC_CHECK_FUNCS([wcslen wmemchr wmemcmp wmemcpy wmemmove wmemset],
    [],[ac_wfuncs=no])

    # Checks for names injected into std:: by the c_std headers.
    AC_CHECK_FUNCS([btowc wctob fgetwc fgetws fputwc fputws fwide \
    fwprintf fwscanf swprintf swscanf vfwprintf vswprintf \
    vwprintf wprintf wscanf getwc getwchar mbsinit mbrlen mbrtowc \
    mbsrtowcs wcsrtombs putwc putwchar ungetwc wcrtomb wcstod wcstol \
    wcstoul wcscpy wcsncpy wcscat wcsncat wcscmp wcscoll wcsncmp wcsxfrm \
    wcscspn wcsspn wcstok wcsftime wcschr wcspbrk wcsrchr wcsstr],
    [],[ac_wfuncs=no])

    # Checks for wide character functions that are not required
    # for basic wchar_t support.  Don't disable support if they are missing.
    # Injection of these is wrapped with guard macros.
    AC_CHECK_FUNCS([vfwscanf vswscanf vwscanf wcstof iswblank],[],[])

    AC_MSG_CHECKING([for ISO C99 wchar_t support])
    if test x"$has_weof" = xyes &&
       test x"$has_wchar_minmax" = xyes &&
       test x"$ac_wfuncs" = xyes;
    then
      ac_isoC99_wchar_t=yes
    else
      ac_isoC99_wchar_t=no
    fi
    AC_MSG_RESULT($ac_isoC99_wchar_t)

    # Use iconv for wchar_t to char conversions. As such, check for
    # X/Open Portability Guide, version 2 features (XPG2).
    AC_CHECK_HEADER(iconv.h, ac_has_iconv_h=yes, ac_has_iconv_h=no)
    AC_CHECK_HEADER(langinfo.h, ac_has_langinfo_h=yes, ac_has_langinfo_h=no)

    # Check for existence of libiconv.a providing XPG2 wchar_t support.
    AC_CHECK_LIB(iconv, iconv, libiconv="-liconv")
    ac_save_LIBS="$LIBS"
    LIBS="$LIBS $libiconv"

    AC_CHECK_FUNCS([iconv_open iconv_close iconv nl_langinfo],
    [ac_XPG2funcs=yes], [ac_XPG2funcs=no])

    LIBS="$ac_save_LIBS"

    AC_MSG_CHECKING([for XPG2 wchar_t support])
    if test x"$ac_has_iconv_h" = xyes &&
       test x"$ac_has_langinfo_h" = xyes &&
       test x"$ac_XPG2funcs" = xyes;
    then
      ac_XPG2_wchar_t=yes
    else
      ac_XPG2_wchar_t=no
    fi
    AC_MSG_RESULT($ac_XPG2_wchar_t)

    # At the moment, only enable wchar_t specializations if all the
    # above support is present.
    if test x"$ac_isoC99_wchar_t" = xyes &&
       test x"$ac_XPG2_wchar_t" = xyes;
    then
      AC_DEFINE(_GLIBCXX_USE_WCHAR_T)
      enable_wchar_t=yes
    fi
  fi
  AC_MSG_CHECKING([for enabled wchar_t specializations])
  AC_MSG_RESULT($enable_wchar_t)
])


dnl
dnl Check for headers for, and arguments to, the setrlimit() function.
dnl Used only in testsuite_hooks.h.  Called from GLIBCXX_CONFIGURE_TESTSUITE.
dnl
dnl Defines:
dnl  _GLIBCXX_RES_LIMITS if we can set artificial resource limits 
dnl  various HAVE_LIMIT_* for individual limit names
dnl
AC_DEFUN([GLIBCXX_CHECK_SETRLIMIT_ancilliary], [
  AC_TRY_COMPILE(
    [#include <unistd.h>
     #include <sys/time.h>
     #include <sys/resource.h>
    ],
    [ int f = RLIMIT_$1 ; ],
    [glibcxx_mresult=1], [glibcxx_mresult=0])
  AC_DEFINE_UNQUOTED(HAVE_LIMIT_$1, $glibcxx_mresult,
                     [Only used in build directory testsuite_hooks.h.])
])

AC_DEFUN([GLIBCXX_CHECK_SETRLIMIT], [
  setrlimit_have_headers=yes
  AC_CHECK_HEADERS(unistd.h sys/time.h sys/resource.h,
                   [],
                   [setrlimit_have_headers=no])
  # If don't have the headers, then we can't run the tests now, and we
  # won't be seeing any of these during testsuite compilation.
  if test $setrlimit_have_headers = yes; then
    # Can't do these in a loop, else the resulting syntax is wrong.
    GLIBCXX_CHECK_SETRLIMIT_ancilliary(DATA)
    GLIBCXX_CHECK_SETRLIMIT_ancilliary(RSS)
    GLIBCXX_CHECK_SETRLIMIT_ancilliary(VMEM)
    GLIBCXX_CHECK_SETRLIMIT_ancilliary(AS)
    GLIBCXX_CHECK_SETRLIMIT_ancilliary(FSIZE)

    # Check for rlimit, setrlimit.
    AC_CACHE_VAL(ac_setrlimit, [
      AC_TRY_COMPILE(
        [#include <unistd.h>
         #include <sys/time.h>
         #include <sys/resource.h>
        ],
        [struct rlimit r;
         setrlimit(0, &r);],
        [ac_setrlimit=yes], [ac_setrlimit=no])
    ])
  fi

  AC_MSG_CHECKING([for testsuite resource limits support])
  if test $setrlimit_have_headers = yes && test $ac_setrlimit = yes; then
    ac_res_limits=yes
    AC_DEFINE(_GLIBCXX_RES_LIMITS)
  else
    ac_res_limits=no
  fi
  AC_MSG_RESULT($ac_res_limits)
])


dnl
dnl Check whether S_ISREG (Posix) or S_IFREG is available in <sys/stat.h>.
dnl Define HAVE_S_ISREG / HAVE_S_IFREG appropriately.
dnl
AC_DEFUN([GLIBCXX_CHECK_S_ISREG_OR_S_IFREG], [
  AC_CACHE_VAL(glibcxx_cv_S_ISREG, [
    AC_TRY_LINK(
      [#include <sys/stat.h>],
      [struct stat buffer;
       fstat(0, &buffer);
       S_ISREG(buffer.st_mode);],
      [glibcxx_cv_S_ISREG=yes],
      [glibcxx_cv_S_ISREG=no])
  ])
  AC_CACHE_VAL(glibcxx_cv_S_IFREG, [
    AC_TRY_LINK(
      [#include <sys/stat.h>],
      [struct stat buffer;
       fstat(0, &buffer);
       S_IFREG & buffer.st_mode;],
      [glibcxx_cv_S_IFREG=yes],
      [glibcxx_cv_S_IFREG=no])
  ])
  if test $glibcxx_cv_S_ISREG = yes; then
    AC_DEFINE(HAVE_S_ISREG)
  elif test $glibcxx_cv_S_IFREG = yes; then
    AC_DEFINE(HAVE_S_IFREG)
  fi
])


dnl
dnl Check whether poll is available in <poll.h>, and define HAVE_POLL.
dnl
AC_DEFUN([GLIBCXX_CHECK_POLL], [
  AC_CACHE_VAL(glibcxx_cv_POLL, [
    AC_TRY_LINK(
      [#include <poll.h>],
      [struct pollfd pfd[1];
       pfd[0].events = POLLIN;
       poll(pfd, 1, 0);],
      [glibcxx_cv_POLL=yes],
      [glibcxx_cv_POLL=no])
  ])
  if test $glibcxx_cv_POLL = yes; then
    AC_DEFINE(HAVE_POLL)
  fi
])


dnl
dnl Check whether writev is available in <sys/uio.h>, and define HAVE_WRITEV.
dnl
AC_DEFUN([GLIBCXX_CHECK_WRITEV], [
  AC_CACHE_VAL(glibcxx_cv_WRITEV, [
    AC_TRY_LINK(
      [#include <sys/uio.h>],
      [struct iovec iov[2];
       writev(0, iov, 0);],
      [glibcxx_cv_WRITEV=yes],
      [glibcxx_cv_WRITEV=no])
  ])
  if test $glibcxx_cv_WRITEV = yes; then
    AC_DEFINE(HAVE_WRITEV)
  fi
])


dnl
dnl Check whether int64_t is available in <stdint.h>, and define HAVE_INT64_T.
dnl
AC_DEFUN([GLIBCXX_CHECK_INT64_T], [
  AC_CACHE_VAL(glibcxx_cv_INT64_T, [
    AC_TRY_COMPILE(
      [#include <stdint.h>],
      [int64_t var;],
      [glibcxx_cv_INT64_T=yes],
      [glibcxx_cv_INT64_T=no])
  ])
  if test $glibcxx_cv_INT64_T = yes; then
    AC_DEFINE(HAVE_INT64_T)
  fi
])


dnl
dnl Check whether LFS support is available.
dnl
AC_DEFUN([GLIBCXX_CHECK_LFS], [
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"	
  AC_CACHE_VAL(glibcxx_cv_LFS, [
    AC_TRY_LINK(
      [#include <unistd.h>
       #include <stdio.h>
      ],
      [FILE* fp;
       fopen64("t", "w");
       fseeko64(fp, 0, SEEK_CUR);
       ftello64(fp);
       lseek64(1, 0, SEEK_CUR);],	
      [glibcxx_cv_LFS=yes],
      [glibcxx_cv_LFS=no])
  ])
  if test $glibcxx_cv_LFS = yes; then
    AC_DEFINE(_GLIBCXX_USE_LFS)
  fi
  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Does any necessary configuration of the testsuite directory.  Generates
dnl the testsuite_hooks.h header.
dnl
dnl GLIBCXX_ENABLE_SYMVERS and GLIBCXX_IS_NATIVE must be done before this.
dnl
dnl Sets:
dnl  enable_abi_check / GLIBCXX_TEST_ABI
dnl Substs:
dnl  baseline_dir
dnl
AC_DEFUN([GLIBCXX_CONFIGURE_TESTSUITE], [
  if $GLIBCXX_IS_NATIVE && test $is_hosted = yes; then
    # Do checks for resource limit functions.
    GLIBCXX_CHECK_SETRLIMIT

    # Look for setenv, so that extended locale tests can be performed.
    GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_3(setenv)

    if test $enable_symvers = no; then
      enable_abi_check=no
    else
      case "$host" in
        *-*-cygwin*)
          enable_abi_check=no ;;
        *)
          enable_abi_check=yes ;;
      esac
    fi
  else
    # Only build this as native, since automake does not understand
    # CXX_FOR_BUILD.
    enable_abi_check=no
  fi

  # Export file names for ABI checking.
  baseline_dir="$glibcxx_srcdir/config/abi/${abi_baseline_pair}\$(MULTISUBDIR)"
  AC_SUBST(baseline_dir)

  GLIBCXX_CONDITIONAL(GLIBCXX_TEST_WCHAR_T, test $enable_wchar_t = yes)
  GLIBCXX_CONDITIONAL(GLIBCXX_TEST_ABI, test $enable_abi_check = yes)
])


dnl
dnl Set up *_INCLUDES variables for all sundry Makefile.am's.
dnl
dnl Substs:
dnl  GLIBCXX_INCLUDES
dnl  TOPLEVEL_INCLUDES
dnl
AC_DEFUN([GLIBCXX_EXPORT_INCLUDES], [
  # Used for every C++ compile we perform.
  GLIBCXX_INCLUDES="\
-I$glibcxx_builddir/include/$host_alias \
-I$glibcxx_builddir/include \
-I$glibcxx_srcdir/libsupc++"

  # For Canadian crosses, pick this up too.
  if test $CANADIAN = yes; then
    GLIBCXX_INCLUDES="$GLIBCXX_INCLUDES -I\${includedir}"
  fi

  # Stuff in the actual top level.  Currently only used by libsupc++ to
  # get unwind* headers from the gcc dir.
  #TOPLEVEL_INCLUDES='-I$(toplevel_srcdir)/gcc -I$(toplevel_srcdir)/include'
  TOPLEVEL_INCLUDES='-I$(toplevel_srcdir)/gcc'

  # Now, export this to all the little Makefiles....
  AC_SUBST(GLIBCXX_INCLUDES)
  AC_SUBST(TOPLEVEL_INCLUDES)
])


dnl
dnl Set up *_FLAGS and *FLAGS variables for all sundry Makefile.am's.
dnl (SECTION_FLAGS is done under CHECK_COMPILER_FEATURES.)
dnl
dnl Substs:
dnl  OPTIMIZE_CXXFLAGS
dnl  WARN_FLAGS
dnl
AC_DEFUN([GLIBCXX_EXPORT_FLAGS], [
  # Optimization flags that are probably a good idea for thrill-seekers. Just
  # uncomment the lines below and make, everything else is ready to go...
  # OPTIMIZE_CXXFLAGS = -O3 -fstrict-aliasing -fvtable-gc
  OPTIMIZE_CXXFLAGS=
  AC_SUBST(OPTIMIZE_CXXFLAGS)

  WARN_FLAGS='-Wall -W -Wwrite-strings -Wcast-qual'
  AC_SUBST(WARN_FLAGS)
])


dnl
dnl All installation directory information is determined here.
dnl
dnl Substs:
dnl  gxx_install_dir
dnl  glibcxx_prefixdir
dnl  glibcxx_toolexecdir
dnl  glibcxx_toolexeclibdir
dnl
dnl Assumes cross_compiling bits already done, and with_cross_host in
dnl particular.
dnl
AC_DEFUN([GLIBCXX_EXPORT_INSTALL_INFO], [
  glibcxx_toolexecdir=no
  glibcxx_toolexeclibdir=no
  glibcxx_prefixdir=$prefix

  AC_MSG_CHECKING([for gxx-include-dir])
  AC_ARG_WITH([gxx-include-dir],
    AC_HELP_STRING([--with-gxx-include-dir=DIR],
                   [installation directory for include files]),
    [case "$withval" in
      yes) AC_MSG_ERROR([Missing directory for --with-gxx-include-dir]) ;;
      no)  gxx_include_dir=no ;;
      *)   gxx_include_dir=$withval ;;
     esac],
    [gxx_include_dir=no])
  AC_MSG_RESULT($gxx_include_dir)

  AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
  AC_ARG_ENABLE([version-specific-runtime-libs],
    AC_HELP_STRING([--enable-version-specific-runtime-libs],
                   [Specify that runtime libraries should be installed in a compiler-specific directory]),
    [case "$enableval" in
      yes) version_specific_libs=yes ;;
      no)  version_specific_libs=no ;;
      *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
     esac],
    [version_specific_libs=no])
  AC_MSG_RESULT($version_specific_libs)

  # Default case for install directory for include files.
  if test $version_specific_libs = no && test $gxx_include_dir = no; then
    gxx_include_dir='${prefix}'/include/c++/${gcc_version}
  fi

  # Version-specific runtime libs processing.
  if test $version_specific_libs = yes; then
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.  FIXME: these variables are misnamed, there are
    # no executables installed in _toolexecdir or _toolexeclibdir.
    if test x"$gxx_include_dir" = x"no"; then
      gxx_include_dir='${libdir}/gcc/${host_alias}/'$gcc_version/include/c++
    fi
    glibcxx_toolexecdir='${libdir}/gcc/${host_alias}'
    glibcxx_toolexeclibdir='${toolexecdir}/'$gcc_version'$(MULTISUBDIR)'
  fi

  # Calculate glibcxx_toolexecdir, glibcxx_toolexeclibdir
  # Install a library built with a cross compiler in tooldir, not libdir.
  if test x"$glibcxx_toolexecdir" = x"no"; then
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      glibcxx_toolexecdir='${exec_prefix}/${host_alias}'
      glibcxx_toolexeclibdir='${toolexecdir}/lib'
    else
      glibcxx_toolexecdir='${libdir}/gcc/${host_alias}'
      glibcxx_toolexeclibdir='${libdir}'
    fi
    multi_os_directory=`$CXX -print-multi-os-directory`
    case $multi_os_directory in
      .) ;; # Avoid trailing /.
      *) glibcxx_toolexeclibdir=$glibcxx_toolexeclibdir/$multi_os_directory ;;
    esac
  fi

  AC_MSG_CHECKING([for install location])
  AC_MSG_RESULT($gxx_include_dir)

  AC_SUBST(glibcxx_prefixdir)
  AC_SUBST(gxx_include_dir)
  AC_SUBST(glibcxx_toolexecdir)
  AC_SUBST(glibcxx_toolexeclibdir)
])


dnl
dnl GLIBCXX_ENABLE
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, permit a|b|c)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, SHELL-CODE-HANDLER)
dnl
dnl See docs/html/17_intro/configury.html#enable for documentation.
dnl
m4_define([GLIBCXX_ENABLE],[dnl
m4_define([_g_switch],[--enable-$1])dnl
m4_define([_g_help],[AC_HELP_STRING(_g_switch$3,[$4 @<:@default=$2@:>@])])dnl
 AC_ARG_ENABLE($1,_g_help,
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
dnl Check for ISO/IEC 9899:1999 "C99" support.
dnl
dnl --enable-c99 defines _GLIBCXX_USE_C99
dnl --disable-c99 leaves _GLIBCXX_USE_C99 undefined
dnl  +  Usage:  GLIBCXX_ENABLE_C99[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl  +  If 'C99' stuff is not available, ignores DEFAULT and sets `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_C99], [
  GLIBCXX_ENABLE(c99,$1,,[turns on ISO/IEC 9899:1999 support])

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS

  # Check for the existence of <math.h> functions used if C99 is enabled.
  ac_c99_math=yes;
  AC_MSG_CHECKING([for ISO C99 support in <math.h>])
  AC_TRY_COMPILE([#include <math.h>],[fpclassify(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isfinite(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isinf(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isnan(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isnormal(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[signbit(0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isgreater(0.0,0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],
                 [isgreaterequal(0.0,0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[isless(0.0,0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],[islessequal(0.0,0.0);],,[ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],
                 [islessgreater(0.0,0.0);],, [ac_c99_math=no])
  AC_TRY_COMPILE([#include <math.h>],
                 [isunordered(0.0,0.0);],, [ac_c99_math=no])
  AC_MSG_RESULT($ac_c99_math)

  if test x"$ac_c99_math" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_MATH)
  fi

  # Check for the existence in <stdio.h> of vscanf, et. al.
  ac_c99_stdio=yes;
  AC_MSG_CHECKING([for ISO C99 support in <stdio.h>])
  AC_TRY_COMPILE([#include <stdio.h>],
                 [snprintf("12", 0, "%i");],, [ac_c99_stdio=no])
  AC_TRY_COMPILE([#include <stdio.h>
                  #include <stdarg.h>
                  void foo(char* fmt, ...)
                  {va_list args; va_start(args, fmt);
                  vfscanf(stderr, "%i", args);}],
                  [],, [ac_c99_stdio=no])
  AC_TRY_COMPILE([#include <stdio.h>
                  #include <stdarg.h>
                  void foo(char* fmt, ...)
                  {va_list args; va_start(args, fmt);
                  vscanf("%i", args);}],
                  [],, [ac_c99_stdio=no])
  AC_TRY_COMPILE([#include <stdio.h>
                  #include <stdarg.h>
                  void foo(char* fmt, ...)
                  {va_list args; va_start(args, fmt);
                  vsnprintf(fmt, 0, "%i", args);}],
                  [],, [ac_c99_stdio=no])
  AC_TRY_COMPILE([#include <stdio.h>
                  #include <stdarg.h>
                  void foo(char* fmt, ...)
                  {va_list args; va_start(args, fmt);
                  vsscanf(fmt, "%i", args);}],
                  [],, [ac_c99_stdio=no])
  AC_MSG_RESULT($ac_c99_stdio)

  # Check for the existence in <stdlib.h> of lldiv_t, et. al.
  ac_c99_stdlib=yes;
  AC_MSG_CHECKING([for lldiv_t declaration])
  AC_CACHE_VAL(ac_c99_lldiv_t, [
  AC_TRY_COMPILE([#include <stdlib.h>],
                   [ lldiv_t mydivt;],
                   [ac_c99_lldiv_t=yes], [ac_c99_lldiv_t=no])
  ])
  AC_MSG_RESULT($ac_c99_lldiv_t)

  AC_MSG_CHECKING([for ISO C99 support in <stdlib.h>])
  AC_TRY_COMPILE([#include <stdlib.h>],
                 [char* tmp; strtof("gnu", &tmp);],, [ac_c99_stdlib=no])
  AC_TRY_COMPILE([#include <stdlib.h>],
                 [char* tmp; strtold("gnu", &tmp);],, [ac_c99_stdlib=no])
  AC_TRY_COMPILE([#include <stdlib.h>], [llabs(10);],, [ac_c99_stdlib=no])
  AC_TRY_COMPILE([#include <stdlib.h>], [lldiv(10,1);],, [ac_c99_stdlib=no])
  AC_TRY_COMPILE([#include <stdlib.h>], [atoll("10");],, [ac_c99_stdlib=no])
  AC_TRY_COMPILE([#include <stdlib.h>], [_Exit(0);],, [ac_c99_stdlib=no])
  if test x"$ac_c99_lldiv_t" = x"no"; then
    ac_c99_stdlib=no;
  fi;
  AC_MSG_RESULT($ac_c99_stdlib)

  # Check for the existence of <wchar.h> functions used if C99 is enabled.
  # XXX the wchar.h checks should be rolled into the general C99 bits.
  ac_c99_wchar=yes;
  AC_MSG_CHECKING([for additional ISO C99 support in <wchar.h>])
  AC_TRY_COMPILE([#include <wchar.h>],
                 [wcstold(L"10.0", NULL);],, [ac_c99_wchar=no])
  AC_TRY_COMPILE([#include <wchar.h>],
                 [wcstoll(L"10", NULL, 10);],, [ac_c99_wchar=no])
  AC_TRY_COMPILE([#include <wchar.h>],
                 [wcstoull(L"10", NULL, 10);],, [ac_c99_wchar=no])
  AC_MSG_RESULT($ac_c99_wchar)

  AC_MSG_CHECKING([for enabled ISO C99 support])
  if test x"$ac_c99_math" = x"no" ||
     test x"$ac_c99_stdio" = x"no" ||
     test x"$ac_c99_stdlib" = x"no" ||
     test x"$ac_c99_wchar" = x"no"; then
    enable_c99=no;
  fi;
  AC_MSG_RESULT($enable_c99)

  if test x"$ac_99_math" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_MATH)
  fi

  # Option parsed, now set things appropriately
  if test x"$enable_c99" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99)
  fi

  AC_LANG_RESTORE
])


dnl
dnl Check for what type of C headers to use.
dnl
dnl --enable-cheaders= [does stuff].
dnl --disable-cheaders [does not do anything, really].
dnl  +  Usage:  GLIBCXX_ENABLE_CHEADERS[(DEFAULT)]
dnl       Where DEFAULT is either `c' or `c_std'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CHEADERS], [
  GLIBCXX_ENABLE(cheaders,$1,[=KIND],
    [construct "C" headers for g++], [permit c|c_std])
  AC_MSG_NOTICE("C" header strategy set to $enable_cheaders)

  C_INCLUDE_DIR='${glibcxx_srcdir}/include/'$enable_cheaders

  AC_SUBST(C_INCLUDE_DIR)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_C, test $enable_cheaders = c)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_C_STD, test $enable_cheaders = c_std)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_COMPATIBILITY, test $c_compatibility = yes)
])


dnl
dnl Check for which locale library to use.  The choice is mapped to
dnl a subdirectory of config/locale.
dnl
dnl Default is generic.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CLOCALE], [
  AC_MSG_CHECKING([for C locale to use])
  GLIBCXX_ENABLE(clocale,auto,[@<:@=MODEL@:>@],
    [use MODEL for target locale package],
    [permit generic|gnu|ieee_1003.1-2001|yes|no|auto])
  
  # If they didn't use this option switch, or if they specified --enable
  # with no specific model, we'll have to look for one.  If they
  # specified --disable (???), do likewise.
  if test $enable_clocale = no || test $enable_clocale = yes; then
     enable_clocale=auto
  fi

  # Either a known package, or "auto"
  enable_clocale_flag=$enable_clocale

  # Probe for locale support if no specific model is specified.
  # Default to "generic".
  if test $enable_clocale_flag = auto; then
    case x${target_os} in
      xlinux* | xgnu* | xkfreebsd*-gnu | xknetbsd*-gnu)
        AC_EGREP_CPP([_GLIBCXX_ok], [
        #include <features.h>
        #if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2)
          _GLIBCXX_ok
        #endif
        ], enable_clocale_flag=gnu, enable_clocale_flag=generic)

        # Test for bugs early in glibc-2.2.x series
          if test x$enable_clocale_flag = xgnu; then
          AC_TRY_RUN([
          #define _GNU_SOURCE 1
          #include <locale.h>
          #include <string.h>
          #if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ > 2)
          extern __typeof(newlocale) __newlocale;
          extern __typeof(duplocale) __duplocale;
          extern __typeof(strcoll_l) __strcoll_l;
          #endif
          int main()
          {
              const char __one[] = "Äuglein Augmen";
              const char __two[] = "Äuglein";
              int i;
              int j;
              __locale_t        loc;
               __locale_t        loc_dup;
              loc = __newlocale(1 << LC_ALL, "de_DE", 0);
              loc_dup = __duplocale(loc);
              i = __strcoll_l(__one, __two, loc);
              j = __strcoll_l(__one, __two, loc_dup);
              return 0;
          }
          ],
          [enable_clocale_flag=gnu],[enable_clocale_flag=generic],
          [enable_clocale_flag=generic])
          fi

        # ... at some point put __strxfrm_l tests in as well.
        ;;
      *)
        enable_clocale_flag=generic
        ;;
    esac
  fi

  # Deal with gettext issues.  Default to not using it (=no) until we detect
  # support for it later.  Let the user turn it off via --e/d, but let that
  # default to on for easier handling.
  USE_NLS=no
  AC_ARG_ENABLE(nls,
    AC_HELP_STRING([--enable-nls],[use Native Language Support (default)]),
    [],
    [enable_nls=yes])

  # Set configure bits for specified locale package
  case ${enable_clocale_flag} in
    generic)
      AC_MSG_RESULT(generic)

      CLOCALE_H=config/locale/generic/c_locale.h
      CLOCALE_CC=config/locale/generic/c_locale.cc
      CCODECVT_H=config/locale/generic/codecvt_specializations.h
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
      CCODECVT_H=config/locale/ieee_1003.1-2001/codecvt_specializations.h
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
      CCODECVT_H=config/locale/ieee_1003.1-2001/codecvt_specializations.h
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
    AC_DEFINE(_GLIBCXX_USE_NLS)
  fi

  AC_SUBST(USE_NLS)
  AC_SUBST(CLOCALE_H)
  AC_SUBST(CCODECVT_H)
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
dnl Check for which std::allocator base class to use.  The choice is
dnl mapped from a subdirectory of include/ext.
dnl
dnl Default is new.
dnl
AC_DEFUN([GLIBCXX_ENABLE_ALLOCATOR], [
  AC_MSG_CHECKING([for std::allocator base class to use])
  GLIBCXX_ENABLE(libstdcxx-allocator,auto,[=KIND],
    [use KIND for target std::allocator base],
    [permit new|malloc|mt|bitmap|pool|yes|no|auto])
  # If they didn't use this option switch, or if they specified --enable
  # with no specific model, we'll have to look for one.  If they
  # specified --disable (???), do likewise.
  if test $enable_libstdcxx_allocator = no || test $enable_libstdcxx_allocator = yes; then
     enable_libstdcxx_allocator=auto
  fi

  # Either a known package, or "auto"
  enable_libstdcxx_allocator_flag=$enable_libstdcxx_allocator

  # Probe for host-specific support if no specific model is specified.
  # Default to "new".
  if test $enable_libstdcxx_allocator_flag = auto; then
    case ${target_os} in
      *)
        enable_libstdcxx_allocator_flag=new
        ;;
    esac
  fi
  AC_MSG_RESULT($enable_libstdcxx_allocator_flag)
  

  # Set configure bits for specified locale package
  case ${enable_libstdcxx_allocator_flag} in
    bitmap)
      ALLOCATOR_H=config/allocator/bitmap_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::bitmap_allocator
      ;;
    malloc)
      ALLOCATOR_H=config/allocator/malloc_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::malloc_allocator
      ;;
    mt)
      ALLOCATOR_H=config/allocator/mt_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::__mt_alloc
      ;;
    new)
      ALLOCATOR_H=config/allocator/new_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::new_allocator
      ;;
    pool)
      ALLOCATOR_H=config/allocator/pool_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::__pool_alloc
      ;;	
  esac

  AC_SUBST(ALLOCATOR_H)
  AC_SUBST(ALLOCATOR_NAME)
])


dnl
dnl Check for whether the Boost-derived checks should be turned on.
dnl
dnl --enable-concept-checks turns them on.
dnl --disable-concept-checks leaves them off.
dnl  +  Usage:  GLIBCXX_ENABLE_CONCEPT_CHECKS[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CONCEPT_CHECKS], [
  GLIBCXX_ENABLE(concept-checks,$1,,[use Boost-derived template checks])
  if test $enable_concept_checks = yes; then
    AC_DEFINE(_GLIBCXX_CONCEPT_CHECKS)
  fi
])


dnl
dnl Check for which I/O library to use:  stdio, or something specific.
dnl
dnl Default is stdio.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CSTDIO], [
  AC_MSG_CHECKING([for underlying I/O to use])
  GLIBCXX_ENABLE(cstdio,stdio,[=PACKAGE],
    [use target-specific I/O package], [permit stdio])

  # Now that libio has been removed, you can have any color you want as long
  # as it's black.  This is one big no-op until other packages are added, but
  # showing the framework never hurts.
  case ${enable_cstdio} in
    stdio)
      CSTDIO_H=config/io/c_io_stdio.h
      BASIC_FILE_H=config/io/basic_file_stdio.h
      BASIC_FILE_CC=config/io/basic_file_stdio.cc
      AC_MSG_RESULT(stdio)
      ;;
  esac

  AC_SUBST(CSTDIO_H)
  AC_SUBST(BASIC_FILE_H)
  AC_SUBST(BASIC_FILE_CC)
])


dnl
dnl Check for "unusual" flags to pass to the compiler while building.
dnl
dnl --enable-cxx-flags='-foo -bar -baz' is a general method for passing
dnl     experimental flags such as -fpch, -fIMI, -Dfloat=char, etc.
dnl --disable-cxx-flags passes nothing.
dnl  +  See http://gcc.gnu.org/ml/libstdc++/2000-q2/msg00131.html
dnl         http://gcc.gnu.org/ml/libstdc++/2000-q2/msg00284.html
dnl         http://gcc.gnu.org/ml/libstdc++/2000-q1/msg00035.html
dnl  +  Usage:  GLIBCXX_ENABLE_CXX_FLAGS(default flags)
dnl       If "default flags" is an empty string, the effect is the same
dnl       as --disable or --enable=no.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CXX_FLAGS], [dnl
  AC_MSG_CHECKING([for extra compiler flags for building])
  GLIBCXX_ENABLE(cxx-flags,$1,[=FLAGS],
    [pass compiler FLAGS when building library],
    [case "x$enable_cxx_flags" in
      xno | x)   enable_cxx_flags= ;;
      x-*)       ;;
      *)         AC_MSG_ERROR(_g_switch needs compiler flags as arguments) ;;
     esac])

  # Run through flags (either default or command-line) and set anything
  # extra (e.g., #defines) that must accompany particular g++ options.
  if test -n "$enable_cxx_flags"; then
    for f in $enable_cxx_flags; do
      case "$f" in
        -fhonor-std)  ;;
        -*)  ;;
        *)   # and we're trying to pass /what/ exactly?
             AC_MSG_ERROR([compiler flags start with a -]) ;;
      esac
    done
  fi

  EXTRA_CXX_FLAGS="$enable_cxx_flags"
  AC_MSG_RESULT($EXTRA_CXX_FLAGS)
  AC_SUBST(EXTRA_CXX_FLAGS)
])


dnl
dnl Check for wide character support.  Has the same effect as the option
dnl in gcc's configure, but in a form that autoconf can mess with.
dnl
dnl --enable-c-mbchar requests all the wchar_t stuff.
dnl --disable-c-mbchar doesn't.
dnl  +  Usage:  GLIBCXX_ENABLE_C_MBCHAR[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_C_MBCHAR], [
  GLIBCXX_ENABLE(c-mbchar,$1,,[enable multibyte (wide) characters])
  # Option parsed, now other scripts can test enable_c_mbchar for yes/no.
])


dnl
dnl Check to see if debugging libraries are to be built.
dnl
dnl --enable-libstdcxx-debug
dnl builds a separate set of debugging libraries in addition to the
dnl normal (shared, static) libstdc++ binaries.
dnl
dnl --disable-libstdcxx-debug
dnl builds only one (non-debug) version of libstdc++.
dnl
dnl --enable-libstdcxx-debug-flags=FLAGS
dnl iff --enable-debug == yes, then use FLAGS to build the debug library.
dnl
dnl  +  Usage:  GLIBCXX_ENABLE_DEBUG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_DEBUG], [
  AC_MSG_CHECKING([for additional debug build])
  GLIBCXX_ENABLE(libstdcxx-debug,$1,,[build extra debug library])
  AC_MSG_RESULT($enable_libstdcxx_debug)
  GLIBCXX_CONDITIONAL(GLIBCXX_BUILD_DEBUG, test $enable_libstdcxx_debug = yes)
])


dnl
dnl Check for explicit debug flags.
dnl
dnl --enable-libstdcxx-debug-flags='-O1'
dnl is a general method for passing flags to be used when
dnl building debug libraries with --enable-debug.
dnl
dnl --disable-libstdcxx-debug-flags does nothing.
dnl  +  Usage:  GLIBCXX_ENABLE_DEBUG_FLAGS(default flags)
dnl       If "default flags" is an empty string, the effect is the same
dnl       as --disable or --enable=no.
dnl
AC_DEFUN([GLIBCXX_ENABLE_DEBUG_FLAGS], [
  GLIBCXX_ENABLE(libstdcxx-debug-flags,[$1],[=FLAGS],
    [pass compiler FLAGS when building debug library],
    [case "x$enable_libstdcxx_debug_flags" in
      xno | x)    enable_libstdcxx_debug_flags= ;;
      x-*)        ;;
      *)          AC_MSG_ERROR(_g_switch needs compiler flags as arguments) ;;
     esac])

  # Option parsed, now set things appropriately
  DEBUG_FLAGS="$enable_libstdcxx_debug_flags"
  AC_SUBST(DEBUG_FLAGS)

  AC_MSG_NOTICE([Debug build flags set to $DEBUG_FLAGS])
])


dnl
dnl Check if the user only wants a freestanding library implementation.
dnl
dnl --disable-hosted-libstdcxx will turn off most of the library build,
dnl installing only the headers required by [17.4.1.3] and the language
dnl support library.  More than that will be built (to keep the Makefiles
dnl conveniently clean), but not installed.
dnl
dnl Sets:
dnl  is_hosted  (yes/no)
dnl
dnl Defines:
dnl  _GLIBCXX_HOSTED   (always defined, either to 1 or 0)
dnl
AC_DEFUN([GLIBCXX_ENABLE_HOSTED], [
  AC_ARG_ENABLE([hosted-libstdcxx],
    AC_HELP_STRING([--disable-hosted-libstdcxx],
                   [only build freestanding C++ runtime support]),,
    [enable_hosted_libstdcxx=yes])
  if test "$enable_hosted_libstdcxx" = no; then
    AC_MSG_NOTICE([Only freestanding libraries will be built])
    is_hosted=no
    hosted_define=0
    enable_abi_check=no
    enable_libstdcxx_pch=no
  else
    is_hosted=yes
    hosted_define=1
  fi
  GLIBCXX_CONDITIONAL(GLIBCXX_HOSTED, test $is_hosted = yes)
  AC_DEFINE_UNQUOTED(_GLIBCXX_HOSTED, $hosted_define,
    [Define to 1 if a full hosted library is built, or 0 if freestanding.])
])


dnl
dnl Check for template specializations for the 'long long' type extension.
dnl The result determines only whether 'long long' I/O is enabled; things
dnl like numeric_limits<> specializations are always available.
dnl
dnl --enable-long-long defines _GLIBCXX_USE_LONG_LONG
dnl --disable-long-long leaves _GLIBCXX_USE_LONG_LONG undefined
dnl  +  Usage:  GLIBCXX_ENABLE_LONG_LONG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_LONG_LONG], [
  GLIBCXX_ENABLE(long-long,$1,,[enables I/O support for 'long long'])
  if test $enable_long_long = yes; then
    AC_DEFINE(_GLIBCXX_USE_LONG_LONG)
  fi
])


dnl
dnl Check to see if building and using a C++ precompiled header can be done.
dnl
dnl --enable-libstdcxx-pch=yes
dnl default, this shows intent to use stdc++.h.gch If it looks like it
dnl may work, after some light-hearted attempts to puzzle out compiler
dnl support, flip bits on in include/Makefile.am
dnl
dnl --disable-libstdcxx-pch
dnl turns off attempts to use or build stdc++.h.gch.
dnl
dnl Substs:
dnl  glibcxx_PCHFLAGS
dnl
AC_DEFUN([GLIBCXX_ENABLE_PCH], [
  AC_MSG_CHECKING([for enabled PCH])
  GLIBCXX_ENABLE(libstdcxx-pch,$1,,[build pre-compiled libstdc++ headers])
  AC_MSG_RESULT([$enable_libstdcxx_pch])

  if test $enable_libstdcxx_pch = yes; then
    AC_CACHE_CHECK([for compiler with PCH support],
      [glibcxx_cv_prog_CXX_pch],
      [ac_save_CXXFLAGS="$CXXFLAGS"
       CXXFLAGS="$CXXFLAGS -Werror -Winvalid-pch -Wno-deprecated"
       AC_LANG_SAVE
       AC_LANG_CPLUSPLUS
       echo '#include <math.h>' > conftest.h
       if $CXX $CXXFLAGS $CPPFLAGS -x c++-header conftest.h \
		          -o conftest.h.gch 1>&5 2>&1 &&
	        echo '#error "pch failed"' > conftest.h &&
          echo '#include "conftest.h"' > conftest.cc &&
	       $CXX -c $CXXFLAGS $CPPFLAGS conftest.cc 1>&5 2>&1 ;
       then
         glibcxx_cv_prog_CXX_pch=yes
       else
         glibcxx_cv_prog_CXX_pch=no
       fi
       rm -f conftest*
       CXXFLAGS=$ac_save_CXXFLAGS
       AC_LANG_RESTORE
      ])
    enable_libstdcxx_pch=$glibcxx_cv_prog_CXX_pch
  fi

  GLIBCXX_CONDITIONAL(GLIBCXX_BUILD_PCH, test $enable_libstdcxx_pch = yes)
  if test $enable_libstdcxx_pch = yes; then
    glibcxx_PCHFLAGS="-include bits/stdc++.h"
  else
    glibcxx_PCHFLAGS=""
  fi
  AC_SUBST(glibcxx_PCHFLAGS)
])


dnl
dnl Check for exception handling support.  If an explicit enable/disable
dnl sjlj exceptions is given, we don't have to detect.  Otherwise the
dnl target may or may not support call frame exceptions.
dnl
dnl --enable-sjlj-exceptions forces the use of builtin setjmp.
dnl --disable-sjlj-exceptions forces the use of call frame unwinding.
dnl Neither one forces an attempt at detection.
dnl
dnl Defines:
dnl  _GLIBCXX_SJLJ_EXCEPTIONS if the compiler is configured for it
dnl
AC_DEFUN([GLIBCXX_ENABLE_SJLJ_EXCEPTIONS], [
  AC_MSG_CHECKING([for exception model to use])
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  GLIBCXX_ENABLE(sjlj-exceptions,auto,,
    [force use of builtin_setjmp for exceptions],
    [permit yes|no|auto])

  if test $enable_sjlj_exceptions = auto; then
    # Botheration.  Now we've got to detect the exception model.  Link tests
    # against libgcc.a are problematic since we've not been given proper -L
    # bits for single-tree newlib and libgloss.
    #
    # Fake what AC_TRY_COMPILE does.  XXX Look at redoing this new-style.
    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
struct S { ~S(); };
void bar();
void foo()
{
  S s;
  bar();
}
EOF
    old_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS=-S
    if AC_TRY_EVAL(ac_compile); then
      if grep _Unwind_SjLj_Resume conftest.s >/dev/null 2>&1 ; then
        enable_sjlj_exceptions=yes
      elif grep _Unwind_Resume conftest.s >/dev/null 2>&1 ; then
        enable_sjlj_exceptions=no
      fi
    fi
    CXXFLAGS="$old_CXXFLAGS"
    rm -f conftest*
  fi

  # This is a tad weird, for hysterical raisins.  We have to map enable/disable 
  # to two different models.
  case $enable_sjlj_exceptions in
    yes)
      AC_DEFINE(_GLIBCXX_SJLJ_EXCEPTIONS, 1,
        [Define if the compiler is configured for setjmp/longjmp exceptions.])
      ac_exception_model_name=sjlj
      ;;
    no)
      ac_exception_model_name="call frame"
      ;;
    *)
      AC_MSG_ERROR([unable to detect exception model])
      ;;
  esac
 AC_LANG_RESTORE
 AC_MSG_RESULT($ac_exception_model_name)
])


dnl
dnl Add version tags to symbols in shared library (or not), additionally
dnl marking other symbols as private/local (or not).
dnl
dnl --enable-symvers=style adds a version script to the linker call when
dnl       creating the shared library.  The choice of version script is
dnl       controlled by 'style'.
dnl --disable-symvers does not.
dnl  +  Usage:  GLIBCXX_ENABLE_SYMVERS[(DEFAULT)]
dnl       Where DEFAULT is either 'yes' or 'no'.  Passing `yes' tries to
dnl       choose a default style based on linker characteristics.  Passing
dnl       'no' disables versioning.
dnl
AC_DEFUN([GLIBCXX_ENABLE_SYMVERS], [

GLIBCXX_ENABLE(symvers,$1,[=STYLE],
  [enables symbol versioning of the shared library],
  [permit yes|no|gnu])

# If we never went through the GLIBCXX_CHECK_LINKER_FEATURES macro, then we
# don't know enough about $LD to do tricks...
AC_REQUIRE([GLIBCXX_CHECK_LINKER_FEATURES])
# FIXME  The following test is too strict, in theory.
if test $enable_shared = no ||
        test "x$LD" = x ||
        test x$glibcxx_gnu_ld_version = x; then
  enable_symvers=no
fi

# Check to see if libgcc_s exists, indicating that shared libgcc is possible.
if test $enable_symvers != no; then
  AC_MSG_CHECKING([for shared libgcc])
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=' -lgcc_s'
  AC_TRY_LINK(, [return 0;], glibcxx_shared_libgcc=yes, glibcxx_shared_libgcc=no)
  CFLAGS="$ac_save_CFLAGS"
  AC_MSG_RESULT($glibcxx_shared_libgcc)
fi

# For GNU ld, we need at least this version.  The format is described in
# GLIBCXX_CHECK_LINKER_FEATURES above.
glibcxx_min_gnu_ld_version=21400
# XXXXXXXXXXX glibcxx_gnu_ld_version=21390

# Check to see if unspecified "yes" value can win, given results above.
# Change "yes" into either "no" or a style name.
if test $enable_symvers = yes; then
  if test $with_gnu_ld = yes &&
     test $glibcxx_shared_libgcc = yes;
  then
    if test $glibcxx_gnu_ld_version -ge $glibcxx_min_gnu_ld_version ; then
      enable_symvers=gnu
    else
      # The right tools, the right setup, but too old.  Fallbacks?
      AC_MSG_WARN(=== Linker version $glibcxx_gnu_ld_version is too old for)
      AC_MSG_WARN(=== full symbol versioning support in this release of GCC.)
      AC_MSG_WARN(=== You would need to upgrade your binutils to version)
      AC_MSG_WARN(=== $glibcxx_min_gnu_ld_version or later and rebuild GCC.)
      if test $glibcxx_gnu_ld_version -ge 21200 ; then
        # Globbing fix is present, proper block support is not.
        dnl AC_MSG_WARN([=== Dude, you are soooo close.  Maybe we can fake it.])
        dnl enable_symvers=???
        AC_MSG_WARN([=== Symbol versioning will be disabled.])
        enable_symvers=no
      else
        # 2.11 or older.
        AC_MSG_WARN([=== Symbol versioning will be disabled.])
        enable_symvers=no
      fi
    fi
  else
    # just fail for now
    AC_MSG_WARN([=== You have requested some kind of symbol versioning, but])
    AC_MSG_WARN([=== either you are not using a supported linker, or you are])
    AC_MSG_WARN([=== not building a shared libgcc_s (which is required).])
    AC_MSG_WARN([=== Symbol versioning will be disabled.])
    enable_symvers=no
  fi
fi

# Everything parsed; figure out what file to use.
case $enable_symvers in
  no)
    SYMVER_MAP=config/linker-map.dummy
    ;;
  gnu)
    SYMVER_MAP=config/linker-map.gnu
    AC_DEFINE(_GLIBCXX_SYMVER)
    ;;
esac

AC_SUBST(SYMVER_MAP)
AC_SUBST(port_specific_symbol_files)
GLIBCXX_CONDITIONAL(GLIBCXX_BUILD_VERSIONED_SHLIB, test $enable_symvers != no)
AC_MSG_NOTICE(versioning on shared library symbols is $enable_symvers)
])


dnl
dnl Setup to use the gcc gthr.h thread-specific memory and mutex model.
dnl We must stage the required headers so that they will be installed
dnl with the library (unlike libgcc, the STL implementation is provided
dnl solely within headers).  Since we must not inject random user-space
dnl macro names into user-provided C++ code, we first stage into <file>-in
dnl and process to <file> with an output command.  The reason for a two-
dnl stage process here is to correctly handle $srcdir!=$objdir without
dnl having to write complex code (the sed commands to clean the macro
dnl namespace are complex and fragile enough as it is).  We must also
dnl add a relative path so that -I- is supported properly.
dnl
dnl Substs:
dnl  glibcxx_thread_h
dnl
dnl Defines:
dnl  HAVE_GTHR_DEFAULT
dnl
AC_DEFUN([GLIBCXX_ENABLE_THREADS], [
  AC_MSG_CHECKING([for thread model used by GCC])
  target_thread_file=`$CXX -v 2>&1 | sed -n 's/^Thread model: //p'`
  AC_MSG_RESULT([$target_thread_file])

  if test $target_thread_file != single; then
    AC_DEFINE(HAVE_GTHR_DEFAULT)
  fi

  glibcxx_thread_h=gthr-$target_thread_file.h
  AC_SUBST(glibcxx_thread_h)
])


# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file file be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.

# serial 1
AC_DEFUN([AC_LC_MESSAGES], [
  AC_CHECK_HEADER(locale.h, [
    AC_CACHE_CHECK([for LC_MESSAGES], ac_cv_val_LC_MESSAGES,
      [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       ac_cv_val_LC_MESSAGES=yes, ac_cv_val_LC_MESSAGES=no)])
    if test $ac_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES)
    fi
  ])
])


sinclude([../libtool.m4])
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
AC_DEFUN([AC_LIBTOOL_DLOPEN])
AC_DEFUN([AC_PROG_LD])
])

dnl vim:et:ts=2:sw=2

# AM_CONDITIONAL                                              -*- Autoconf -*-

# Copyright 1997, 2000, 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 5

AC_PREREQ(2.52)

# AM_CONDITIONAL(NAME, SHELL-CONDITION)
# -------------------------------------
# Define a conditional.
AC_DEFUN([AM_CONDITIONAL],
[ifelse([$1], [TRUE],  [AC_FATAL([$0: invalid condition: $1])],
        [$1], [FALSE], [AC_FATAL([$0: invalid condition: $1])])dnl
AC_SUBST([$1_TRUE])
AC_SUBST([$1_FALSE])
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi
AC_CONFIG_COMMANDS_PRE(
[if test -z "${$1_TRUE}" && test -z "${$1_FALSE}"; then
  AC_MSG_ERROR([conditional "$1" was never defined.
Usually this means the macro was only invoked conditionally.])
fi])])

# Copyright 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

# AM_ENABLE_MULTILIB([MAKEFILE], [REL-TO-TOP-SRCDIR])
# ---------------------------------------------------
# Add --enable-multilib to configure.
AC_DEFUN([AM_ENABLE_MULTILIB],
[# Default to --enable-multilib
AC_ARG_ENABLE(multilib,
[  --enable-multilib         build many library versions (default)],
[case "$enableval" in
  yes) multilib=yes ;;
  no)  multilib=no ;;
  *)   AC_MSG_ERROR([bad value $enableval for multilib option]) ;;
 esac],
              [multilib=yes])

# We may get other options which we leave undocumented:
# --with-target-subdir, --with-multisrctop, --with-multisubdir
# See config-ml.in if you want the gory details.

if test "$srcdir" = "."; then
  if test "$with_target_subdir" != "."; then
    multi_basedir="$srcdir/$with_multisrctop../$2"
  else
    multi_basedir="$srcdir/$with_multisrctop$2"
  fi
else
  multi_basedir="$srcdir/$2"
fi
AC_SUBST(multi_basedir)

AC_OUTPUT_COMMANDS([
# Only add multilib support code if we just rebuilt the top-level
# Makefile.
case " $CONFIG_FILES " in
 *" ]m4_default([$1],Makefile)[ "*)
   ac_file=]m4_default([$1],Makefile)[ . ${multi_basedir}/config-ml.in
   ;;
esac],
                   [
srcdir="$srcdir"
host="$host"
target="$target"
with_multisubdir="$with_multisubdir"
with_multisrctop="$with_multisrctop"
with_target_subdir="$with_target_subdir"
ac_configure_args="${multilib_arg} ${ac_configure_args}"
multi_basedir="$multi_basedir"
CONFIG_SHELL=${CONFIG_SHELL-/bin/sh}
CC="$CC"])])dnl

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# Copyright 1996, 1998, 2000, 2001, 2002  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 2

AC_DEFUN([AM_MAINTAINER_MODE],
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT([$USE_MAINTAINER_MODE])
  AM_CONDITIONAL(MAINTAINER_MODE, [test $USE_MAINTAINER_MODE = yes])
  MAINT=$MAINTAINER_MODE_TRUE
  AC_SUBST(MAINT)dnl
]
)

AU_DEFUN([jm_MAINTAINER_MODE], [AM_MAINTAINER_MODE])

# Do all the work for Automake.                            -*- Autoconf -*-

# This macro actually does too much some checks are only needed if
# your package does certain things.  But this isn't really a big deal.

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003
# Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 10

AC_PREREQ([2.54])

# Autoconf 2.50 wants to disallow AM_ names.  We explicitly allow
# the ones we care about.
m4_pattern_allow([^AM_[A-Z]+FLAGS$])dnl

# AM_INIT_AUTOMAKE(PACKAGE, VERSION, [NO-DEFINE])
# AM_INIT_AUTOMAKE([OPTIONS])
# -----------------------------------------------
# The call with PACKAGE and VERSION arguments is the old style
# call (pre autoconf-2.50), which is being phased out.  PACKAGE
# and VERSION should now be passed to AC_INIT and removed from
# the call to AM_INIT_AUTOMAKE.
# We support both call styles for the transition.  After
# the next Automake release, Autoconf can make the AC_INIT
# arguments mandatory, and then we can depend on a new Autoconf
# release and drop the old call support.
AC_DEFUN([AM_INIT_AUTOMAKE],
[AC_REQUIRE([AM_SET_CURRENT_AUTOMAKE_VERSION])dnl
 AC_REQUIRE([AC_PROG_INSTALL])dnl
# test to see if srcdir already configured
if test "`cd $srcdir && pwd`" != "`pwd`" &&
   test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi

# test whether we have cygpath
if test -z "$CYGPATH_W"; then
  if (cygpath --version) >/dev/null 2>/dev/null; then
    CYGPATH_W='cygpath -w'
  else
    CYGPATH_W=echo
  fi
fi
AC_SUBST([CYGPATH_W])

# Define the identity of the package.
dnl Distinguish between old-style and new-style calls.
m4_ifval([$2],
[m4_ifval([$3], [_AM_SET_OPTION([no-define])])dnl
 AC_SUBST([PACKAGE], [$1])dnl
 AC_SUBST([VERSION], [$2])],
[_AM_SET_OPTIONS([$1])dnl
 AC_SUBST([PACKAGE], ['AC_PACKAGE_TARNAME'])dnl
 AC_SUBST([VERSION], ['AC_PACKAGE_VERSION'])])dnl

_AM_IF_OPTION([no-define],,
[AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
 AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package])])dnl

# Some tools Automake needs.
AC_REQUIRE([AM_SANITY_CHECK])dnl
AC_REQUIRE([AC_ARG_PROGRAM])dnl
AM_MISSING_PROG(ACLOCAL, aclocal-${am__api_version})
AM_MISSING_PROG(AUTOCONF, autoconf)
AM_MISSING_PROG(AUTOMAKE, automake-${am__api_version})
AM_MISSING_PROG(AUTOHEADER, autoheader)
AM_MISSING_PROG(MAKEINFO, makeinfo)
AM_MISSING_PROG(AMTAR, tar)
AM_PROG_INSTALL_SH
AM_PROG_INSTALL_STRIP
# We need awk for the "check" target.  The system "awk" is bad on
# some platforms.
AC_REQUIRE([AC_PROG_AWK])dnl
AC_REQUIRE([AC_PROG_MAKE_SET])dnl
AC_REQUIRE([AM_SET_LEADING_DOT])dnl

_AM_IF_OPTION([no-dependencies],,
[AC_PROVIDE_IFELSE([AC_PROG_CC],
                  [_AM_DEPENDENCIES(CC)],
                  [define([AC_PROG_CC],
                          defn([AC_PROG_CC])[_AM_DEPENDENCIES(CC)])])dnl
AC_PROVIDE_IFELSE([AC_PROG_CXX],
                  [_AM_DEPENDENCIES(CXX)],
                  [define([AC_PROG_CXX],
                          defn([AC_PROG_CXX])[_AM_DEPENDENCIES(CXX)])])dnl
])
])


# When config.status generates a header, we must update the stamp-h file.
# This file resides in the same directory as the config header
# that is generated.  The stamp files are numbered to have different names.

# Autoconf calls _AC_AM_CONFIG_HEADER_HOOK (when defined) in the
# loop where config.status creates the headers, so we can generate
# our stamp files there.
AC_DEFUN([_AC_AM_CONFIG_HEADER_HOOK],
[# Compute $1's index in $config_headers.
_am_stamp_count=1
for _am_header in $config_headers :; do
  case $_am_header in
    $1 | $1:* )
      break ;;
    * )
      _am_stamp_count=`expr $_am_stamp_count + 1` ;;
  esac
done
echo "timestamp for $1" >`AS_DIRNAME([$1])`/stamp-h[]$_am_stamp_count])

# Copyright 2002  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA

# AM_AUTOMAKE_VERSION(VERSION)
# ----------------------------
# Automake X.Y traces this macro to ensure aclocal.m4 has been
# generated from the m4 files accompanying Automake X.Y.
AC_DEFUN([AM_AUTOMAKE_VERSION],[am__api_version="1.7"])

# AM_SET_CURRENT_AUTOMAKE_VERSION
# -------------------------------
# Call AM_AUTOMAKE_VERSION so it can be traced.
# This function is AC_REQUIREd by AC_INIT_AUTOMAKE.
AC_DEFUN([AM_SET_CURRENT_AUTOMAKE_VERSION],
	 [AM_AUTOMAKE_VERSION([1.7.8])])

# Helper functions for option handling.                    -*- Autoconf -*-

# Copyright 2001, 2002  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 2

# _AM_MANGLE_OPTION(NAME)
# -----------------------
AC_DEFUN([_AM_MANGLE_OPTION],
[[_AM_OPTION_]m4_bpatsubst($1, [[^a-zA-Z0-9_]], [_])])

# _AM_SET_OPTION(NAME)
# ------------------------------
# Set option NAME.  Presently that only means defining a flag for this option.
AC_DEFUN([_AM_SET_OPTION],
[m4_define(_AM_MANGLE_OPTION([$1]), 1)])

# _AM_SET_OPTIONS(OPTIONS)
# ----------------------------------
# OPTIONS is a space-separated list of Automake options.
AC_DEFUN([_AM_SET_OPTIONS],
[AC_FOREACH([_AM_Option], [$1], [_AM_SET_OPTION(_AM_Option)])])

# _AM_IF_OPTION(OPTION, IF-SET, [IF-NOT-SET])
# -------------------------------------------
# Execute IF-SET if OPTION is set, IF-NOT-SET otherwise.
AC_DEFUN([_AM_IF_OPTION],
[m4_ifset(_AM_MANGLE_OPTION([$1]), [$2], [$3])])

#
# Check to make sure that the build environment is sane.
#

# Copyright 1996, 1997, 2000, 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

# AM_SANITY_CHECK
# ---------------
AC_DEFUN([AM_SANITY_CHECK],
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftest.file
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftest.file 2> /dev/null`
   if test "$[*]" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftest.file`
   fi
   rm -f conftest.file
   if test "$[*]" != "X $srcdir/configure conftest.file" \
      && test "$[*]" != "X conftest.file $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "$[2]" = conftest.file
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
AC_MSG_RESULT(yes)])

#  -*- Autoconf -*-


# Copyright 1997, 1999, 2000, 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 3

# AM_MISSING_PROG(NAME, PROGRAM)
# ------------------------------
AC_DEFUN([AM_MISSING_PROG],
[AC_REQUIRE([AM_MISSING_HAS_RUN])
$1=${$1-"${am_missing_run}$2"}
AC_SUBST($1)])


# AM_MISSING_HAS_RUN
# ------------------
# Define MISSING if not defined so far and test if it supports --run.
# If it does, set am_missing_run to use it, otherwise, to nothing.
AC_DEFUN([AM_MISSING_HAS_RUN],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
test x"${MISSING+set}" = xset || MISSING="\${SHELL} $am_aux_dir/missing"
# Use eval to expand $SHELL
if eval "$MISSING --run true"; then
  am_missing_run="$MISSING --run "
else
  am_missing_run=
  AC_MSG_WARN([`missing' script is too old or missing])
fi
])

# AM_AUX_DIR_EXPAND

# Copyright 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# For projects using AC_CONFIG_AUX_DIR([foo]), Autoconf sets
# $ac_aux_dir to `$srcdir/foo'.  In other projects, it is set to
# `$srcdir', `$srcdir/..', or `$srcdir/../..'.
#
# Of course, Automake must honor this variable whenever it calls a
# tool from the auxiliary directory.  The problem is that $srcdir (and
# therefore $ac_aux_dir as well) can be either absolute or relative,
# depending on how configure is run.  This is pretty annoying, since
# it makes $ac_aux_dir quite unusable in subdirectories: in the top
# source directory, any form will work fine, but in subdirectories a
# relative path needs to be adjusted first.
#
# $ac_aux_dir/missing
#    fails when called from a subdirectory if $ac_aux_dir is relative
# $top_srcdir/$ac_aux_dir/missing
#    fails if $ac_aux_dir is absolute,
#    fails when called from a subdirectory in a VPATH build with
#          a relative $ac_aux_dir
#
# The reason of the latter failure is that $top_srcdir and $ac_aux_dir
# are both prefixed by $srcdir.  In an in-source build this is usually
# harmless because $srcdir is `.', but things will broke when you
# start a VPATH build or use an absolute $srcdir.
#
# So we could use something similar to $top_srcdir/$ac_aux_dir/missing,
# iff we strip the leading $srcdir from $ac_aux_dir.  That would be:
#   am_aux_dir='\$(top_srcdir)/'`expr "$ac_aux_dir" : "$srcdir//*\(.*\)"`
# and then we would define $MISSING as
#   MISSING="\${SHELL} $am_aux_dir/missing"
# This will work as long as MISSING is not called from configure, because
# unfortunately $(top_srcdir) has no meaning in configure.
# However there are other variables, like CC, which are often used in
# configure, and could therefore not use this "fixed" $ac_aux_dir.
#
# Another solution, used here, is to always expand $ac_aux_dir to an
# absolute PATH.  The drawback is that using absolute paths prevent a
# configured tree to be moved without reconfiguration.

# Rely on autoconf to set up CDPATH properly.
AC_PREREQ([2.50])

AC_DEFUN([AM_AUX_DIR_EXPAND], [
# expand $ac_aux_dir to an absolute path
am_aux_dir=`cd $ac_aux_dir && pwd`
])

# AM_PROG_INSTALL_SH
# ------------------
# Define $install_sh.

# Copyright 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

AC_DEFUN([AM_PROG_INSTALL_SH],
[AC_REQUIRE([AM_AUX_DIR_EXPAND])dnl
install_sh=${install_sh-"$am_aux_dir/install-sh"}
AC_SUBST(install_sh)])

# AM_PROG_INSTALL_STRIP

# Copyright 2001 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# One issue with vendor `install' (even GNU) is that you can't
# specify the program used to strip binaries.  This is especially
# annoying in cross-compiling environments, where the build's strip
# is unlikely to handle the host's binaries.
# Fortunately install-sh will honor a STRIPPROG variable, so we
# always use install-sh in `make install-strip', and initialize
# STRIPPROG with the value of the STRIP variable (set by the user).
AC_DEFUN([AM_PROG_INSTALL_STRIP],
[AC_REQUIRE([AM_PROG_INSTALL_SH])dnl
# Installed binaries are usually stripped using `strip' when the user
# run `make install-strip'.  However `strip' might not be the right
# tool to use in cross-compilation environments, therefore Automake
# will honor the `STRIP' environment variable to overrule this program.
dnl Don't test for $cross_compiling = yes, because it might be `maybe'.
if test "$cross_compiling" != no; then
  AC_CHECK_TOOL([STRIP], [strip], :)
fi
INSTALL_STRIP_PROGRAM="\${SHELL} \$(install_sh) -c -s"
AC_SUBST([INSTALL_STRIP_PROGRAM])])

#                                                          -*- Autoconf -*-
# Copyright (C) 2003  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 1

# Check whether the underlying file-system supports filenames
# with a leading dot.  For instance MS-DOS doesn't.
AC_DEFUN([AM_SET_LEADING_DOT],
[rm -rf .tst 2>/dev/null
mkdir .tst 2>/dev/null
if test -d .tst; then
  am__leading_dot=.
else
  am__leading_dot=_
fi
rmdir .tst 2>/dev/null
AC_SUBST([am__leading_dot])])

# serial 5						-*- Autoconf -*-

# Copyright (C) 1999, 2000, 2001, 2002, 2003  Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.


# There are a few dirty hacks below to avoid letting `AC_PROG_CC' be
# written in clear, in which case automake, when reading aclocal.m4,
# will think it sees a *use*, and therefore will trigger all it's
# C support machinery.  Also note that it means that autoscan, seeing
# CC etc. in the Makefile, will ask for an AC_PROG_CC use...



# _AM_DEPENDENCIES(NAME)
# ----------------------
# See how the compiler implements dependency checking.
# NAME is "CC", "CXX", "GCJ", or "OBJC".
# We try a few techniques and use that to set a single cache variable.
#
# We don't AC_REQUIRE the corresponding AC_PROG_CC since the latter was
# modified to invoke _AM_DEPENDENCIES(CC); we would have a circular
# dependency, and given that the user is not expected to run this macro,
# just rely on AC_PROG_CC.
AC_DEFUN([_AM_DEPENDENCIES],
[AC_REQUIRE([AM_SET_DEPDIR])dnl
AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])dnl
AC_REQUIRE([AM_MAKE_INCLUDE])dnl
AC_REQUIRE([AM_DEP_TRACK])dnl

ifelse([$1], CC,   [depcc="$CC"   am_compiler_list=],
       [$1], CXX,  [depcc="$CXX"  am_compiler_list=],
       [$1], OBJC, [depcc="$OBJC" am_compiler_list='gcc3 gcc'],
       [$1], GCJ,  [depcc="$GCJ"  am_compiler_list='gcc3 gcc'],
                   [depcc="$$1"   am_compiler_list=])

AC_CACHE_CHECK([dependency style of $depcc],
               [am_cv_$1_dependencies_compiler_type],
[if test -z "$AMDEP_TRUE" && test -f "$am_depcomp"; then
  # We make a subdir and do the tests there.  Otherwise we can end up
  # making bogus files that we don't know about and never remove.  For
  # instance it was reported that on HP-UX the gcc test will end up
  # making a dummy file named `D' -- because `-MD' means `put the output
  # in D'.
  mkdir conftest.dir
  # Copy depcomp to subdir because otherwise we won't find it if we're
  # using a relative directory.
  cp "$am_depcomp" conftest.dir
  cd conftest.dir
  # We will build objects and dependencies in a subdirectory because
  # it helps to detect inapplicable dependency modes.  For instance
  # both Tru64's cc and ICC support -MD to output dependencies as a
  # side effect of compilation, but ICC will put the dependencies in
  # the current directory while Tru64 will put them in the object
  # directory.
  mkdir sub

  am_cv_$1_dependencies_compiler_type=none
  if test "$am_compiler_list" = ""; then
     am_compiler_list=`sed -n ['s/^#*\([a-zA-Z0-9]*\))$/\1/p'] < ./depcomp`
  fi
  for depmode in $am_compiler_list; do
    # Setup a source with many dependencies, because some compilers
    # like to wrap large dependency lists on column 80 (with \), and
    # we should not choose a depcomp mode which is confused by this.
    #
    # We need to recreate these files for each test, as the compiler may
    # overwrite some of them when testing with obscure command lines.
    # This happens at least with the AIX C compiler.
    : > sub/conftest.c
    for i in 1 2 3 4 5 6; do
      echo '#include "conftst'$i'.h"' >> sub/conftest.c
      : > sub/conftst$i.h
    done
    echo "${am__include} ${am__quote}sub/conftest.Po${am__quote}" > confmf

    case $depmode in
    nosideeffect)
      # after this tag, mechanisms are not by side-effect, so they'll
      # only be used when explicitly requested
      if test "x$enable_dependency_tracking" = xyes; then
	continue
      else
	break
      fi
      ;;
    none) break ;;
    esac
    # We check with `-c' and `-o' for the sake of the "dashmstdout"
    # mode.  It turns out that the SunPro C++ compiler does not properly
    # handle `-M -o', and we need to detect this.
    if depmode=$depmode \
       source=sub/conftest.c object=sub/conftest.${OBJEXT-o} \
       depfile=sub/conftest.Po tmpdepfile=sub/conftest.TPo \
       $SHELL ./depcomp $depcc -c -o sub/conftest.${OBJEXT-o} sub/conftest.c \
         >/dev/null 2>conftest.err &&
       grep sub/conftst6.h sub/conftest.Po > /dev/null 2>&1 &&
       grep sub/conftest.${OBJEXT-o} sub/conftest.Po > /dev/null 2>&1 &&
       ${MAKE-make} -s -f confmf > /dev/null 2>&1; then
      # icc doesn't choke on unknown options, it will just issue warnings
      # (even with -Werror).  So we grep stderr for any message
      # that says an option was ignored.
      if grep 'ignoring option' conftest.err >/dev/null 2>&1; then :; else
        am_cv_$1_dependencies_compiler_type=$depmode
        break
      fi
    fi
  done

  cd ..
  rm -rf conftest.dir
else
  am_cv_$1_dependencies_compiler_type=none
fi
])
AC_SUBST([$1DEPMODE], [depmode=$am_cv_$1_dependencies_compiler_type])
AM_CONDITIONAL([am__fastdep$1], [
  test "x$enable_dependency_tracking" != xno \
  && test "$am_cv_$1_dependencies_compiler_type" = gcc3])
])


# AM_SET_DEPDIR
# -------------
# Choose a directory name for dependency files.
# This macro is AC_REQUIREd in _AM_DEPENDENCIES
AC_DEFUN([AM_SET_DEPDIR],
[AC_REQUIRE([AM_SET_LEADING_DOT])dnl
AC_SUBST([DEPDIR], ["${am__leading_dot}deps"])dnl
])


# AM_DEP_TRACK
# ------------
AC_DEFUN([AM_DEP_TRACK],
[AC_ARG_ENABLE(dependency-tracking,
[  --disable-dependency-tracking Speeds up one-time builds
  --enable-dependency-tracking  Do not reject slow dependency extractors])
if test "x$enable_dependency_tracking" != xno; then
  am_depcomp="$ac_aux_dir/depcomp"
  AMDEPBACKSLASH='\'
fi
AM_CONDITIONAL([AMDEP], [test "x$enable_dependency_tracking" != xno])
AC_SUBST([AMDEPBACKSLASH])
])

# Generate code to set up dependency tracking.   -*- Autoconf -*-

# Copyright 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

#serial 2

# _AM_OUTPUT_DEPENDENCY_COMMANDS
# ------------------------------
AC_DEFUN([_AM_OUTPUT_DEPENDENCY_COMMANDS],
[for mf in $CONFIG_FILES; do
  # Strip MF so we end up with the name of the file.
  mf=`echo "$mf" | sed -e 's/:.*$//'`
  # Check whether this is an Automake generated Makefile or not.
  # We used to match only the files named `Makefile.in', but
  # some people rename them; so instead we look at the file content.
  # Grep'ing the first line is not enough: some people post-process
  # each Makefile.in and add a new line on top of each file to say so.
  # So let's grep whole file.
  if grep '^#.*generated by automake' $mf > /dev/null 2>&1; then
    dirpart=`AS_DIRNAME("$mf")`
  else
    continue
  fi
  grep '^DEP_FILES *= *[[^ @%:@]]' < "$mf" > /dev/null || continue
  # Extract the definition of DEP_FILES from the Makefile without
  # running `make'.
  DEPDIR=`sed -n -e '/^DEPDIR = / s///p' < "$mf"`
  test -z "$DEPDIR" && continue
  # When using ansi2knr, U may be empty or an underscore; expand it
  U=`sed -n -e '/^U = / s///p' < "$mf"`
  test -d "$dirpart/$DEPDIR" || mkdir "$dirpart/$DEPDIR"
  # We invoke sed twice because it is the simplest approach to
  # changing $(DEPDIR) to its actual value in the expansion.
  for file in `sed -n -e '
    /^DEP_FILES = .*\\\\$/ {
      s/^DEP_FILES = //
      :loop
	s/\\\\$//
	p
	n
	/\\\\$/ b loop
      p
    }
    /^DEP_FILES = / s/^DEP_FILES = //p' < "$mf" | \
       sed -e 's/\$(DEPDIR)/'"$DEPDIR"'/g' -e 's/\$U/'"$U"'/g'`; do
    # Make sure the directory exists.
    test -f "$dirpart/$file" && continue
    fdir=`AS_DIRNAME(["$file"])`
    AS_MKDIR_P([$dirpart/$fdir])
    # echo "creating $dirpart/$file"
    echo '# dummy' > "$dirpart/$file"
  done
done
])# _AM_OUTPUT_DEPENDENCY_COMMANDS


# AM_OUTPUT_DEPENDENCY_COMMANDS
# -----------------------------
# This macro should only be invoked once -- use via AC_REQUIRE.
#
# This code is only required when automatic dependency tracking
# is enabled.  FIXME.  This creates each `.P' file that we will
# need in order to bootstrap the dependency handling code.
AC_DEFUN([AM_OUTPUT_DEPENDENCY_COMMANDS],
[AC_CONFIG_COMMANDS([depfiles],
     [test x"$AMDEP_TRUE" != x"" || _AM_OUTPUT_DEPENDENCY_COMMANDS],
     [AMDEP_TRUE="$AMDEP_TRUE" ac_aux_dir="$ac_aux_dir"])
])

# Check to see how 'make' treats includes.	-*- Autoconf -*-

# Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

# serial 2

# AM_MAKE_INCLUDE()
# -----------------
# Check to see how make treats includes.
AC_DEFUN([AM_MAKE_INCLUDE],
[am_make=${MAKE-make}
cat > confinc << 'END'
am__doit:
	@echo done
.PHONY: am__doit
END
# If we don't find an include directive, just comment out the code.
AC_MSG_CHECKING([for style of include used by $am_make])
am__include="#"
am__quote=
_am_result=none
# First try GNU make style include.
echo "include confinc" > confmf
# We grep out `Entering directory' and `Leaving directory'
# messages which can occur if `w' ends up in MAKEFLAGS.
# In particular we don't look at `^make:' because GNU make might
# be invoked under some other name (usually "gmake"), in which
# case it prints its new name instead of `make'.
if test "`$am_make -s -f confmf 2> /dev/null | grep -v 'ing directory'`" = "done"; then
   am__include=include
   am__quote=
   _am_result=GNU
fi
# Now try BSD make style include.
if test "$am__include" = "#"; then
   echo '.include "confinc"' > confmf
   if test "`$am_make -s -f confmf 2> /dev/null`" = "done"; then
      am__include=.include
      am__quote="\""
      _am_result=BSD
   fi
fi
AC_SUBST([am__include])
AC_SUBST([am__quote])
AC_MSG_RESULT([$_am_result])
rm -f confinc confmf
])

