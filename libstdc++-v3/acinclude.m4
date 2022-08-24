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
  m4_define([glibcxx_SUBDIRS],[include libsupc++ src src/c++98 src/c++11 src/c++17 src/c++20 src/filesystem src/libbacktrace doc po testsuite python])
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

  # Check for -ffunction-sections -fdata-sections
  AC_MSG_CHECKING([for g++ that supports -ffunction-sections -fdata-sections])
  CXXFLAGS='-g -Werror -ffunction-sections -fdata-sections'
  AC_TRY_COMPILE([int foo; void bar() { };],, [ac_fdsections=yes], [ac_fdsections=no])
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
  AC_SUBST(SECTION_FLAGS)
])


dnl
dnl If GNU ld is in use, check to see if tricky linker opts can be used.  If
dnl the native linker is in use, all variables will be defined to something
dnl safe (like an empty string).
dnl
dnl Defines:
dnl  SECTION_LDFLAGS='-Wl,--gc-sections' if possible
dnl  OPT_LDFLAGS='-Wl,-O1' and '-z,relro' if possible
dnl  LD (as a side effect of testing)
dnl Sets:
dnl  with_gnu_ld
dnl  glibcxx_ld_is_gold (set to "no" or "yes")
dnl  glibcxx_ld_is_mold (set to "no" or "yes")
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
  glibcxx_ld_is_gold=no
  glibcxx_ld_is_mold=no
  if test x"$with_gnu_ld" = x"yes"; then
    AC_MSG_CHECKING([for ld version])
    changequote(,)
    if $LD --version 2>/dev/null | grep 'GNU gold' >/dev/null 2>&1; then
      glibcxx_ld_is_gold=yes
    elif $LD --version 2>/dev/null | grep 'mold' >/dev/null 2>&1; then
      glibcxx_ld_is_mold=yes
    fi
    ldver=`$LD --version 2>/dev/null |
	   sed -e 's/[. ][0-9]\{8\}$//;s/.* \([^ ]\{1,\}\)$/\1/; q'`
    changequote([,])
    glibcxx_gnu_ld_version=`echo $ldver | \
	   $AWK -F. '{ if (NF<3) [$]3=0; print ([$]1*100+[$]2)*100+[$]3 }'`
    AC_MSG_RESULT($glibcxx_gnu_ld_version)
  fi

  # Set --gc-sections.
  glibcxx_have_gc_sections=no
  if test "$glibcxx_ld_is_gold" = "yes" || test "$glibcxx_ld_is_mold" = "yes" ; then
    if $LD --help 2>/dev/null | grep gc-sections >/dev/null 2>&1; then
      glibcxx_have_gc_sections=yes
    fi
  else
    glibcxx_gcsections_min_ld=21602
    if test x"$with_gnu_ld" = x"yes" &&
	test $glibcxx_gnu_ld_version -gt $glibcxx_gcsections_min_ld ; then
      glibcxx_have_gc_sections=yes
    fi
  fi
  if test "$glibcxx_have_gc_sections" = "yes"; then
    # Sufficiently young GNU ld it is!  Joy and bunny rabbits!
    # NB: This flag only works reliably after 2.16.1. Configure tests
    # for this are difficult, so hard wire a value that should work.

    ac_test_CFLAGS="${CFLAGS+set}"
    ac_save_CFLAGS="$CFLAGS"
    CFLAGS='-Wl,--gc-sections'

    # Check for -Wl,--gc-sections
    AC_MSG_CHECKING([for ld that supports -Wl,--gc-sections])
    AC_TRY_LINK([ int one(void) { return 1; }
     int two(void) { return 2; }
	], [ two(); ] , [ac_gcsections=yes], [ac_gcsections=no])
    if test "$ac_gcsections" = "yes"; then
      rm -f conftest.c
      touch conftest.c
      if $CC -c conftest.c; then
	if $LD --gc-sections -o conftest conftest.o 2>&1 | \
	   grep "Warning: gc-sections option ignored" > /dev/null; then
	  ac_gcsections=no
	fi
      fi
      rm -f conftest.c conftest.o conftest
    fi
    if test "$ac_gcsections" = "yes"; then
      SECTION_LDFLAGS="-Wl,--gc-sections $SECTION_LDFLAGS"
    fi
    AC_MSG_RESULT($ac_gcsections)

    if test "$ac_test_CFLAGS" = set; then
      CFLAGS="$ac_save_CFLAGS"
    else
      # this is the suspicious part
      CFLAGS=''
    fi
  fi

  # Set -z,relro.
  # Note this is only for shared objects.
  ac_ld_relro=no
  if test x"$with_gnu_ld" = x"yes"; then
    # cygwin and mingw uses PE, which has no ELF relro support,
    # multi target ld may confuse configure machinery
    case "$host" in
    *-*-cygwin*)
     ;;
    *-*-mingw*)
     ;;
    *)
      AC_MSG_CHECKING([for ld that supports -Wl,-z,relro])
      cxx_z_relo=`$LD -v --help 2>/dev/null | grep "z relro"`
      if test -n "$cxx_z_relo"; then
        OPT_LDFLAGS="-Wl,-z,relro"
        ac_ld_relro=yes
      fi
      AC_MSG_RESULT($ac_ld_relro)
    esac
  fi

  # Set linker optimization flags.
  if test x"$with_gnu_ld" = x"yes"; then
    OPT_LDFLAGS="-Wl,-O1 $OPT_LDFLAGS"
  fi

  AC_SUBST(SECTION_LDFLAGS)
  AC_SUBST(OPT_LDFLAGS)
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
  AC_MSG_CHECKING([for RLIMIT_$1])
  AC_TRY_COMPILE(
    [#include <unistd.h>
     #include <sys/time.h>
     #include <sys/resource.h>
    ],
    [ int f = RLIMIT_$1 ; ],
    [glibcxx_mresult=1], [glibcxx_mresult=0])
  AC_DEFINE_UNQUOTED(HAVE_LIMIT_$1, $glibcxx_mresult,
		     [Only used in build directory testsuite_hooks.h.])
  if test $glibcxx_mresult = 1 ; then res=yes ; else res=no ; fi
  AC_MSG_RESULT($res)
])

AC_DEFUN([GLIBCXX_CHECK_SETRLIMIT], [
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
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
    AC_CACHE_CHECK([for testsuite resource limits support],
      glibcxx_cv_setrlimit, [
      AC_TRY_COMPILE(
	[#include <unistd.h>
	 #include <sys/time.h>
	 #include <sys/resource.h>
	],
	[struct rlimit r;
	 setrlimit(0, &r);],
	[glibcxx_cv_setrlimit=yes], [glibcxx_cv_setrlimit=no])
    ])

    if test $glibcxx_cv_setrlimit = yes; then
      AC_DEFINE(_GLIBCXX_RES_LIMITS, 1,
		[Define if using setrlimit to set resource limits during
		"make check"])
    fi
  fi
  AC_LANG_RESTORE
])


dnl
dnl Check whether S_ISREG (Posix) or S_IFREG is available in <sys/stat.h>.
dnl Define HAVE_S_ISREG / HAVE_S_IFREG appropriately.
dnl
AC_DEFUN([GLIBCXX_CHECK_S_ISREG_OR_S_IFREG], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_MSG_CHECKING([for S_ISREG or S_IFREG])
  AC_CACHE_VAL(glibcxx_cv_S_ISREG, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <sys/stat.h>],
      [struct stat buffer;
       fstat(0, &buffer);
       S_ISREG(buffer.st_mode);],
      [glibcxx_cv_S_ISREG=yes],
      [glibcxx_cv_S_ISREG=no])
  ])
  AC_CACHE_VAL(glibcxx_cv_S_IFREG, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <sys/stat.h>],
      [struct stat buffer;
       fstat(0, &buffer);
       S_IFREG & buffer.st_mode;],
      [glibcxx_cv_S_IFREG=yes],
      [glibcxx_cv_S_IFREG=no])
  ])
  res=no
  if test $glibcxx_cv_S_ISREG = yes; then
    AC_DEFINE(HAVE_S_ISREG, 1,
	      [Define if S_ISREG is available in <sys/stat.h>.])
    res=S_ISREG
  elif test $glibcxx_cv_S_IFREG = yes; then
    AC_DEFINE(HAVE_S_IFREG, 1,
	      [Define if S_IFREG is available in <sys/stat.h>.])
    res=S_IFREG
  fi
  AC_MSG_RESULT($res)

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Check whether poll is available in <poll.h>, and define HAVE_POLL.
dnl
AC_DEFUN([GLIBCXX_CHECK_POLL], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for poll], glibcxx_cv_POLL, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <poll.h>],
      [struct pollfd pfd[1];
       pfd[0].events = POLLIN;
       poll(pfd, 1, 0);],
      [glibcxx_cv_POLL=yes],
      [glibcxx_cv_POLL=no])
  ])
  if test $glibcxx_cv_POLL = yes; then
    AC_DEFINE(HAVE_POLL, 1, [Define if poll is available in <poll.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Check whether writev is available in <sys/uio.h>, and define HAVE_WRITEV.
dnl
AC_DEFUN([GLIBCXX_CHECK_WRITEV], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for writev], glibcxx_cv_WRITEV, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <sys/uio.h>],
      [struct iovec iov[2];
       writev(0, iov, 0);],
      [glibcxx_cv_WRITEV=yes],
      [glibcxx_cv_WRITEV=no])
  ])
  if test $glibcxx_cv_WRITEV = yes; then
    AC_DEFINE(HAVE_WRITEV, 1, [Define if writev is available in <sys/uio.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Check whether LFS support is available.
dnl
AC_DEFUN([GLIBCXX_CHECK_LFS], [
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
  AC_CACHE_CHECK([for LFS support], glibcxx_cv_LFS, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>
       #include <stdio.h>
       #include <sys/stat.h>
      ],
      [FILE* fp;
       fopen64("t", "w");
       fseeko64(fp, 0, SEEK_CUR);
       ftello64(fp);
       lseek64(1, 0, SEEK_CUR);
       struct stat64 buf;
       fstat64(1, &buf);],
      [glibcxx_cv_LFS=yes],
      [glibcxx_cv_LFS=no])
  ])
  if test $glibcxx_cv_LFS = yes; then
    AC_DEFINE(_GLIBCXX_USE_LFS, 1, [Define if LFS support is available.])
  fi
  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Check whether the old Copy-On-Write basic_string should allocate a new
dnl empty representation for every default-constructed basic_string. Without
dnl this option, COW strings share a single empty rep in static storage,
dnl but this only works if the linker can guarantee the static storage has
dnl a unique definition in the process. It also doesn't work if basic_string
dnl objects are stored in shared memory (see PR libstdc++/16612).
dnl When fully dynamic strings are enabled, the static storage is not used
dnl and a new empty string with reference-count == 1 is allocated each time.
dnl Enabling this changes the libstdc++.so ABI.
dnl
dnl --enable-fully-dynamic-string defines _GLIBCXX_FULLY_DYNAMIC_STRING to 1
dnl --disable-fully-dynamic-string defines _GLIBCXX_FULLY_DYNAMIC_STRING to 0
dnl otherwise the macro is not defined.
dnl  +  Usage:  GLIBCXX_ENABLE_FULLY_DYNAMIC_STRING[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_FULLY_DYNAMIC_STRING], [
  GLIBCXX_ENABLE(fully-dynamic-string,$1,,[do not put empty strings in per-process static memory], [permit yes|no])
  if test $enable_fully_dynamic_string = yes; then
    enable_fully_dynamic_string_def=1
  else
    enable_fully_dynamic_string_def=0
  fi
  AC_DEFINE_UNQUOTED([_GLIBCXX_FULLY_DYNAMIC_STRING], [${enable_fully_dynamic_string_def}],
	      [Define to 1 if a fully dynamic basic_string is wanted, 0 to disable, undefined for platform defaults])
])


dnl
dnl Does any necessary configuration of the testsuite directory.  Generates
dnl the testsuite_hooks.h header.
dnl
dnl GLIBCXX_ENABLE_SYMVERS and GLIBCXX_IS_NATIVE must be done before this.
dnl
dnl Sets:
dnl  enable_abi_check
dnl  GLIBCXX_TEST_WCHAR_T
dnl  GLIBCXX_TEST_THREAD
dnl Substs:
dnl  baseline_dir
dnl  baseline_subdir_switch
dnl
AC_DEFUN([GLIBCXX_CONFIGURE_TESTSUITE], [
  # Do checks for resource limit functions.
  GLIBCXX_CHECK_SETRLIMIT

  if $GLIBCXX_IS_NATIVE ; then
    # Look for setenv, so that extended locale tests can be performed.
    GLIBCXX_CHECK_STDLIB_DECL_AND_LINKAGE_3(setenv)
  fi

  if $GLIBCXX_IS_NATIVE && test $is_hosted = yes &&
     test $enable_symvers != no; then
    case "$host" in
      *-*-cygwin*)
	enable_abi_check=no ;;
      *)
	enable_abi_check=yes ;;
    esac
  else
    # Only build this as native, since automake does not understand
    # CXX_FOR_BUILD.
    enable_abi_check=no
  fi

  # Export file names for ABI checking.
  baseline_dir="$glibcxx_srcdir/config/abi/post/${abi_baseline_pair}"
  AC_SUBST(baseline_dir)
  baseline_subdir_switch="$abi_baseline_subdir_switch"
  AC_SUBST(baseline_subdir_switch)
])


dnl
dnl Does any necessary configuration for docbook in the docs directory.
dnl
dnl XSLTPROC must be set before this
dnl
dnl Sets:
dnl  glibcxx_stylesheets
dnl Substs:
dnl  XSL_STYLE_DIR
dnl
AC_DEFUN([GLIBCXX_CONFIGURE_DOCBOOK], [

glibcxx_docbook_url=http://docbook.sourceforge.net/release/xsl-ns/current/

AC_MSG_CHECKING([for local stylesheet directory])
glibcxx_local_stylesheets=no
if test x${XMLCATALOG} = xyes && xsl_style_dir=`xmlcatalog "" $glibcxx_docbook_url 2>/dev/null`
then
  XSL_STYLE_DIR=`echo $xsl_style_dir | sed -n 's;^file://;;p'`
  glibcxx_local_stylesheets=yes
else
  for dir in \
    /usr/share/sgml/docbook/xsl-ns-stylesheets \
    /usr/share/xml/docbook/stylesheet/docbook-xsl-ns \
    /usr/share/xml/docbook/stylesheet/nwalsh5/current \
    /usr/share/xml/docbook/stylesheet/nwalsh/current
  do
    if test -d $dir; then
      glibcxx_local_stylesheets=yes
      XSL_STYLE_DIR=$dir
      break
    fi
  done
fi
AC_MSG_RESULT($glibcxx_local_stylesheets)

if test x"$glibcxx_local_stylesheets" = x"yes"; then
  AC_SUBST(XSL_STYLE_DIR)
  AC_MSG_NOTICE($XSL_STYLE_DIR)

  AC_MSG_CHECKING([for docbook stylesheets for documentation creation])
  glibcxx_stylesheets=no
  if test x${XMLCATALOG} = xno || xmlcatalog "" $glibcxx_docbook_url/xhtml/docbook.xsl >/dev/null 2>&1; then
    if test x${XSLTPROC} = xyes && echo '<title/>' | xsltproc --noout --nonet --xinclude $glibcxx_docbook_url/xhtml/docbook.xsl - 2>/dev/null; then
      glibcxx_stylesheets=yes
    fi
  fi
  AC_MSG_RESULT($glibcxx_stylesheets)

else
  glibcxx_stylesheets=no
fi

# Check for epub3 dependencies.
AC_MSG_CHECKING([for epub3 stylesheets for documentation creation])
glibcxx_epub_stylesheets=no
if test x"$glibcxx_local_stylesheets" = x"yes"; then
   if test -f "$XSL_STYLE_DIR/epub3/chunk.xsl"; then
      glibcxx_epub_stylesheets=yes
   fi
fi
AC_MSG_RESULT($glibcxx_epub_stylesheets)
AM_CONDITIONAL(BUILD_EPUB, test x"$glibcxx_epub_stylesheets" = x"yes")

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
  # get unwind* headers from the libgcc dir.
  #TOPLEVEL_INCLUDES='-I$(toplevel_srcdir)/libgcc -I$(toplevel_srcdir)/include'
  TOPLEVEL_INCLUDES='-I$(toplevel_srcdir)/libgcc'

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
  # Alternatively OPTIMIZE_CXXFLAGS can be set in configure.host.
  # OPTIMIZE_CXXFLAGS = -O3 -fstrict-aliasing -fvtable-gc
  AC_SUBST(OPTIMIZE_CXXFLAGS)

  WARN_FLAGS="-Wall -Wextra -Wwrite-strings -Wcast-qual -Wabi=2"
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
dnl This logic must match gcc/configure.ac's setting of gcc_gxx_include_dir.
dnl config/gxx-include-dir.m4 must be kept consistant with this as well.
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

  GCC_WITH_TOOLEXECLIBDIR

  # Default case for install directory for include files.
  if test $version_specific_libs = no && test $gxx_include_dir = no; then
    gxx_include_dir='include/c++/${gcc_version}'
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      gxx_include_dir='${prefix}/${target_alias}/'"$gxx_include_dir"
    else
      gxx_include_dir='${prefix}/'"$gxx_include_dir"
    fi
  fi

  # Version-specific runtime libs processing.
  if test $version_specific_libs = yes; then
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.  FIXME: these variables are misnamed, there are
    # no executables installed in _toolexecdir or _toolexeclibdir.
    if test x"$gxx_include_dir" = x"no"; then
      gxx_include_dir='${libdir}/gcc/${host_alias}/${gcc_version}/include/c++'
    fi
    glibcxx_toolexecdir='${libdir}/gcc/${host_alias}'
    glibcxx_toolexeclibdir='${toolexecdir}/${gcc_version}$(MULTISUBDIR)'
  fi

  # Calculate glibcxx_toolexecdir, glibcxx_toolexeclibdir
  # Install a library built with a cross compiler in tooldir, not libdir.
  if test x"$glibcxx_toolexecdir" = x"no"; then
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      glibcxx_toolexecdir='${exec_prefix}/${host_alias}'
      case ${with_toolexeclibdir} in
	no)
	  glibcxx_toolexeclibdir='${toolexecdir}/lib'
	  ;;
	*)
	  glibcxx_toolexeclibdir=${with_toolexeclibdir}
	  ;;
      esac
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

  if test x"$enable_c99" = x"yes"; then
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS

    # Use -std=c++98 (instead of -std=gnu++98) because leaving __STRICT_ANSI__
    # undefined may cause fake C99 facilities, like pre-standard snprintf,
    # to be spuriously enabled.
    ac_save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS -std=c++98"
    ac_save_LIBS="$LIBS"
    ac_save_gcc_no_link="$gcc_no_link"

    if test x$gcc_no_link != xyes; then
      # Use -fno-exceptions to that the C driver can link these tests without
      # hitting undefined references to personality routines.
      CXXFLAGS="$CXXFLAGS -fno-exceptions"
      AC_CHECK_LIB(m, sin, [LIBS="$LIBS -lm"], [
        # Use the default compile-only tests in GCC_TRY_COMPILE_OR_LINK
        gcc_no_link=yes
      ])
    fi

    # Check for the existence of <math.h> functions used if C99 is enabled.
    AC_CACHE_CHECK([for ISO C99 support in <math.h> for C++98],
      glibcxx_cv_c99_math_cxx98, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <math.h>
         volatile double d1, d2;
         volatile int i;],
        [i = fpclassify(d1);
         i = isfinite(d1);
         i = isinf(d1);
         i = isnan(d1);
         i = isnormal(d1);
         i = signbit(d1);
         i = isgreater(d1, d2);
         i = isgreaterequal(d1, d2);
         i = isless(d1, d2);
         i = islessequal(d1, d2);
         i = islessgreater(d1, d2);
         i = islessgreater(d1, d2);
         i = isunordered(d1, d2);
        ], [glibcxx_cv_c99_math_cxx98=yes], [glibcxx_cv_c99_math_cxx98=no])
    ])
    if test x"$glibcxx_cv_c99_math_cxx98" = x"yes"; then
      AC_DEFINE(_GLIBCXX98_USE_C99_MATH, 1,
        [Define if C99 functions or macros in <math.h> should be imported
        in <cmath> in namespace std for C++98.])
    fi

    # Check for the existence of <complex.h> complex math functions.
    # This is necessary even though libstdc++ uses the builtin versions
    # of these functions, because if the builtin cannot be used, a reference
    # to the library function is emitted.
    AC_CHECK_HEADERS(tgmath.h, ac_has_tgmath_h=yes, ac_has_tgmath_h=no)
    AC_CHECK_HEADERS(complex.h, ac_has_complex_h=yes, ac_has_complex_h=no)
    if test x"$ac_has_complex_h" = x"yes"; then
      AC_CACHE_CHECK([for ISO C99 support in <complex.h> for C++98],
	glibcxx_cv_c99_complex_cxx98, [
        GCC_TRY_COMPILE_OR_LINK(
          [#include <complex.h>
           typedef __complex__ float float_type;
           typedef __complex__ double double_type;
           typedef __complex__ long double ld_type;
           volatile float_type tmpf;
           volatile double_type tmpd;
           volatile ld_type tmpld;
           volatile float f;
           volatile double d;
           volatile long double ld;],
          [f = cabsf(tmpf);
           f = cargf(tmpf);
           tmpf = ccosf(tmpf);
           tmpf = ccoshf(tmpf);
           tmpf = cexpf(tmpf);
           tmpf = clogf(tmpf);
           tmpf = csinf(tmpf);
           tmpf = csinhf(tmpf);
           tmpf = csqrtf(tmpf);
           tmpf = ctanf(tmpf);
           tmpf = ctanhf(tmpf);
           tmpf = cpowf(tmpf, tmpf);
           tmpf = cprojf(tmpf);
           d = cabs(tmpd);
           d = carg(tmpd);
           tmpd = ccos(tmpd);
           tmpd = ccosh(tmpd);
           tmpd = cexp(tmpd);
           tmpd = clog(tmpd);
           tmpd = csin(tmpd);
           tmpd = csinh(tmpd);
           tmpd = csqrt(tmpd);
           tmpd = ctan(tmpd);
           tmpd = ctanh(tmpd);
           tmpd = cpow(tmpd, tmpd);
           tmpd = cproj(tmpd);
           ld = cabsl(tmpld);
           ld = cargl(tmpld);
           tmpld = ccosl(tmpld);
           tmpld = ccoshl(tmpld);
           tmpld = cexpl(tmpld);
           tmpld = clogl(tmpld);
           tmpld = csinl(tmpld);
           tmpld = csinhl(tmpld);
           tmpld = csqrtl(tmpld);
           tmpld = ctanl(tmpld);
           tmpld = ctanhl(tmpld);
           tmpld = cpowl(tmpld, tmpld);
           tmpld = cprojl(tmpld);
          ], [glibcxx_cv_c99_complex_cxx98=yes], [glibcxx_cv_c99_complex_cxx98=no])
      ])
    fi
    if test x"$glibcxx_cv_c99_complex_cxx98" = x"yes"; then
      AC_DEFINE(_GLIBCXX98_USE_C99_COMPLEX, 1,
        [Define if C99 functions in <complex.h> should be used in
        <complex> for C++98. Using compiler builtins for these functions
        requires corresponding C99 library functions to be present.])
    fi

    # Check for the existence in <stdio.h> of vscanf, et. al.
    AC_CACHE_CHECK([for ISO C99 support in <stdio.h> for C++98],
      glibcxx_cv_c99_stdio_cxx98, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <stdio.h>
         #include <stdarg.h>
         void foo(char* fmt, ...)
         {
           va_list args; va_start(args, fmt);
           vfscanf(stderr, "%i", args);
           vscanf("%i", args);
           vsnprintf(fmt, 0, "%i", args);
           vsscanf(fmt, "%i", args);
           snprintf(fmt, 0, "%i");
         }], [],
        [glibcxx_cv_c99_stdio_cxx98=yes], [glibcxx_cv_c99_stdio_cxx98=no])
    ])
    if test x"$glibcxx_cv_c99_stdio_cxx98" = x"yes"; then
      AC_DEFINE(_GLIBCXX98_USE_C99_STDIO, 1,
        [Define if C99 functions or macros in <stdio.h> should be imported
        in <cstdio> in namespace std for C++98.])
    fi

    # Check for the existence in <stdlib.h> of lldiv_t, et. al.
    AC_CACHE_CHECK([for ISO C99 support in <stdlib.h> for C++98],
      glibcxx_cv_c99_stdlib_cxx98, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <stdlib.h>
         volatile float f;
         volatile long double ld;
         volatile unsigned long long ll;
         lldiv_t mydivt;],
        [char* tmp;
         f = strtof("gnu", &tmp);
         ld = strtold("gnu", &tmp);
         ll = strtoll("gnu", &tmp, 10);
         ll = strtoull("gnu", &tmp, 10);
         ll = llabs(10);
         mydivt = lldiv(10,1);
         ll = mydivt.quot;
         ll = mydivt.rem;
         ll = atoll("10");
         _Exit(0);
        ], [glibcxx_cv_c99_stdlib_cxx98=yes], [glibcxx_cv_c99_stdlib_cxx98=no])
    ])
    if test x"$glibcxx_cv_c99_stdlib_cxx98" = x"yes"; then
      AC_DEFINE(_GLIBCXX98_USE_C99_STDLIB, 1,
        [Define if C99 functions or macros in <stdlib.h> should be imported
        in <cstdlib> in namespace std for C++98.])
    fi

    # Check for the existence in <wchar.h> of wcstold, etc.
    if test x"$ac_has_wchar_h" = xyes &&
       test x"$ac_has_wctype_h" = xyes; then
      AC_CACHE_CHECK([for ISO C99 support in <wchar.h> for C++98],
	glibcxx_cv_c99_wchar_cxx98, [
        AC_TRY_COMPILE([#include <wchar.h>
          namespace test
          {
            using ::wcstold;
            using ::wcstoll;
            using ::wcstoull;
          }
        ], [], [glibcxx_cv_c99_wchar_cxx98=yes], [glibcxx_cv_c99_wchar_cxx98=no])
      ])

      # Checks for wide character functions that may not be present.
      # Injection of these is wrapped with guard macros.
      # NB: only put functions here, instead of immediately above, if
      # absolutely necessary.
      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vfwscanf; }], [],
        [AC_DEFINE(HAVE_VFWSCANF, 1, [Defined if vfwscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vswscanf; }], [],
        [AC_DEFINE(HAVE_VSWSCANF, 1, [Defined if vswscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vwscanf; }], [],
        [AC_DEFINE(HAVE_VWSCANF, 1, [Defined if vwscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::wcstof; }], [],
        [AC_DEFINE(HAVE_WCSTOF, 1, [Defined if wcstof exists.])], [])

      AC_TRY_COMPILE([#include <wctype.h>],
        [wint_t t; int i = iswblank(t);],
        [AC_DEFINE(HAVE_ISWBLANK, 1, [Defined if iswblank exists.])], [])

      if test x"$glibcxx_cv_c99_wchar_cxx98" = x"yes"; then
        AC_DEFINE(_GLIBCXX98_USE_C99_WCHAR, 1,
          [Define if C99 functions or macros in <wchar.h> should be imported
          in <cwchar> in namespace std for C++98.])
      fi
    fi

    # Option parsed, now set things appropriately.
    if test x"$glibcxx_cv_c99_math_cxx98" = x"no" ||
       test x"$glibcxx_cv_c99_complex_cxx98" = x"no" ||
       test x"$glibcxx_cv_c99_stdio_cxx98" = x"no" ||
       test x"$glibcxx_cv_c99_stdlib_cxx98" = x"no" ||
       test x"$glibcxx_cv_c99_wchar_cxx98" = x"no"; then
      enable_c99=no;
    else
      AC_DEFINE(_GLIBCXX_USE_C99, 1,
        [Define if C99 functions or macros from <wchar.h>, <math.h>,
        <complex.h>, <stdio.h>, and <stdlib.h> can be used or exposed.])
    fi

    gcc_no_link="$ac_save_gcc_no_link"
    LIBS="$ac_save_LIBS"
    CXXFLAGS="$ac_save_CXXFLAGS"
    AC_LANG_RESTORE

    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS

    # Use -std=c++11 and test again for C99 library feature in C++11 mode.
    # For the reasons given above we use -std=c++11 not -std=gnu++11.
    ac_save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS -std=c++11"
    ac_save_LIBS="$LIBS"
    ac_save_gcc_no_link="$gcc_no_link"

    if test x$gcc_no_link != xyes; then
      # Use -fno-exceptions to that the C driver can link these tests without
      # hitting undefined references to personality routines.
      CXXFLAGS="$CXXFLAGS -fno-exceptions"
      AC_CHECK_LIB(m, sin, [LIBS="$LIBS -lm"], [
        # Use the default compile-only tests in GCC_TRY_COMPILE_OR_LINK
        gcc_no_link=yes
      ])
    fi

    # Check for the existence of <math.h> functions used if C99 is enabled.
    AC_CACHE_CHECK([for ISO C99 support in <math.h> for C++11],
    glibcxx_cv_c99_math_cxx11, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <math.h>
         volatile double d1, d2;
         volatile int i;],
        [i = fpclassify(d1);
         i = isfinite(d1);
         i = isinf(d1);
         i = isnan(d1);
         i = isnormal(d1);
         i = signbit(d1);
         i = isgreater(d1, d2);
         i = isgreaterequal(d1, d2);
         i = isless(d1, d2);
         i = islessequal(d1, d2);
         i = islessgreater(d1, d2);
         i = islessgreater(d1, d2);
         i = isunordered(d1, d2);
        ], [glibcxx_cv_c99_math_cxx11=yes], [glibcxx_cv_c99_math_cxx11=no])
    ])
    if test x"$glibcxx_cv_c99_math_cxx11" = x"yes"; then
      AC_DEFINE(_GLIBCXX11_USE_C99_MATH, 1,
        [Define if C99 functions or macros in <math.h> should be imported
        in <cmath> in namespace std for C++11.])
    fi

    # Check for the existence of <complex.h> complex math functions.
    # This is necessary even though libstdc++ uses the builtin versions
    # of these functions, because if the builtin cannot be used, a reference
    # to the library function is emitted.
    AC_CHECK_HEADERS(tgmath.h, ac_has_tgmath_h=yes, ac_has_tgmath_h=no)
    AC_CHECK_HEADERS(complex.h, ac_has_complex_h=yes, ac_has_complex_h=no)
    if test x"$ac_has_complex_h" = x"yes"; then
      AC_CACHE_CHECK([for ISO C99 support in <complex.h> for C++11],
	glibcxx_cv_c99_complex_cxx11, [
        GCC_TRY_COMPILE_OR_LINK(
          [#include <complex.h>
           typedef __complex__ float float_type;
           typedef __complex__ double double_type;
           typedef __complex__ long double ld_type;
           volatile float_type tmpf;
           volatile double_type tmpd;
           volatile ld_type tmpld;
           volatile float f;
           volatile double d;
           volatile long double ld;],
          [f = cabsf(tmpf);
           f = cargf(tmpf);
           tmpf = ccosf(tmpf);
           tmpf = ccoshf(tmpf);
           tmpf = cexpf(tmpf);
           tmpf = clogf(tmpf);
           tmpf = csinf(tmpf);
           tmpf = csinhf(tmpf);
           tmpf = csqrtf(tmpf);
           tmpf = ctanf(tmpf);
           tmpf = ctanhf(tmpf);
           tmpf = cpowf(tmpf, tmpf);
           tmpf = cprojf(tmpf);
           d = cabs(tmpd);
           d = carg(tmpd);
           tmpd = ccos(tmpd);
           tmpd = ccosh(tmpd);
           tmpd = cexp(tmpd);
           tmpd = clog(tmpd);
           tmpd = csin(tmpd);
           tmpd = csinh(tmpd);
           tmpd = csqrt(tmpd);
           tmpd = ctan(tmpd);
           tmpd = ctanh(tmpd);
           tmpd = cpow(tmpd, tmpd);
           tmpd = cproj(tmpd);
           ld = cabsl(tmpld);
           ld = cargl(tmpld);
           tmpld = ccosl(tmpld);
           tmpld = ccoshl(tmpld);
           tmpld = cexpl(tmpld);
           tmpld = clogl(tmpld);
           tmpld = csinl(tmpld);
           tmpld = csinhl(tmpld);
           tmpld = csqrtl(tmpld);
           tmpld = ctanl(tmpld);
           tmpld = ctanhl(tmpld);
           tmpld = cpowl(tmpld, tmpld);
           tmpld = cprojl(tmpld);
          ], [glibcxx_cv_c99_complex_cxx11=yes], [glibcxx_cv_c99_complex_cxx11=no])
      ])
    fi
    if test x"$glibcxx_cv_c99_complex_cxx11" = x"yes"; then
      AC_DEFINE(_GLIBCXX11_USE_C99_COMPLEX, 1,
        [Define if C99 functions in <complex.h> should be used in
        <complex> for C++11. Using compiler builtins for these functions
        requires corresponding C99 library functions to be present.])
    fi

    # Check for the existence in <stdio.h> of vscanf, et. al.
    AC_CACHE_CHECK([for ISO C99 support in <stdio.h> for C++11],
      glibcxx_cv_c99_stdio_cxx11, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <stdio.h>
         #include <stdarg.h>
         void foo(char* fmt, ...)
         {
           va_list args; va_start(args, fmt);
           vfscanf(stderr, "%i", args);
           vscanf("%i", args);
           vsnprintf(fmt, 0, "%i", args);
           vsscanf(fmt, "%i", args);
           snprintf(fmt, 0, "%i");
         }], [],
        [glibcxx_cv_c99_stdio_cxx11=yes], [glibcxx_cv_c99_stdio_cxx11=no])
    ])
    if test x"$glibcxx_cv_c99_stdio_cxx11" = x"yes"; then
      AC_DEFINE(_GLIBCXX11_USE_C99_STDIO, 1,
        [Define if C99 functions or macros in <stdio.h> should be imported
        in <cstdio> in namespace std for C++11.])
    fi

    # Check for the existence in <stdlib.h> of lldiv_t, et. al.
    AC_CACHE_CHECK([for ISO C99 support in <stdlib.h> for C++11],
      glibcxx_cv_c99_stdlib_cxx11, [
      GCC_TRY_COMPILE_OR_LINK(
        [#include <stdlib.h>
         volatile float f;
         volatile long double ld;
         volatile unsigned long long ll;
         lldiv_t mydivt;],
        [char* tmp;
         f = strtof("gnu", &tmp);
         ld = strtold("gnu", &tmp);
         ll = strtoll("gnu", &tmp, 10);
         ll = strtoull("gnu", &tmp, 10);
         ll = llabs(10);
         mydivt = lldiv(10,1);
         ll = mydivt.quot;
         ll = mydivt.rem;
         ll = atoll("10");
         _Exit(0);
        ], [glibcxx_cv_c99_stdlib_cxx11=yes], [glibcxx_cv_c99_stdlib_cxx11=no])
    ])
    if test x"$glibcxx_cv_c99_stdlib_cxx11" = x"yes"; then
      AC_DEFINE(_GLIBCXX11_USE_C99_STDLIB, 1,
        [Define if C99 functions or macros in <stdlib.h> should be imported
        in <cstdlib> in namespace std for C++11.])
    fi

    # Check for the existence in <wchar.h> of wcstold, etc.
    if test x"$ac_has_wchar_h" = xyes &&
       test x"$ac_has_wctype_h" = xyes; then
      AC_CACHE_CHECK([for ISO C99 support in <wchar.h> for C++11],
	glibcxx_cv_c99_wchar_cxx11, [
        AC_TRY_COMPILE([#include <wchar.h>
          namespace test
          {
            using ::wcstold;
            using ::wcstoll;
            using ::wcstoull;
          }
        ], [], [glibcxx_cv_c99_wchar_cxx11=yes], [glibcxx_cv_c99_wchar_cxx11=no])
      ])

      # Checks for wide character functions that may not be present.
      # Injection of these is wrapped with guard macros.
      # NB: only put functions here, instead of immediately above, if
      # absolutely necessary.
      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vfwscanf; }], [],
        [AC_DEFINE(HAVE_VFWSCANF, 1, [Defined if vfwscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vswscanf; }], [],
        [AC_DEFINE(HAVE_VSWSCANF, 1, [Defined if vswscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::vwscanf; }], [],
        [AC_DEFINE(HAVE_VWSCANF, 1, [Defined if vwscanf exists.])], [])

      AC_TRY_COMPILE([#include <wchar.h>
        namespace test { using ::wcstof; }], [],
        [AC_DEFINE(HAVE_WCSTOF, 1, [Defined if wcstof exists.])], [])

      AC_TRY_COMPILE([#include <wctype.h>],
        [wint_t t; int i = iswblank(t);],
        [AC_DEFINE(HAVE_ISWBLANK, 1, [Defined if iswblank exists.])], [])

      if test x"$glibcxx_cv_c99_wchar_cxx11" = x"yes"; then
        AC_DEFINE(_GLIBCXX11_USE_C99_WCHAR, 1,
          [Define if C99 functions or macros in <wchar.h> should be imported
          in <cwchar> in namespace std for C++11.])
      fi
    fi

    gcc_no_link="$ac_save_gcc_no_link"
    LIBS="$ac_save_LIBS"
    CXXFLAGS="$ac_save_CXXFLAGS"
    AC_LANG_RESTORE
  fi

  AC_MSG_CHECKING([for fully enabled ISO C99 support])
  AC_MSG_RESULT($enable_c99)
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

  if test x"$ac_has_nanosleep$ac_has_sleep" = x"nono"; then
      ac_no_sleep=yes
      AC_MSG_CHECKING([for Sleep])
      AC_TRY_COMPILE([#include <windows.h>],
                     [Sleep(1)],
                     [ac_has_win32_sleep=yes],[ac_has_win32_sleep=no])
      if test x"$ac_has_win32_sleep" = x"yes"; then
        AC_DEFINE(HAVE_WIN32_SLEEP,1, [Defined if Sleep exists.])
	ac_no_sleep=no
      fi
      AC_MSG_RESULT($ac_has_win32_sleep)
  fi

  if test x"$ac_no_sleep" = x"yes"; then
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
dnl Check for ISO/IEC 9899:1999 "C99" support to ISO/IEC DTR 19768 "TR1"
dnl facilities in Chapter 8, "C compatibility".
dnl
AC_DEFUN([GLIBCXX_CHECK_C99_TR1], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS

  # Use -std=c++98 because the default (-std=gnu++98) leaves __STRICT_ANSI__
  # undefined and fake C99 facilities may be spuriously enabled.
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -std=c++98"

  # Check for the existence of <complex.h> complex math functions used
  # by tr1/complex.
  AC_CHECK_HEADERS(complex.h, ac_has_complex_h=yes, ac_has_complex_h=no)
  ac_c99_complex_tr1=no;
  if test x"$ac_has_complex_h" = x"yes"; then
    AC_MSG_CHECKING([for ISO C99 support to TR1 in <complex.h>])
    AC_TRY_COMPILE([#include <complex.h>],
		   [typedef __complex__ float float_type; float_type tmpf;
		    cacosf(tmpf);
		    casinf(tmpf);
		    catanf(tmpf);
		    cacoshf(tmpf);
		    casinhf(tmpf);
		    catanhf(tmpf);
		    typedef __complex__ double double_type; double_type tmpd;
		    cacos(tmpd);
		    casin(tmpd);
		    catan(tmpd);
		    cacosh(tmpd);
		    casinh(tmpd);
		    catanh(tmpd);
		    typedef __complex__ long double ld_type; ld_type tmpld;
		    cacosl(tmpld);
		    casinl(tmpld);
		    catanl(tmpld);
		    cacoshl(tmpld);
		    casinhl(tmpld);
		    catanhl(tmpld);
		   ],[ac_c99_complex_tr1=yes], [ac_c99_complex_tr1=no])
  fi
  AC_MSG_RESULT($ac_c99_complex_tr1)
  if test x"$ac_c99_complex_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_COMPLEX_TR1, 1,
	      [Define if C99 functions in <complex.h> should be used in
	      <tr1/complex>. Using compiler builtins for these functions
	      requires corresponding C99 library functions to be present.])
  fi

  # Check for the existence of <ctype.h> functions.
  AC_CACHE_CHECK([for ISO C99 support to TR1 in <ctype.h>],
  glibcxx_cv_c99_ctype_tr1, [
  AC_TRY_COMPILE([#include <ctype.h>],
		 [int ch;
		  int ret;
		  ret = isblank(ch);
		 ],[glibcxx_cv_c99_ctype_tr1=yes],
		   [glibcxx_cv_c99_ctype_tr1=no])
  ])
  if test x"$glibcxx_cv_c99_ctype_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_CTYPE_TR1, 1,
	      [Define if C99 functions in <ctype.h> should be imported in
	      <tr1/cctype> in namespace std::tr1.])
  fi

  # Check for the existence of <fenv.h> functions.
  AC_CHECK_HEADERS(fenv.h, ac_has_fenv_h=yes, ac_has_fenv_h=no)
  ac_c99_fenv_tr1=no;
  if test x"$ac_has_fenv_h" = x"yes"; then
    AC_MSG_CHECKING([for ISO C99 support to TR1 in <fenv.h>])
    AC_TRY_COMPILE([#include <fenv.h>],
		   [int except, mode;
		    fexcept_t* pflag;
		    fenv_t* penv;
		    int ret;
		    ret = feclearexcept(except);
		    ret = fegetexceptflag(pflag, except);
		    ret = feraiseexcept(except);
		    ret = fesetexceptflag(pflag, except);
		    ret = fetestexcept(except);
		    ret = fegetround();
		    ret = fesetround(mode);
		    ret = fegetenv(penv);
		    ret = feholdexcept(penv);
		    ret = fesetenv(penv);
		    ret = feupdateenv(penv);
		   ],[ac_c99_fenv_tr1=yes], [ac_c99_fenv_tr1=no])
    AC_MSG_RESULT($ac_c99_fenv_tr1)
  fi
  if test x"$ac_c99_fenv_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_FENV_TR1, 1,
	      [Define if C99 functions in <fenv.h> should be imported in
	      <tr1/cfenv> in namespace std::tr1.])
  fi

  # Check for the existence of <stdint.h> types.
  AC_CACHE_CHECK([for ISO C99 support to TR1 in <stdint.h>],
  glibcxx_cv_c99_stdint_tr1, [
  AC_TRY_COMPILE([#define __STDC_LIMIT_MACROS
		  #define __STDC_CONSTANT_MACROS
		  #include <stdint.h>],
		 [typedef int8_t          my_int8_t;
		  my_int8_t               i8 = INT8_MIN;
		  i8 = INT8_MAX;
		  typedef int16_t         my_int16_t;
		  my_int16_t              i16 = INT16_MIN;
		  i16 = INT16_MAX;
		  typedef int32_t         my_int32_t;
		  my_int32_t              i32 = INT32_MIN;
		  i32 = INT32_MAX;
		  typedef int64_t         my_int64_t;
		  my_int64_t              i64 = INT64_MIN;
		  i64 = INT64_MAX;
		  typedef int_fast8_t     my_int_fast8_t;
		  my_int_fast8_t          if8 = INT_FAST8_MIN;
		  if8 = INT_FAST8_MAX;
		  typedef int_fast16_t    my_int_fast16_t;
		  my_int_fast16_t         if16 = INT_FAST16_MIN;
		  if16 = INT_FAST16_MAX;
		  typedef int_fast32_t    my_int_fast32_t;
		  my_int_fast32_t         if32 = INT_FAST32_MIN;
		  if32 = INT_FAST32_MAX;
		  typedef int_fast64_t    my_int_fast64_t;
		  my_int_fast64_t         if64 = INT_FAST64_MIN;
		  if64 = INT_FAST64_MAX;
		  typedef int_least8_t    my_int_least8_t;
		  my_int_least8_t         il8 = INT_LEAST8_MIN;
		  il8 = INT_LEAST8_MAX;
		  typedef int_least16_t   my_int_least16_t;
		  my_int_least16_t        il16 = INT_LEAST16_MIN;
		  il16 = INT_LEAST16_MAX;
		  typedef int_least32_t   my_int_least32_t;
		  my_int_least32_t        il32 = INT_LEAST32_MIN;
		  il32 = INT_LEAST32_MAX;
		  typedef int_least64_t   my_int_least64_t;
		  my_int_least64_t        il64 = INT_LEAST64_MIN;
		  il64 = INT_LEAST64_MAX;
		  typedef intmax_t        my_intmax_t;
		  my_intmax_t             im = INTMAX_MAX;
		  im = INTMAX_MIN;
		  typedef intptr_t        my_intptr_t;
		  my_intptr_t             ip = INTPTR_MAX;
		  ip = INTPTR_MIN;
		  typedef uint8_t         my_uint8_t;
		  my_uint8_t              ui8 = UINT8_MAX;
		  ui8 = UINT8_MAX;
		  typedef uint16_t        my_uint16_t;
		  my_uint16_t             ui16 = UINT16_MAX;
		  ui16 = UINT16_MAX;
		  typedef uint32_t        my_uint32_t;
		  my_uint32_t             ui32 = UINT32_MAX;
		  ui32 = UINT32_MAX;
		  typedef uint64_t        my_uint64_t;
		  my_uint64_t             ui64 = UINT64_MAX;
		  ui64 = UINT64_MAX;
		  typedef uint_fast8_t    my_uint_fast8_t;
		  my_uint_fast8_t         uif8 = UINT_FAST8_MAX;
		  uif8 = UINT_FAST8_MAX;
		  typedef uint_fast16_t   my_uint_fast16_t;
		  my_uint_fast16_t        uif16 = UINT_FAST16_MAX;
		  uif16 = UINT_FAST16_MAX;
		  typedef uint_fast32_t   my_uint_fast32_t;
		  my_uint_fast32_t        uif32 = UINT_FAST32_MAX;
		  uif32 = UINT_FAST32_MAX;
		  typedef uint_fast64_t   my_uint_fast64_t;
		  my_uint_fast64_t        uif64 = UINT_FAST64_MAX;
		  uif64 = UINT_FAST64_MAX;
		  typedef uint_least8_t   my_uint_least8_t;
		  my_uint_least8_t        uil8 = UINT_LEAST8_MAX;
		  uil8 = UINT_LEAST8_MAX;
		  typedef uint_least16_t  my_uint_least16_t;
		  my_uint_least16_t       uil16 = UINT_LEAST16_MAX;
		  uil16 = UINT_LEAST16_MAX;
		  typedef uint_least32_t  my_uint_least32_t;
		  my_uint_least32_t       uil32 = UINT_LEAST32_MAX;
		  uil32 = UINT_LEAST32_MAX;
		  typedef uint_least64_t  my_uint_least64_t;
		  my_uint_least64_t       uil64 = UINT_LEAST64_MAX;
		  uil64 = UINT_LEAST64_MAX;
		  typedef uintmax_t       my_uintmax_t;
		  my_uintmax_t            uim = UINTMAX_MAX;
		  uim = UINTMAX_MAX;
		  typedef uintptr_t       my_uintptr_t;
		  my_uintptr_t            uip = UINTPTR_MAX;
		  uip = UINTPTR_MAX;
		 ],[glibcxx_cv_c99_stdint_tr1=yes],
		   [glibcxx_cv_c99_stdint_tr1=no])
  ])
  if test x"$glibcxx_cv_c99_stdint_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_STDINT_TR1, 1,
	      [Define if C99 types in <stdint.h> should be imported in
	      <tr1/cstdint> in namespace std::tr1.])
  fi

  # Check for the existence of <math.h> functions.
  AC_CACHE_CHECK([for ISO C99 support to TR1 in <math.h>],
  glibcxx_cv_c99_math_tr1, [
  AC_TRY_COMPILE([#include <math.h>],
		 [typedef double_t  my_double_t;
		  typedef float_t   my_float_t;
		  acosh(0.0);
		  acoshf(0.0f);
		  acoshl(0.0l);
		  asinh(0.0);
		  asinhf(0.0f);
		  asinhl(0.0l);
		  atanh(0.0);
		  atanhf(0.0f);
		  atanhl(0.0l);
		  cbrt(0.0);
		  cbrtf(0.0f);
		  cbrtl(0.0l);
		  copysign(0.0, 0.0);
		  copysignf(0.0f, 0.0f);
		  copysignl(0.0l, 0.0l);
		  erf(0.0);
		  erff(0.0f);
		  erfl(0.0l);
		  erfc(0.0);
		  erfcf(0.0f);
		  erfcl(0.0l);
		  exp2(0.0);
		  exp2f(0.0f);
		  exp2l(0.0l);
		  expm1(0.0);
		  expm1f(0.0f);
		  expm1l(0.0l);
		  fdim(0.0, 0.0);
		  fdimf(0.0f, 0.0f);
		  fdiml(0.0l, 0.0l);
		  fma(0.0, 0.0, 0.0);
		  fmaf(0.0f, 0.0f, 0.0f);
		  fmal(0.0l, 0.0l, 0.0l);
		  fmax(0.0, 0.0);
		  fmaxf(0.0f, 0.0f);
		  fmaxl(0.0l, 0.0l);
		  fmin(0.0, 0.0);
		  fminf(0.0f, 0.0f);
		  fminl(0.0l, 0.0l);
		  hypot(0.0, 0.0);
		  hypotf(0.0f, 0.0f);
		  hypotl(0.0l, 0.0l);
		  ilogb(0.0);
		  ilogbf(0.0f);
		  ilogbl(0.0l);
		  lgamma(0.0);
		  lgammaf(0.0f);
		  lgammal(0.0l);
		  #ifndef __APPLE__ /* see below */
		  llrint(0.0);
		  llrintf(0.0f);
		  llrintl(0.0l);
		  llround(0.0);
		  llroundf(0.0f);
		  llroundl(0.0l);
		  #endif
		  log1p(0.0);
		  log1pf(0.0f);
		  log1pl(0.0l);
		  log2(0.0);
		  log2f(0.0f);
		  log2l(0.0l);
		  logb(0.0);
		  logbf(0.0f);
		  logbl(0.0l);
		  lrint(0.0);
		  lrintf(0.0f);
		  lrintl(0.0l);
		  lround(0.0);
		  lroundf(0.0f);
		  lroundl(0.0l);
		  nan(0);
		  nanf(0);
		  nanl(0);
		  nearbyint(0.0);
		  nearbyintf(0.0f);
		  nearbyintl(0.0l);
		  nextafter(0.0, 0.0);
		  nextafterf(0.0f, 0.0f);
		  nextafterl(0.0l, 0.0l);
		  nexttoward(0.0, 0.0);
		  nexttowardf(0.0f, 0.0f);
		  nexttowardl(0.0l, 0.0l);
		  remainder(0.0, 0.0);
		  remainderf(0.0f, 0.0f);
		  remainderl(0.0l, 0.0l);
		  remquo(0.0, 0.0, 0);
		  remquof(0.0f, 0.0f, 0);
		  remquol(0.0l, 0.0l, 0);
		  rint(0.0);
		  rintf(0.0f);
		  rintl(0.0l);
		  round(0.0);
		  roundf(0.0f);
		  roundl(0.0l);
		  scalbln(0.0, 0l);
		  scalblnf(0.0f, 0l);
		  scalblnl(0.0l, 0l);
		  scalbn(0.0, 0);
		  scalbnf(0.0f, 0);
		  scalbnl(0.0l, 0);
		  tgamma(0.0);
		  tgammaf(0.0f);
		  tgammal(0.0l);
		  trunc(0.0);
		  truncf(0.0f);
		  truncl(0.0l);
		 ],[glibcxx_cv_c99_math_tr1=yes], [glibcxx_cv_c99_math_tr1=no])
  ])
  if test x"$glibcxx_cv_c99_math_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_MATH_TR1, 1,
	      [Define if C99 functions or macros in <math.h> should be imported
	      in <tr1/cmath> in namespace std::tr1.])

    case "${target_os}" in
      darwin*)
	AC_CACHE_CHECK([for ISO C99 rounding functions in <math.h>],
	  glibcxx_cv_c99_math_llround, [
          AC_TRY_COMPILE([#include <math.h>],
		 [llrint(0.0);
		  llrintf(0.0f);
		  llrintl(0.0l);
		  llround(0.0);
		  llroundf(0.0f);
		  llroundl(0.0l);
		 ],
		 [glibcxx_cv_c99_math_llround=yes],
		 [glibcxx_cv_c99_math_llround=no])
          ])
        ;;
    esac
    if test x"$glibcxx_cv_c99_math_llround" = x"no"; then
      AC_DEFINE(_GLIBCXX_NO_C99_ROUNDING_FUNCS, 1,
		[Define if C99 llrint and llround functions are missing from <math.h>.])
    fi
  fi

  # Check for the existence of <inttypes.h> functions (NB: doesn't make
  # sense if the glibcxx_cv_c99_stdint_tr1 check fails, per C99, 7.8/1).
  ac_c99_inttypes_tr1=no;
  if test x"$glibcxx_cv_c99_stdint_tr1" = x"yes"; then
    AC_MSG_CHECKING([for ISO C99 support to TR1 in <inttypes.h>])
    AC_TRY_COMPILE([#include <inttypes.h>],
		   [intmax_t i, numer, denom, base;
		    const char* s;
		    char** endptr;
		    intmax_t ret = imaxabs(i);
		    imaxdiv_t dret = imaxdiv(numer, denom);
		    ret = strtoimax(s, endptr, base);
		    uintmax_t uret = strtoumax(s, endptr, base);
		   ],[ac_c99_inttypes_tr1=yes], [ac_c99_inttypes_tr1=no])
    AC_MSG_RESULT($ac_c99_inttypes_tr1)
  fi
  if test x"$ac_c99_inttypes_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_INTTYPES_TR1, 1,
	      [Define if C99 functions in <inttypes.h> should be imported in
	      <tr1/cinttypes> in namespace std::tr1.])
  fi

  # Check for the existence of wchar_t <inttypes.h> functions (NB: doesn't
  # make sense if the glibcxx_cv_c99_stdint_tr1 check fails, per C99, 7.8/1).
  ac_c99_inttypes_wchar_t_tr1=no;
  if test x"$glibcxx_cv_c99_stdint_tr1" = x"yes"; then
    AC_MSG_CHECKING([for wchar_t ISO C99 support to TR1 in <inttypes.h>])
    AC_TRY_COMPILE([#include <inttypes.h>],
		   [intmax_t base;
		    const wchar_t* s;
		    wchar_t** endptr;
		    intmax_t ret = wcstoimax(s, endptr, base);
		    uintmax_t uret = wcstoumax(s, endptr, base);
		   ],[ac_c99_inttypes_wchar_t_tr1=yes],
		     [ac_c99_inttypes_wchar_t_tr1=no])
    AC_MSG_RESULT($ac_c99_inttypes_wchar_t_tr1)
  fi
  if test x"$ac_c99_inttypes_wchar_t_tr1" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C99_INTTYPES_WCHAR_T_TR1, 1,
	      [Define if wchar_t C99 functions in <inttypes.h> should be
	      imported in <tr1/cinttypes> in namespace std::tr1.])
  fi

  # Check for the existence of the <stdbool.h> header.
  AC_CHECK_HEADERS(stdbool.h)

  # Check for the existence of the <stdalign.h> header.
  AC_CHECK_HEADERS(stdalign.h)

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check for uchar.h and usability.
dnl
AC_DEFUN([GLIBCXX_CHECK_UCHAR_H], [

  # Test uchar.h.
  AC_CHECK_HEADERS(uchar.h, ac_has_uchar_h=yes, ac_has_uchar_h=no)

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -std=c++11"

  if test x"$ac_has_uchar_h" = x"yes"; then
    AC_MSG_CHECKING([for ISO C11 support for <uchar.h>])
    AC_TRY_COMPILE([#include <uchar.h>
		    #ifdef __STDC_UTF_16__
		    long i = __STDC_UTF_16__;
		    #endif
		    #ifdef __STDC_UTF_32__
		    long j = __STDC_UTF_32__;
		    #endif
		    namespace test
		    {
		      using ::c16rtomb;
		      using ::c32rtomb;
		      using ::mbrtoc16;
		      using ::mbrtoc32;
		    }
		   ],
		   [], [ac_c11_uchar_cxx11=yes], [ac_c11_uchar_cxx11=no])
    AC_MSG_RESULT($ac_c11_uchar_cxx11)
  else
    ac_c11_uchar_cxx11=no
  fi
  if test x"$ac_c11_uchar_cxx11" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_C11_UCHAR_CXX11, 1,
	      [Define if C11 functions in <uchar.h> should be imported into
	      namespace std in <cuchar>.])
  fi

  CXXFLAGS="$CXXFLAGS -fchar8_t"
  if test x"$ac_has_uchar_h" = x"yes"; then
    AC_MSG_CHECKING([for c8rtomb and mbrtoc8 in <uchar.h> with -fchar8_t])
    AC_TRY_COMPILE([#include <uchar.h>
		    namespace test
		    {
		      using ::c8rtomb;
		      using ::mbrtoc8;
		    }
		   ],
		   [], [ac_uchar_c8rtomb_mbrtoc8_fchar8_t=yes],
		       [ac_uchar_c8rtomb_mbrtoc8_fchar8_t=no])
  else
    ac_uchar_c8rtomb_mbrtoc8_fchar8_t=no
  fi
  AC_MSG_RESULT($ac_uchar_c8rtomb_mbrtoc8_fchar8_t)
  if test x"$ac_uchar_c8rtomb_mbrtoc8_fchar8_t" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_FCHAR8_T, 1,
	      [Define if c8rtomb and mbrtoc8 functions in <uchar.h> should be
	      imported into namespace std in <cuchar> for -fchar8_t.])
  fi

  CXXFLAGS="$CXXFLAGS -std=c++20"
  if test x"$ac_has_uchar_h" = x"yes"; then
    AC_MSG_CHECKING([for c8rtomb and mbrtoc8 in <uchar.h> with -std=c++20])
    AC_TRY_COMPILE([#include <uchar.h>
		    namespace test
		    {
		      using ::c8rtomb;
		      using ::mbrtoc8;
		    }
		   ],
		   [], [ac_uchar_c8rtomb_mbrtoc8_cxx20=yes],
		       [ac_uchar_c8rtomb_mbrtoc8_cxx20=no])
  else
    ac_uchar_c8rtomb_mbrtoc8_cxx20=no
  fi
  AC_MSG_RESULT($ac_uchar_c8rtomb_mbrtoc8_cxx20)
  if test x"$ac_uchar_c8rtomb_mbrtoc8_cxx20" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_UCHAR_C8RTOMB_MBRTOC8_CXX20, 1,
	      [Define if c8rtomb and mbrtoc8 functions in <uchar.h> should be
	      imported into namespace std in <cuchar> for C++20.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


dnl
dnl Check whether "/dev/random" and "/dev/urandom" are available for
dnl class std::random_device from C++ 2011 [rand.device], and
dnl random_device of "TR1" (Chapter 5.1, "Random number generation").
dnl
AC_DEFUN([GLIBCXX_CHECK_DEV_RANDOM], [

  AC_CACHE_CHECK([for "/dev/random" and "/dev/urandom" for std::random_device],
    glibcxx_cv_dev_random, [
    if test -r /dev/random && test -r /dev/urandom; then
  ## For MSys environment the test above is detected as false-positive
  ## on mingw-targets.  So disable it explicitly for them.
      case ${target_os} in
	*mingw*) glibcxx_cv_dev_random=no ;;
	*) glibcxx_cv_dev_random=yes ;;
      esac
    else
      glibcxx_cv_dev_random=no;
    fi
  ])

  if test x"$glibcxx_cv_dev_random" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_DEV_RANDOM, 1,
	      [Define if /dev/random and /dev/urandom are available for
	       std::random_device.])
    AC_DEFINE(_GLIBCXX_USE_RANDOM_TR1, 1,
	      [Define if /dev/random and /dev/urandom are available for
	       the random_device of TR1 (Chapter 5.1).])
  fi

])

dnl
dnl Compute the EOF, SEEK_CUR, and SEEK_END integer constants.
dnl
AC_DEFUN([GLIBCXX_COMPUTE_STDIO_INTEGER_CONSTANTS], [

if test "$is_hosted" = yes; then
  AC_CACHE_CHECK([for the value of EOF], glibcxx_cv_stdio_eof, [
  AC_COMPUTE_INT([glibcxx_cv_stdio_eof], [[EOF]],
		 [#include <stdio.h>],
		 [AC_MSG_ERROR([computing EOF failed])])
  ])
  AC_DEFINE_UNQUOTED(_GLIBCXX_STDIO_EOF, $glibcxx_cv_stdio_eof,
		     [Define to the value of the EOF integer constant.])

  AC_CACHE_CHECK([for the value of SEEK_CUR], glibcxx_cv_stdio_seek_cur, [
  AC_COMPUTE_INT([glibcxx_cv_stdio_seek_cur], [[SEEK_CUR]],
		 [#include <stdio.h>],
		 [AC_MSG_ERROR([computing SEEK_CUR failed])])
  ])
  AC_DEFINE_UNQUOTED(_GLIBCXX_STDIO_SEEK_CUR, $glibcxx_cv_stdio_seek_cur,
		     [Define to the value of the SEEK_CUR integer constant.])

  AC_CACHE_CHECK([for the value of SEEK_END], glibcxx_cv_stdio_seek_end, [
  AC_COMPUTE_INT([glibcxx_cv_stdio_seek_end], [[SEEK_END]],
		 [#include <stdio.h>],
		 [AC_MSG_ERROR([computing SEEK_END failed])])
  ])
  AC_DEFINE_UNQUOTED(_GLIBCXX_STDIO_SEEK_END, $glibcxx_cv_stdio_seek_end,
		     [Define to the value of the SEEK_END integer constant.])
fi
])

dnl
dnl Check whether required C++ overloads are present in <stdio.h>.
dnl
AC_DEFUN([GLIBCXX_CHECK_STDIO_PROTO], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  # Use C++11 because a conforming <stdio.h> won't define gets for C++14,
  # and we don't need a declaration for C++14 anyway.
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -std=gnu++11"

  AC_CACHE_CHECK([for gets declaration], glibcxx_cv_gets, [
  AC_COMPILE_IFELSE([AC_LANG_SOURCE(
	  [#include <stdio.h>
	   namespace test
	   {
              using ::gets;
	   }
	])],
	[glibcxx_cv_gets=yes],
	[glibcxx_cv_gets=no]
      )])

  if test $glibcxx_cv_gets = yes; then
    AC_DEFINE(HAVE_GETS, 1, [Define if gets is available in <stdio.h> before C++14.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether required C++11 overloads for floating point and integral
dnl types are present in <math.h>.
dnl
AC_DEFUN([GLIBCXX_CHECK_MATH11_PROTO], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -std=c++11"

  case "$host" in
    *-*-solaris2.*)
      # Solaris 12 Build 86, Solaris 11.3 SRU 3.6, and Solaris 10 Patch
      # 11996[67]-02 introduced the C++11 <math.h> floating point overloads.
      AC_CACHE_CHECK([for C++11 <math.h> floating point overloads],
	glibcxx_cv_math11_fp_overload, [
	AC_COMPILE_IFELSE([AC_LANG_SOURCE(
	  [#include <math.h>
	   #undef isfinite
	   namespace std {
	     inline bool isfinite(float __x)
	     { return __builtin_isfinite(__x); }
	   }
	])],
	[glibcxx_cv_math11_fp_overload=no],
	[glibcxx_cv_math11_fp_overload=yes]
      )])

      # autoheader cannot handle indented templates.
      AH_VERBATIM([__CORRECT_ISO_CPP11_MATH_H_PROTO_FP],
        [/* Define if all C++11 floating point overloads are available in <math.h>.  */
#if __cplusplus >= 201103L
#undef __CORRECT_ISO_CPP11_MATH_H_PROTO_FP
#endif])

      if test $glibcxx_cv_math11_fp_overload = yes; then
        AC_DEFINE(__CORRECT_ISO_CPP11_MATH_H_PROTO_FP)
      fi

      # Solaris 12 Build 90, Solaris 11.3 SRU 5.6, and Solaris 10 Patch
      # 11996[67]-02 introduced the C++11 <math.h> integral type overloads.
      AC_CACHE_CHECK([for C++11 <math.h> integral type overloads],
	glibcxx_cv_math11_int_overload, [
	AC_COMPILE_IFELSE([AC_LANG_SOURCE(
	  [#include <math.h>
	   namespace std {
	     template<typename _Tp>
	       struct __is_integer;
	     template<>
	       struct __is_integer<int>
	       {
	         enum { __value = 1 };
	       };
	   }
	   namespace __gnu_cxx {
	     template<bool, typename>
	       struct __enable_if;
	     template<typename _Tp>
	       struct __enable_if<true, _Tp>
	       { typedef _Tp __type; };
	   }
	   namespace std {
	     template<typename _Tp>
	       constexpr typename __gnu_cxx::__enable_if
	       		 <__is_integer<_Tp>::__value, double>::__type
	       log2(_Tp __x)
	       { return __builtin_log2(__x); }
	   }
	   int
	   main (void)
	   {
	     int i = 1000;
	     return std::log2(i);
	   }
	])],
	[glibcxx_cv_math11_int_overload=no],
	[glibcxx_cv_math11_int_overload=yes]
      )])

      # autoheader cannot handle indented templates.
      AH_VERBATIM([__CORRECT_ISO_CPP11_MATH_H_PROTO_INT],
        [/* Define if all C++11 integral type overloads are available in <math.h>.  */
#if __cplusplus >= 201103L
#undef __CORRECT_ISO_CPP11_MATH_H_PROTO_INT
#endif])

      if test $glibcxx_cv_math11_int_overload = yes; then
        AC_DEFINE(__CORRECT_ISO_CPP11_MATH_H_PROTO_INT)
      fi
      ;;
    *)
      # If <math.h> defines the obsolete isinf(double) and isnan(double)
      # functions (instead of or as well as the C99 generic macros) then we
      # can't define std::isinf(double) and std::isnan(double) in <cmath>
      # and must use the ones from <math.h> instead.
	AC_CACHE_CHECK([for obsolete isinf function in <math.h>],
	  glibcxx_cv_obsolete_isinf, [
          AC_COMPILE_IFELSE([AC_LANG_SOURCE(
            [#define _GLIBCXX_INCLUDE_NEXT_C_HEADERS
             #include <math.h>
             #undef isinf
             namespace std {
               using ::isinf;
               bool isinf(float);
               bool isinf(long double);
             }
             using std::isinf;
             bool b = isinf(0.0);
          ])],
          [glibcxx_cv_obsolete_isinf=yes],
          [glibcxx_cv_obsolete_isinf=no]
        )])
      if test $glibcxx_cv_obsolete_isinf = yes; then
        AC_DEFINE(HAVE_OBSOLETE_ISINF, 1,
                  [Define if <math.h> defines obsolete isinf function.])
      fi

	AC_CACHE_CHECK([for obsolete isnan function in <math.h>],
	  glibcxx_cv_obsolete_isnan, [
          AC_COMPILE_IFELSE([AC_LANG_SOURCE(
            [#define _GLIBCXX_INCLUDE_NEXT_C_HEADERS
             #include <math.h>
             #undef isnan
             namespace std {
               using ::isnan;
               bool isnan(float);
               bool isnan(long double);
             }
             using std::isnan;
             bool b = isnan(0.0);
          ])],
          [glibcxx_cv_obsolete_isnan=yes],
          [glibcxx_cv_obsolete_isnan=no]
        )])
      if test $glibcxx_cv_obsolete_isnan = yes; then
        AC_DEFINE(HAVE_OBSOLETE_ISNAN, 1,
                  [Define if <math.h> defines obsolete isnan function.])
      fi
      ;;
  esac

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check for what type of C headers to use.
dnl
dnl --enable-cheaders= [does stuff].
dnl --disable-cheaders [does not do anything, really].
dnl  +  Usage:  GLIBCXX_ENABLE_CHEADERS[(DEFAULT)]
dnl       Where DEFAULT is either 'c' or 'c_global' or 'c_std'.
dnl
dnl To use the obsolete 'c_std' headers use --enable-cheaders-obsolete as
dnl well as --enable-cheaders=c_std, otherwise configure will fail.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CHEADERS], [
  GLIBCXX_ENABLE(cheaders-obsolete,no,,
    [allow use of obsolete "C" headers for g++])
  GLIBCXX_ENABLE(cheaders,$1,[[[=KIND]]],
    [construct "C" headers for g++], [permit c|c_global|c_std])
  AC_MSG_NOTICE("C" header strategy set to $enable_cheaders)
  if test $enable_cheaders = c_std ; then
    AC_MSG_WARN([the --enable-cheaders=c_std configuration is obsolete, c_global should be used instead])
    AC_MSG_WARN([if you are unable to use c_global please report a bug or inform libstdc++@gcc.gnu.org])
    if test $enable_cheaders_obsolete != yes ; then
      AC_MSG_ERROR(use --enable-cheaders-obsolete to use c_std "C" headers)
    fi
  fi

  C_INCLUDE_DIR='${glibcxx_srcdir}/include/'$enable_cheaders

  # Allow overrides to configure.host here.
  if test $enable_cheaders = c_global; then
     c_compatibility=yes
  fi

  AC_SUBST(C_INCLUDE_DIR)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_C, test $enable_cheaders = c)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_C_STD, test $enable_cheaders = c_std)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_C_GLOBAL, test $enable_cheaders = c_global)
  GLIBCXX_CONDITIONAL(GLIBCXX_C_HEADERS_COMPATIBILITY, test $c_compatibility = yes)
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
dnl Check for which std::allocator base class to use.  The choice is
dnl mapped from a subdirectory of include/ext.
dnl
dnl Default is new.
dnl
AC_DEFUN([GLIBCXX_ENABLE_ALLOCATOR], [
  AC_MSG_CHECKING([for std::allocator base class])
  GLIBCXX_ENABLE(libstdcxx-allocator,auto,[[[=KIND]]],
    [use KIND for target std::allocator base],
    [permit new|malloc|yes|no|auto])

  # If they didn't use this option switch, or if they specified --enable
  # with no specific model, we'll have to look for one.  If they
  # specified --disable (???), do likewise.
  if test $enable_libstdcxx_allocator = no ||
     test $enable_libstdcxx_allocator = yes;
  then
     enable_libstdcxx_allocator=auto
  fi

  # Either a known package, or "auto". Auto implies the default choice
  # for a particular platform.
  enable_libstdcxx_allocator_flag=$enable_libstdcxx_allocator

  # Probe for host-specific support if no specific model is specified.
  # Default to "new".
  if test $enable_libstdcxx_allocator_flag = auto; then
    case ${target_os} in
      linux* | gnu* | kfreebsd*-gnu | knetbsd*-gnu)
	enable_libstdcxx_allocator_flag=new
	;;
      *)
	enable_libstdcxx_allocator_flag=new
	;;
    esac
  fi
  AC_MSG_RESULT($enable_libstdcxx_allocator_flag)


  # Set configure bits for specified locale package
  case ${enable_libstdcxx_allocator_flag} in
    malloc)
      ALLOCATOR_H=config/allocator/malloc_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::malloc_allocator
      ;;
    new)
      ALLOCATOR_H=config/allocator/new_allocator_base.h
      ALLOCATOR_NAME=__gnu_cxx::new_allocator
      ;;
  esac

  GLIBCXX_CONDITIONAL(ENABLE_ALLOCATOR_NEW,
		      test $enable_libstdcxx_allocator_flag = new)
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
    AC_DEFINE(_GLIBCXX_CONCEPT_CHECKS, 1,
	      [Define to use concept checking code from the boost libraries.])
  fi
])

dnl
dnl Use extern templates.
dnl
dnl --enable-extern-template defines _GLIBCXX_EXTERN_TEMPLATE to 1
dnl --disable-extern-template defines _GLIBCXX_EXTERN_TEMPLATE to 0

dnl  +  Usage:  GLIBCXX_ENABLE_TEMPLATE[(DEFAULT)]
dnl       Where DEFAULT is `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_EXTERN_TEMPLATE], [

  GLIBCXX_ENABLE(extern-template,$1,,[enable extern template])

  AC_MSG_CHECKING([for extern template support])
  AC_MSG_RESULT([$enable_extern_template])

  GLIBCXX_CONDITIONAL(ENABLE_EXTERN_TEMPLATE, test $enable_extern_template = yes)
])

dnl
dnl Use vtable verification.
dnl
dnl --enable-vtable-verify defines _GLIBCXX_VTABLE_VERIFY to 1
dnl --disable-vtable-verify defines _GLIBCXX_VTABLE_VERIFY to 0

dnl  +  Usage:  GLIBCXX_ENABLE_VTABLE_VERIFY[(DEFAULT)]
dnl       Where DEFAULT is `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_VTABLE_VERIFY], [

  GLIBCXX_ENABLE(vtable-verify,$1,,[enable vtable verify])

  AC_MSG_CHECKING([for vtable verify support])
  AC_MSG_RESULT([$enable_vtable_verify])

  vtv_cygmin=no
  if test $enable_vtable_verify = yes; then
    case ${target_os} in
      cygwin*|mingw32*)
        VTV_CXXFLAGS="-fvtable-verify=std -Wl,-lvtv,-u_vtable_map_vars_start,-u_vtable_map_vars_end"
        VTV_CXXLINKFLAGS="-L${toplevel_builddir}/libvtv/.libs -Wl,--rpath -Wl,${toplevel_builddir}/libvtv/.libs"
        vtv_cygmin=yes
        ;;
      darwin*)
        VTV_CXXFLAGS="-fvtable-verify=std -Wl,-u,_vtable_map_vars_start -Wl,-u,_vtable_map_vars_end"
        VTV_CXXLINKFLAGS="-L${toplevel_builddir}/libvtv/.libs -Wl,-rpath,${toplevel_builddir}/libvtv/.libs"
        ;;
      solaris2*)
        VTV_CXXFLAGS="-fvtable-verify=std -Wl,-u_vtable_map_vars_start,-u_vtable_map_vars_end"
        VTV_CXXLINKFLAGS="-L${toplevel_builddir}/libvtv/.libs -Wl,-R -Wl,${toplevel_builddir}/libvtv/.libs"
        ;;
      *)
        VTV_CXXFLAGS="-fvtable-verify=std -Wl,-u_vtable_map_vars_start,-u_vtable_map_vars_end"
        VTV_CXXLINKFLAGS="-L${toplevel_builddir}/libvtv/.libs -Wl,--rpath -Wl,${toplevel_builddir}/libvtv/.libs"
        ;;
    esac
    VTV_PCH_CXXFLAGS="-fvtable-verify=std"
  else
    VTV_CXXFLAGS=
    VTV_PCH_CXXFLAGS=
    VTV_CXXLINKFLAGS=
  fi

  AC_SUBST(VTV_CXXFLAGS)
  AC_SUBST(VTV_PCH_CXXFLAGS)
  AC_SUBST(VTV_CXXLINKFLAGS)
  AM_CONDITIONAL(VTV_CYGMIN, test x$vtv_cygmin = xyes)
  GLIBCXX_CONDITIONAL(ENABLE_VTABLE_VERIFY, test $enable_vtable_verify = yes)
])

dnl
dnl Check for parallel mode pre-requisites, including OpenMP support.
dnl
dnl  +  Usage:  GLIBCXX_ENABLE_PARALLEL
dnl
AC_DEFUN([GLIBCXX_ENABLE_PARALLEL], [

  enable_parallel=no;

  # See if configured libgomp/omp.h exists. (libgomp may be in
  # noconfigdirs but not explicitly disabled.)
  if echo " ${TARGET_CONFIGDIRS} " | grep " libgomp " > /dev/null 2>&1 ; then
    enable_parallel=yes;
  else
    AC_MSG_NOTICE([target-libgomp not built])
  fi

  AC_MSG_CHECKING([for parallel mode support])
  AC_MSG_RESULT([$enable_parallel])
])


dnl
dnl Check for which I/O library to use:  stdio and POSIX, or pure stdio.
dnl
dnl Default is stdio_posix.
dnl
AC_DEFUN([GLIBCXX_ENABLE_CSTDIO], [
  AC_MSG_CHECKING([for underlying I/O to use])
  GLIBCXX_ENABLE(cstdio,stdio,[[[=PACKAGE]]],
    [use target-specific I/O package], [permit stdio|stdio_posix|stdio_pure])

  # The only available I/O model is based on stdio, via basic_file_stdio.
  # The default "stdio" is actually "stdio + POSIX" because it uses fdopen(3)
  # to get a file descriptor and then uses read(3) and write(3) with it.
  # The "stdio_pure" model doesn't use fdopen and only uses FILE* for I/O.
  case ${enable_cstdio} in
    stdio*)
      CSTDIO_H=config/io/c_io_stdio.h
      BASIC_FILE_H=config/io/basic_file_stdio.h
      BASIC_FILE_CC=config/io/basic_file_stdio.cc

      if test "x$enable_cstdio" = "xstdio_pure" ; then
	AC_MSG_RESULT([stdio (without POSIX read/write)])
	AC_DEFINE(_GLIBCXX_USE_STDIO_PURE, 1,
		  [Define to restrict std::__basic_file<> to stdio APIs.])
      else
	AC_MSG_RESULT([stdio (with POSIX read/write)])
      fi
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
  skip_debug_build=
  GLIBCXX_ENABLE(libstdcxx-debug,$1,,[build extra debug library])
  if test x$enable_libstdcxx_debug = xyes; then
    if test -f $toplevel_builddir/../stage_final \
      && test -f $toplevel_builddir/../stage_current; then
      stage_final=`cat $toplevel_builddir/../stage_final`
      stage_current=`cat $toplevel_builddir/../stage_current`
      if test x$stage_current != x$stage_final ; then
	skip_debug_build=" (skipped for bootstrap stage $stage_current)"
	enable_libstdcxx_debug=no
      fi
    fi
  fi
  AC_MSG_RESULT($enable_libstdcxx_debug$skip_debug_build)
  GLIBCXX_CONDITIONAL(GLIBCXX_BUILD_DEBUG, test $enable_libstdcxx_debug = yes)
])


dnl
dnl Check for explicit debug flags.
dnl
dnl --enable-libstdcxx-debug-flags='-O1'
dnl is a general method for passing flags to be used when
dnl building debug libraries with --enable-libstdcxx-debug.
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
    [case "$host" in
	arm*-*-symbianelf*)
	    enable_hosted_libstdcxx=no
	    ;;
	*)
	    enable_hosted_libstdcxx=yes
	    ;;
     esac])
  freestanding_flags=
  if test "$enable_hosted_libstdcxx" = no; then
    AC_MSG_NOTICE([Only freestanding libraries will be built])
    is_hosted=no
    hosted_define=0
    enable_abi_check=no
    enable_libstdcxx_pch=no
    if test "x$with_headers" = xno; then
      freestanding_flags="-ffreestanding"
    fi
  else
    is_hosted=yes
    hosted_define=1
  fi
  GLIBCXX_CONDITIONAL(GLIBCXX_HOSTED, test $is_hosted = yes)
  AC_DEFINE_UNQUOTED(_GLIBCXX_HOSTED, $hosted_define,
    [Define to 1 if a full hosted library is built, or 0 if freestanding.])
  FREESTANDING_FLAGS="$freestanding_flags"
  AC_SUBST(FREESTANDING_FLAGS)
])


dnl
dnl Check if the user wants a non-verbose library implementation.
dnl
dnl --disable-libstdcxx-verbose will turn off descriptive messages to
dnl standard error on termination.
dnl
dnl Defines:
dnl  _GLIBCXX_VERBOSE (always defined, either to 1 or 0)
dnl
AC_DEFUN([GLIBCXX_ENABLE_VERBOSE], [
  AC_ARG_ENABLE([libstdcxx-verbose],
    AC_HELP_STRING([--disable-libstdcxx-verbose],
		   [disable termination messages to standard error]),,
		   [enable_libstdcxx_verbose=yes])
  if test x"$enable_libstdcxx_verbose" = xyes; then
    verbose_define=1
  else
    AC_MSG_NOTICE([verbose termination messages are disabled])
    verbose_define=0
  fi
  AC_DEFINE_UNQUOTED(_GLIBCXX_VERBOSE, $verbose_define,
    [Define to 1 if a verbose library is built, or 0 otherwise.])
])


dnl
dnl Check for template specializations for the 'long long' type.
dnl The result determines only whether 'long long' I/O is enabled; things
dnl like numeric_limits<> specializations are always available.
dnl
dnl --enable-long-long defines _GLIBCXX_USE_LONG_LONG
dnl --disable-long-long leaves _GLIBCXX_USE_LONG_LONG undefined
dnl  +  Usage:  GLIBCXX_ENABLE_LONG_LONG[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_LONG_LONG], [
  GLIBCXX_ENABLE(long-long,$1,,[enable template specializations for 'long long'])
  if test $enable_long_long = yes; then
    AC_DEFINE(_GLIBCXX_USE_LONG_LONG, 1,
	      [Define if code specialized for long long should be used.])
  fi
  AC_MSG_CHECKING([for enabled long long specializations])
  AC_MSG_RESULT([$enable_long_long])
])


dnl
dnl Check for decimal floating point.
dnl See:
dnl http://gcc.gnu.org/onlinedocs/gcc/Decimal-Float.html#Decimal-Float
dnl
dnl This checks to see if the host supports decimal floating point types.
dnl
dnl Defines:
dnl  _GLIBCXX_USE_DECIMAL_FLOAT
dnl
AC_DEFUN([GLIBCXX_ENABLE_DECIMAL_FLOAT], [

  # Fake what AC_TRY_COMPILE does, without linking as this is
  # unnecessary for this test.

    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
int main()
{
  _Decimal32 d1;
  _Decimal64 d2;
  _Decimal128 d3;
  return 0;
}
EOF

    AC_MSG_CHECKING([for ISO/IEC TR 24733 ])
    if AC_TRY_EVAL(ac_compile); then
      AC_DEFINE(_GLIBCXX_USE_DECIMAL_FLOAT, 1,
      [Define if ISO/IEC TR 24733 decimal floating point types are supported on this host.])
      enable_dfp=yes
    else
      enable_dfp=no
    fi
    AC_MSG_RESULT($enable_dfp)
    rm -f conftest*
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

dnl
dnl Check for template specializations for the 'wchar_t' type.
dnl
dnl --enable-wchar_t defines _GLIBCXX_USE_WCHAR_T
dnl --disable-wchar_t leaves _GLIBCXX_USE_WCHAR_T undefined
dnl  +  Usage:  GLIBCXX_ENABLE_WCHAR_T[(DEFAULT)]
dnl       Where DEFAULT is either `yes' or `no'.
dnl
dnl Necessary support must also be present.
dnl
AC_DEFUN([GLIBCXX_ENABLE_WCHAR_T], [
  GLIBCXX_ENABLE(wchar_t,$1,,[enable template specializations for 'wchar_t'])

  # Test wchar.h for mbstate_t, which is needed for char_traits and fpos.
  AC_CHECK_HEADERS(wchar.h, ac_has_wchar_h=yes, ac_has_wchar_h=no)
  AC_MSG_CHECKING([for mbstate_t])
  AC_TRY_COMPILE([#include <wchar.h>],
  [mbstate_t teststate;],
  have_mbstate_t=yes, have_mbstate_t=no)
  AC_MSG_RESULT($have_mbstate_t)
  if test x"$have_mbstate_t" = xyes; then
    AC_DEFINE(HAVE_MBSTATE_T,1,[Define if mbstate_t exists in wchar.h.])
  fi

  # Test it always, for use in GLIBCXX_ENABLE_C99, together with
  # ac_has_wchar_h.
  AC_CHECK_HEADERS(wctype.h, ac_has_wctype_h=yes, ac_has_wctype_h=no)

  if test x"$enable_wchar_t" = x"yes"; then

    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS

    if test x"$ac_has_wchar_h" = xyes &&
       test x"$ac_has_wctype_h" = xyes; then
      AC_TRY_COMPILE([#include <wchar.h>
		      #include <stddef.h>
		      wint_t i;
		      long l = WEOF;
		      long j = WCHAR_MIN;
		      long k = WCHAR_MAX;
		      namespace test
		      {
			using ::btowc;
			using ::fgetwc;
			using ::fgetws;
			using ::fputwc;
			using ::fputws;
			using ::fwide;
			using ::fwprintf;
			using ::fwscanf;
			using ::getwc;
			using ::getwchar;
 			using ::mbrlen;
			using ::mbrtowc;
			using ::mbsinit;
			using ::mbsrtowcs;
			using ::putwc;
			using ::putwchar;
			using ::swprintf;
			using ::swscanf;
			using ::ungetwc;
			using ::vfwprintf;
			using ::vswprintf;
			using ::vwprintf;
			using ::wcrtomb;
			using ::wcscat;
			using ::wcschr;
			using ::wcscmp;
			using ::wcscoll;
			using ::wcscpy;
			using ::wcscspn;
			using ::wcsftime;
			using ::wcslen;
			using ::wcsncat;
			using ::wcsncmp;
			using ::wcsncpy;
			using ::wcspbrk;
			using ::wcsrchr;
			using ::wcsrtombs;
			using ::wcsspn;
			using ::wcsstr;
			using ::wcstod;
			using ::wcstok;
			using ::wcstol;
			using ::wcstoul;
			using ::wcsxfrm;
			using ::wctob;
			using ::wmemchr;
			using ::wmemcmp;
			using ::wmemcpy;
			using ::wmemmove;
			using ::wmemset;
			using ::wprintf;
			using ::wscanf;
		      }
		     ],[],[], [enable_wchar_t=no])
    else
      enable_wchar_t=no
    fi

    AC_LANG_RESTORE
  fi

  if test x"$enable_wchar_t" = x"yes"; then
    AC_DEFINE(_GLIBCXX_USE_WCHAR_T, 1,
	      [Define if code specialized for wchar_t should be used.])
  fi

  AC_MSG_CHECKING([for enabled wchar_t specializations])
  AC_MSG_RESULT([$enable_wchar_t])
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
  GLIBCXX_ENABLE(libstdcxx-pch,$1,,[build pre-compiled libstdc++ headers])
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

  AC_MSG_CHECKING([for enabled PCH])
  AC_MSG_RESULT([$enable_libstdcxx_pch])

  GLIBCXX_CONDITIONAL(GLIBCXX_BUILD_PCH, test $enable_libstdcxx_pch = yes)
  if test $enable_libstdcxx_pch = yes; then
    glibcxx_PCHFLAGS="-include bits/stdc++.h"
  else
    glibcxx_PCHFLAGS=""
  fi
  AC_SUBST(glibcxx_PCHFLAGS)
])


dnl
dnl Check for atomic builtins.
dnl See:
dnl http://gcc.gnu.org/onlinedocs/gcc/_005f_005fatomic-Builtins.html
dnl
dnl This checks to see if the host supports the compiler-generated
dnl builtins for atomic operations for various integral sizes. Note, this
dnl is intended to be an all-or-nothing switch, so all the atomic operations
dnl that are used should be checked.
dnl
dnl Note:
dnl libgomp and libgfortran use a link test, see CHECK_SYNC_FETCH_AND_ADD.
dnl
AC_DEFUN([GLIBCXX_ENABLE_ATOMIC_BUILTINS], [
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  old_CXXFLAGS="$CXXFLAGS"

  # Do link tests if possible, instead asm tests, limited to some platforms
  # see discussion in PR target/40134, PR libstdc++/40133 and the thread
  # starting at http://gcc.gnu.org/ml/gcc-patches/2009-07/msg00322.html
  atomic_builtins_link_tests=no
  if test x$gcc_no_link != xyes; then
    # Can do link tests. Limit to some tested platforms
    case "$host" in
      *-*-linux* | *-*-uclinux* | *-*-kfreebsd*-gnu | *-*-gnu*)
	atomic_builtins_link_tests=yes
	;;
    esac
  fi

  if test x$atomic_builtins_link_tests = xyes; then

  # Do link tests.

  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for atomic builtins for bool],
    glibcxx_cv_atomic_bool, [
    AC_TRY_LINK(
      [ ],
      [typedef bool atomic_type;
       atomic_type c1;
       atomic_type c2;
       atomic_type c3(0);
       // N.B. __atomic_fetch_add is not supported for bool.
       __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
				   __ATOMIC_RELAXED);
       __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
       __atomic_load_n(&c1, __ATOMIC_RELAXED);
      ],
      [glibcxx_cv_atomic_bool=yes],
      [glibcxx_cv_atomic_bool=no])
  ])

  AC_CACHE_CHECK([for atomic builtins for short],
    glibcxx_cv_atomic_short, [
    AC_TRY_LINK(
      [ ],
      [typedef short atomic_type;
       atomic_type c1;
       atomic_type c2;
       atomic_type c3(0);
       __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
       __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
				   __ATOMIC_RELAXED);
       __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
       __atomic_load_n(&c1, __ATOMIC_RELAXED);
      ],
      [glibcxx_cv_atomic_short=yes],
      [glibcxx_cv_atomic_short=no])
  ])

  AC_CACHE_CHECK([for atomic builtins for int],
    glibcxx_cv_atomic_int, [
    AC_TRY_LINK(
      [ ],
      [typedef int atomic_type;
       atomic_type c1;
       atomic_type c2;
       atomic_type c3(0);
       __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
       __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
				   __ATOMIC_RELAXED);
       __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
       __atomic_load_n(&c1, __ATOMIC_RELAXED);
      ],
      [glibcxx_cv_atomic_int=yes],
      [glibcxx_cv_atomic_int=no])
  ])

  AC_CACHE_CHECK([for atomic builtins for long long],
    glibcxx_cv_atomic_long_long, [
    AC_TRY_LINK(
      [ ],
      [typedef long long atomic_type;
       atomic_type c1;
       atomic_type c2;
       atomic_type c3(0);
       __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
       __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
				   __ATOMIC_RELAXED);
       __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
       __atomic_load_n(&c1, __ATOMIC_RELAXED);
      ],
      [glibcxx_cv_atomic_long_long=yes],
      [glibcxx_cv_atomic_long_long=no])
  ])

  else

  # Do asm tests.

  # Compile unoptimized.
  CXXFLAGS='-O0 -S'

  # Fake what AC_TRY_COMPILE does.

    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
int main()
{
  typedef bool atomic_type;
  atomic_type c1;
  atomic_type c2;
  atomic_type c3(0);
  // N.B. __atomic_fetch_add is not supported for bool.
  __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
			      __ATOMIC_RELAXED);
  __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
  __atomic_load_n(&c1, __ATOMIC_RELAXED);

  return 0;
}
EOF

    AC_MSG_CHECKING([for atomic builtins for bool])
    if AC_TRY_EVAL(ac_compile); then
      if grep __atomic_ conftest.s >/dev/null 2>&1 ; then
	glibcxx_cv_atomic_bool=no
      else
	glibcxx_cv_atomic_bool=yes
      fi
    fi
    AC_MSG_RESULT($glibcxx_cv_atomic_bool)
    rm -f conftest*

    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
int main()
{
  typedef short atomic_type;
  atomic_type c1;
  atomic_type c2;
  atomic_type c3(0);
  __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
  __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
			      __ATOMIC_RELAXED);
  __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
  __atomic_load_n(&c1, __ATOMIC_RELAXED);

  return 0;
}
EOF

    AC_MSG_CHECKING([for atomic builtins for short])
    if AC_TRY_EVAL(ac_compile); then
      if grep __atomic_ conftest.s >/dev/null 2>&1 ; then
	glibcxx_cv_atomic_short=no
      else
	glibcxx_cv_atomic_short=yes
      fi
    fi
    AC_MSG_RESULT($glibcxx_cv_atomic_short)
    rm -f conftest*

    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
int main()
{
  // NB: _Atomic_word not necessarily int.
  typedef int atomic_type;
  atomic_type c1;
  atomic_type c2;
  atomic_type c3(0);
  __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
  __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
			      __ATOMIC_RELAXED);
  __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
  __atomic_load_n(&c1, __ATOMIC_RELAXED);

  return 0;
}
EOF

    AC_MSG_CHECKING([for atomic builtins for int])
    if AC_TRY_EVAL(ac_compile); then
      if grep __atomic_ conftest.s >/dev/null 2>&1 ; then
	glibcxx_cv_atomic_int=no
      else
	glibcxx_cv_atomic_int=yes
      fi
    fi
    AC_MSG_RESULT($glibcxx_cv_atomic_int)
    rm -f conftest*

    cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
int main()
{
  typedef long long atomic_type;
  atomic_type c1;
  atomic_type c2;
  atomic_type c3(0);
  __atomic_fetch_add(&c1, c2, __ATOMIC_RELAXED);
  __atomic_compare_exchange_n(&c1, &c2, c3, true, __ATOMIC_ACQ_REL,
			      __ATOMIC_RELAXED);
  __atomic_test_and_set(&c1, __ATOMIC_RELAXED);
  __atomic_load_n(&c1, __ATOMIC_RELAXED);

  return 0;
}
EOF

    AC_MSG_CHECKING([for atomic builtins for long long])
    if AC_TRY_EVAL(ac_compile); then
      if grep __atomic_ conftest.s >/dev/null 2>&1 ; then
	glibcxx_cv_atomic_long_long=no
      else
	glibcxx_cv_atomic_long_long=yes
      fi
    fi
    AC_MSG_RESULT($glibcxx_cv_atomic_long_long)
    rm -f conftest*

  fi

  CXXFLAGS="$old_CXXFLAGS"
  AC_LANG_RESTORE

  # Set atomicity_dir to builtins if all but the long long test above passes,
  # or if the builtins were already chosen (e.g. by configure.host).
  if { test "$glibcxx_cv_atomic_bool" = yes \
     && test "$glibcxx_cv_atomic_short" = yes \
     && test "$glibcxx_cv_atomic_int" = yes; } \
     || test "$atomicity_dir" = "cpu/generic/atomicity_builtins"; then
    AC_DEFINE(_GLIBCXX_ATOMIC_BUILTINS, 1,
    [Define if the compiler supports C++11 atomics.])
    atomicity_dir=cpu/generic/atomicity_builtins
  fi

  # If still generic, set to mutex.
  if test $atomicity_dir = "cpu/generic" ; then
    atomicity_dir=cpu/generic/atomicity_mutex
    AC_MSG_WARN([No native atomic operations are provided for this platform.])
      if test "x$target_thread_file" = xsingle; then
	AC_MSG_WARN([They cannot be faked when thread support is disabled.])
	AC_MSG_WARN([Thread-safety of certain classes is not guaranteed.])
      else
	AC_MSG_WARN([They will be faked using a mutex.])
	AC_MSG_WARN([Performance of certain classes will degrade as a result.])
      fi
  fi

])

dnl
dnl Set default lock policy for synchronizing shared_ptr reference counting.
dnl
dnl --with-libstdcxx-lock-policy=auto
dnl	Use atomic operations for shared_ptr reference counting only if
dnl	the default target supports atomic compare-and-swap.
dnl --with-libstdcxx-lock-policy=atomic
dnl	Use atomic operations for shared_ptr reference counting.
dnl --with-libstdcxx-lock-policy=mutex
dnl	Use a mutex to synchronize shared_ptr reference counting.
dnl
dnl This controls the value of __gnu_cxx::__default_lock_policy, which
dnl determines how shared_ptr reference counts are synchronized.
dnl The option "atomic" means that atomic operations should be used,
dnl "mutex" means that a mutex will be used. The default option, "auto",
dnl will check if the target supports the compiler-generated builtins
dnl for atomic compare-and-swap operations for 2-byte and 4-byte integers,
dnl and will use "atomic" if supported, "mutex" otherwise.
dnl This option is ignored if the thread model used by GCC is "single",
dnl as no synchronization is used at all in that case.
dnl This option affects the library ABI (except in the "single" thread model).
dnl
dnl Defines _GLIBCXX_HAVE_ATOMIC_LOCK_POLICY to 1 if atomics should be used.
dnl
AC_DEFUN([GLIBCXX_ENABLE_LOCK_POLICY], [

  AC_ARG_WITH([libstdcxx-lock-policy],
    AC_HELP_STRING([--with-libstdcxx-lock-policy={atomic,mutex,auto}],
      [synchronization policy for shared_ptr reference counting [default=auto]]),
              [libstdcxx_atomic_lock_policy=$withval],
              [libstdcxx_atomic_lock_policy=auto])

  case "$libstdcxx_atomic_lock_policy" in
    atomic|mutex|auto) ;;
    *) AC_MSG_ERROR([Invalid argument for --with-libstdcxx-lock-policy]) ;;
  esac
  AC_MSG_CHECKING([for lock policy for shared_ptr reference counts])

  if test x"$libstdcxx_atomic_lock_policy" = x"auto"; then
    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    ac_save_CXXFLAGS="$CXXFLAGS"

    dnl Why do we care about 2-byte CAS on targets with 4-byte _Atomic_word?!
    dnl Why don't we check 8-byte CAS for sparc64, where _Atomic_word is long?!
    dnl New targets should only check for CAS for the _Atomic_word type.
    AC_TRY_COMPILE([
    #if defined __riscv
    # error "Defaulting to mutex-based locks for ABI compatibility"
    #endif
    #if ! defined __GCC_HAVE_SYNC_COMPARE_AND_SWAP_2
    # error "No 2-byte compare-and-swap"
    #elif ! defined __GCC_HAVE_SYNC_COMPARE_AND_SWAP_4
    # error "No 4-byte compare-and-swap"
    #endif
    ],,
    [libstdcxx_atomic_lock_policy=atomic],
    [libstdcxx_atomic_lock_policy=mutex])
    AC_LANG_RESTORE
    CXXFLAGS="$ac_save_CXXFLAGS"
  fi

  if test x"$libstdcxx_atomic_lock_policy" = x"atomic"; then
    AC_MSG_RESULT(atomic)
    AC_DEFINE(HAVE_ATOMIC_LOCK_POLICY,1,
      [Defined if shared_ptr reference counting should use atomic operations.])
  else
    AC_MSG_RESULT(mutex)
  fi

])

dnl
dnl Allow visibility attributes to be used on namespaces, objects, etc.
dnl
dnl --enable-libstdcxx-visibility enables attempt to use visibility attributes.
dnl --disable-libstdcxx-visibility turns off all use of visibility attributes.
dnl  +  Usage:  GLIBCXX_ENABLE_LIBSTDCXX_VISIBILITY[(DEFAULT)]
dnl       Where DEFAULT is 'yes'.
dnl
AC_DEFUN([GLIBCXX_ENABLE_LIBSTDCXX_VISIBILITY], [
GLIBCXX_ENABLE(libstdcxx-visibility,$1,,[enables visibility safe usage])

if test x$enable_libstdcxx_visibility = xyes ; then
  dnl all hail libgfortran
  dnl Check whether the target supports hidden visibility.
  AC_CACHE_CHECK([whether the target supports hidden visibility],
		 glibcxx_cv_have_attribute_visibility, [
  save_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -Werror"
  AC_TRY_COMPILE([void __attribute__((visibility("hidden"))) foo(void) { }],
		 [], glibcxx_cv_have_attribute_visibility=yes,
		 glibcxx_cv_have_attribute_visibility=no)
  CFLAGS="$save_CFLAGS"])
  if test $glibcxx_cv_have_attribute_visibility = no; then
    enable_libstdcxx_visibility=no
  fi
fi

GLIBCXX_CONDITIONAL(ENABLE_VISIBILITY, test $enable_libstdcxx_visibility = yes)
AC_MSG_NOTICE([visibility supported: $enable_libstdcxx_visibility])
])


dnl
dnl Add version tags to symbols in shared library (or not), additionally
dnl marking other symbols as private/local (or not).
dnl
dnl Sets libtool_VERSION, and determines shared library SONAME.
dnl
dnl  This depends on GLIBCXX CHECK_LINKER_FEATURES, but without it assumes no.
dnl
dnl --enable-symvers=style adds a version script to the linker call when
dnl       creating the shared library.  The choice of version script is
dnl       controlled by 'style'.
dnl --disable-symvers does not.
dnl
dnl  +  Usage:  GLIBCXX_ENABLE_SYMVERS[(DEFAULT)]
dnl       Where DEFAULT is either 'yes' or 'no'.  Passing `yes' tries to
dnl       choose a default style based on linker characteristics.  Passing
dnl       'no' disables versioning.
dnl
AC_DEFUN([GLIBCXX_ENABLE_SYMVERS], [

GLIBCXX_ENABLE(symvers,$1,[[[=STYLE]]],
  [enables symbol versioning of the shared library],
  [permit yes|no|gnu|gnu-versioned-namespace|darwin|darwin-export|sun])

# If we never went through the GLIBCXX_CHECK_LINKER_FEATURES macro, then we
# don't know enough about $LD to do tricks...
AC_REQUIRE([GLIBCXX_CHECK_LINKER_FEATURES])
# Sun style symbol versions needs GNU c++filt for make_sunver.pl to work
# with extern "C++" in version scripts.
AC_REQUIRE([GCC_PROG_GNU_CXXFILT])

# Turn a 'yes' into a suitable default.
if test x$enable_symvers = xyes ; then
  if test $enable_shared = no || test "x$LD" = x || test x$gcc_no_link = xyes; then
    enable_symvers=no
  else
    if test $with_gnu_ld = yes ; then
      case ${target_os} in
	hpux*)
	  enable_symvers=no ;;
	*)
	  enable_symvers=gnu ;;
      esac
    else
      case ${target_os} in
	darwin*)
	  enable_symvers=darwin ;;
	# Sun symbol versioning exists since Solaris 2.5.
	solaris2.[[5-9]]* | solaris2.1[[0-9]]*)
	  # make_sunver.pl needs GNU c++filt to support extern "C++" in
	  # version scripts, so disable symbol versioning if none can be
	  # found.
	  if test -z "$ac_cv_path_CXXFILT"; then
	    AC_MSG_WARN([=== You have requested Sun symbol versioning, but])
	    AC_MSG_WARN([=== no GNU c++filt could  be found.])
	    AC_MSG_WARN([=== Symbol versioning will be disabled.])
	    enable_symvers=no
	  else
	    enable_symvers=sun
	  fi
	  ;;
	*)
	  enable_symvers=no ;;
      esac
    fi
  fi
fi

# Check to see if 'darwin' or 'darwin-export' can win.
if test x$enable_symvers = xdarwin-export ; then
    enable_symvers=darwin
fi

# Check if 'sun' was requested on non-Solaris 2 platforms.
if test x$enable_symvers = xsun ; then
  case ${target_os} in
    solaris2*)
      # All fine.
      ;;
    *)
      # Unlikely to work.
      AC_MSG_WARN([=== You have requested Sun symbol versioning, but])
      AC_MSG_WARN([=== you are not targetting Solaris 2.])
      AC_MSG_WARN([=== Symbol versioning will be disabled.])
      enable_symvers=no
      ;;
  esac
fi

# Check to see if 'gnu' can win.
if test $enable_symvers = gnu ||
  test $enable_symvers = gnu-versioned-namespace ||
  test $enable_symvers = sun; then
  # Check to see if libgcc_s exists, indicating that shared libgcc is possible.
  AC_MSG_CHECKING([for shared libgcc])
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=' -lgcc_s'
  AC_TRY_LINK(, [return 0;], glibcxx_shared_libgcc=yes, glibcxx_shared_libgcc=no)
  CFLAGS="$ac_save_CFLAGS"
  if test $glibcxx_shared_libgcc = no; then
    cat > conftest.c <<EOF
int main (void) { return 0; }
EOF
changequote(,)dnl
    glibcxx_libgcc_s_suffix=`${CC-cc} $CFLAGS $CPPFLAGS $LDFLAGS \
			     -shared -shared-libgcc -o conftest.so \
			     conftest.c -v 2>&1 >/dev/null \
			     | sed -n 's/^.* -lgcc_s\([^ ]*\) .*$/\1/p'`
changequote([,])dnl
    rm -f conftest.c conftest.so
    if test x${glibcxx_libgcc_s_suffix+set} = xset; then
      CFLAGS=" -lgcc_s$glibcxx_libgcc_s_suffix"
      AC_TRY_LINK(, [return 0;], glibcxx_shared_libgcc=yes)
      CFLAGS="$ac_save_CFLAGS"
    fi
  fi
  AC_MSG_RESULT($glibcxx_shared_libgcc)

  # For GNU ld, we need at least this version.  The format is described in
  # GLIBCXX_CHECK_LINKER_FEATURES above.
  glibcxx_min_gnu_ld_version=21400

  # If no shared libgcc, can't win.
  if test $glibcxx_shared_libgcc != yes; then
      AC_MSG_WARN([=== You have requested GNU symbol versioning, but])
      AC_MSG_WARN([=== you are not building a shared libgcc_s.])
      AC_MSG_WARN([=== Symbol versioning will be disabled.])
      enable_symvers=no
  elif test $with_gnu_ld != yes && test $enable_symvers = sun; then
    : All interesting versions of Sun ld support sun style symbol versioning.
  elif test $with_gnu_ld != yes ; then
    # just fail for now
    AC_MSG_WARN([=== You have requested GNU symbol versioning, but])
    AC_MSG_WARN([=== you are not using the GNU linker.])
    AC_MSG_WARN([=== Symbol versioning will be disabled.])
    enable_symvers=no
  elif test $glibcxx_ld_is_gold = yes ; then
    : All versions of gold support symbol versioning.
  elif test $glibcxx_ld_is_mold = yes ; then
    : All versions of mold support symbol versioning.
  elif test $glibcxx_gnu_ld_version -lt $glibcxx_min_gnu_ld_version ; then
    # The right tools, the right setup, but too old.  Fallbacks?
    AC_MSG_WARN(=== Linker version $glibcxx_gnu_ld_version is too old for)
    AC_MSG_WARN(=== full symbol versioning support in this release of GCC.)
    AC_MSG_WARN(=== You would need to upgrade your binutils to version)
    AC_MSG_WARN(=== $glibcxx_min_gnu_ld_version or later and rebuild GCC.)
    AC_MSG_WARN([=== Symbol versioning will be disabled.])
    enable_symvers=no
  fi
fi

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=6:30:0

# Everything parsed; figure out what files and settings to use.
case $enable_symvers in
  no)
    SYMVER_FILE=config/abi/pre/none.ver
    ;;
  gnu)
    SYMVER_FILE=config/abi/pre/gnu.ver
    AC_DEFINE(_GLIBCXX_SYMVER_GNU, 1,
	      [Define to use GNU versioning in the shared library.])
    ;;
  gnu-versioned-namespace)
    libtool_VERSION=8:0:0
    SYMVER_FILE=config/abi/pre/gnu-versioned-namespace.ver
    AC_DEFINE(_GLIBCXX_SYMVER_GNU_NAMESPACE, 1,
	      [Define to use GNU namespace versioning in the shared library.])
    ;;
  darwin)
    SYMVER_FILE=config/abi/pre/gnu.ver
    AC_DEFINE(_GLIBCXX_SYMVER_DARWIN, 1,
	      [Define to use darwin versioning in the shared library.])
    ;;
  sun)
    SYMVER_FILE=config/abi/pre/gnu.ver
    AC_DEFINE(_GLIBCXX_SYMVER_SUN, 1,
	      [Define to use Sun versioning in the shared library.])
    ;;
esac

if test x$enable_symvers != xno ; then
  AC_DEFINE(_GLIBCXX_SYMVER, 1,
	 [Define to use symbol versioning in the shared library.])
fi

AC_CACHE_CHECK([whether the target supports .symver directive],
	       glibcxx_cv_have_as_symver_directive, [
  AC_TRY_COMPILE([void foo (void); __asm (".symver foo, bar@SYMVER");],
		 [], glibcxx_cv_have_as_symver_directive=yes,
		 glibcxx_cv_have_as_symver_directive=no)])
if test $glibcxx_cv_have_as_symver_directive = yes; then
  AC_DEFINE(HAVE_AS_SYMVER_DIRECTIVE, 1,
    [Define to 1 if the target assembler supports .symver directive.])
fi

AC_SUBST(SYMVER_FILE)
AC_SUBST(port_specific_symbol_files)
GLIBCXX_CONDITIONAL(ENABLE_SYMVERS, test $enable_symvers != no)
GLIBCXX_CONDITIONAL(ENABLE_SYMVERS_GNU, test $enable_symvers = gnu)
GLIBCXX_CONDITIONAL(ENABLE_SYMVERS_GNU_NAMESPACE, test $enable_symvers = gnu-versioned-namespace)
GLIBCXX_CONDITIONAL(ENABLE_SYMVERS_DARWIN, test $enable_symvers = darwin)
GLIBCXX_CONDITIONAL(ENABLE_SYMVERS_SUN, test $enable_symvers = sun)
AC_MSG_NOTICE(versioning on shared library symbols is $enable_symvers)

if test $enable_symvers != no ; then
   case ${target_os} in
     # The Solaris 2 runtime linker doesn't support the GNU extension of
     # binding the same symbol to different versions
     solaris2*)
       ;;
     # Other platforms with GNU symbol versioning (GNU/Linux, more?) do.
     *)
       AC_DEFINE(HAVE_SYMVER_SYMBOL_RENAMING_RUNTIME_SUPPORT, 1,
	 [Define to 1 if the target runtime linker supports binding the same symbol to different versions.])
       ;;
    esac
fi

# Now, set up compatibility support, if any.
# In addition, need this to deal with std::size_t mangling in
# src/compatibility.cc.  In a perfect world, could use
# typeid(std::size_t).name()[0] to do direct substitution.
AC_MSG_CHECKING([for size_t as unsigned int])
ac_save_CFLAGS="$CFLAGS"
CFLAGS="-Werror"
AC_TRY_COMPILE(, [__SIZE_TYPE__* stp; unsigned int* uip; stp = uip;],
		 [glibcxx_size_t_is_i=yes], [glibcxx_size_t_is_i=no])
CFLAGS=$ac_save_CFLAGS
if test "$glibcxx_size_t_is_i" = yes; then
  AC_DEFINE(_GLIBCXX_SIZE_T_IS_UINT, 1, [Define if size_t is unsigned int.])
fi
AC_MSG_RESULT([$glibcxx_size_t_is_i])

AC_MSG_CHECKING([for ptrdiff_t as int])
ac_save_CFLAGS="$CFLAGS"
CFLAGS="-Werror"
AC_TRY_COMPILE(, [__PTRDIFF_TYPE__* ptp; int* ip; ptp = ip;],
		 [glibcxx_ptrdiff_t_is_i=yes], [glibcxx_ptrdiff_t_is_i=no])
CFLAGS=$ac_save_CFLAGS
if test "$glibcxx_ptrdiff_t_is_i" = yes; then
  AC_DEFINE(_GLIBCXX_PTRDIFF_T_IS_INT, 1, [Define if ptrdiff_t is int.])
fi
AC_MSG_RESULT([$glibcxx_ptrdiff_t_is_i])
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
dnl  thread_header
dnl
AC_DEFUN([GLIBCXX_ENABLE_THREADS], [
  AC_MSG_CHECKING([for thread model used by GCC])
  target_thread_file=`$CXX -v 2>&1 | sed -n 's/^Thread model: //p'`
  AC_MSG_RESULT([$target_thread_file])
  GCC_AC_THREAD_HEADER([$target_thread_file])
])


dnl
dnl Check if gthread implementation defines the types and functions
dnl required by the c++0x thread library.  Conforming gthread
dnl implementations can define __GTHREADS_CXX0X to enable use with c++0x.
dnl
dnl GLIBCXX_ENABLE_SYMVERS must be done before this.
dnl
AC_DEFUN([GLIBCXX_CHECK_GTHREADS], [
  GLIBCXX_ENABLE(libstdcxx-threads,auto,,[enable C++11 threads support])

  if test x$enable_libstdcxx_threads = xauto ||
     test x$enable_libstdcxx_threads = xyes; then

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS

  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions \
	-I${toplevel_srcdir}/libgcc -I${toplevel_builddir}/libgcc"

  target_thread_file=`$CXX -v 2>&1 | sed -n 's/^Thread model: //p'`
  case $target_thread_file in
    posix)
      CXXFLAGS="$CXXFLAGS -DSUPPORTS_WEAK -DGTHREAD_USE_WEAK -D_PTHREADS"
  esac

  AC_MSG_CHECKING([whether it can be safely assumed that mutex_timedlock is available])

  AC_TRY_COMPILE([#include <unistd.h>],
    [
      // In case of POSIX threads check _POSIX_TIMEOUTS.
      #if (defined(_PTHREADS) \
	  && (!defined(_POSIX_TIMEOUTS) || _POSIX_TIMEOUTS <= 0))
      #error
      #endif
    ], [ac_gthread_use_mutex_timedlock=1], [ac_gthread_use_mutex_timedlock=0])

  AC_DEFINE_UNQUOTED(_GTHREAD_USE_MUTEX_TIMEDLOCK, $ac_gthread_use_mutex_timedlock,
		     [Define to 1 if mutex_timedlock is available.])

  if test $ac_gthread_use_mutex_timedlock = 1 ; then res_mutex_timedlock=yes ;
  else res_mutex_timedlock=no ; fi
  AC_MSG_RESULT([$res_mutex_timedlock])

  AC_MSG_CHECKING([for gthreads library])

  AC_TRY_COMPILE([#include "gthr.h"],
    [
      #ifndef __GTHREADS_CXX0X
      #error
      #endif
    ], [ac_has_gthreads=yes], [ac_has_gthreads=no])
  else
    ac_has_gthreads=no
  fi

  AC_MSG_RESULT([$ac_has_gthreads])

  if test x"$ac_has_gthreads" = x"yes"; then
    AC_DEFINE(_GLIBCXX_HAS_GTHREADS, 1,
	      [Define if gthreads library is available.])

    # Also check for pthread_rwlock_t for std::shared_timed_mutex in C++14
    # but only do so if we're using pthread in the gthread library.
    # On VxWorks for example, pthread_rwlock_t is defined in sys/types.h
    # but the pthread library is not there by default and the gthread library
    # does not use it.
    AC_TRY_COMPILE([#include "gthr.h"],
    [
      #if (!defined(_PTHREADS))
      #error
      #endif
    ], [ac_gthread_use_pthreads=yes], [ac_gthread_use_pthreads=no])
    if test x"$ac_gthread_use_pthreads" = x"yes"; then
      AC_CHECK_TYPE([pthread_rwlock_t],
             [AC_DEFINE([_GLIBCXX_USE_PTHREAD_RWLOCK_T], 1,
             [Define if POSIX read/write locks are available in <gthr.h>.])],
             [],
             [#include "gthr.h"])
    fi
  fi

  AC_CHECK_HEADER(semaphore.h, [
    AC_MSG_CHECKING([for POSIX Semaphores and sem_timedwait])
    AC_TRY_COMPILE([
	#include <unistd.h>
	#include <semaphore.h>
	#include <limits.h>
      ],
      [
	#if !defined _POSIX_TIMEOUTS || _POSIX_TIMEOUTS <= 0
	# error "POSIX Timeouts option not supported"
	#elif !defined _POSIX_SEMAPHORES || _POSIX_SEMAPHORES <= 0
	# error "POSIX Semaphores option not supported"
	#else
	#if defined SEM_VALUE_MAX
	constexpr int sem_value_max = SEM_VALUE_MAX;
	#elif defined _POSIX_SEM_VALUE_MAX
	constexpr int sem_value_max = _POSIX_SEM_VALUE_MAX;
	#else
	# error "SEM_VALUE_MAX not available"
	#endif
	sem_t sem;
	sem_init(&sem, 0, sem_value_max);
	struct timespec ts = { 0 };
	sem_timedwait(&sem, &ts);
	#endif
      ],
      [ac_have_posix_semaphore=yes],
      [ac_have_posix_semaphore=no])],
      [ac_have_posix_semaphore=no])

  if test $ac_have_posix_semaphore = yes ; then
    AC_DEFINE(HAVE_POSIX_SEMAPHORE,
	      1,
	      [Define to 1 if POSIX Semaphores with sem_timedwait are available in <semaphore.h>.])
  fi
  AC_MSG_RESULT([$ac_have_posix_semaphore])

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])


# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file file be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU Public License
# but which still want to provide support for the GNU gettext functionality.
# Please note that the actual code is *not* freely available.
AC_DEFUN([AC_LC_MESSAGES], [
  AC_CHECK_HEADER(locale.h, [
    AC_CACHE_CHECK([for LC_MESSAGES], ac_cv_val_LC_MESSAGES,
      [AC_TRY_COMPILE([#include <locale.h>], [return LC_MESSAGES],
       ac_cv_val_LC_MESSAGES=yes, ac_cv_val_LC_MESSAGES=no)])
    if test $ac_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES, 1,
		[Define if LC_MESSAGES is available in <locale.h>.])
    fi
  ])
])

dnl
dnl Check whether rdrand is supported in the assembler.
AC_DEFUN([GLIBCXX_CHECK_X86_RDRAND], [
  AC_CACHE_CHECK([for rdrand support in assembler],
  ac_cv_x86_rdrand, [
  ac_cv_x86_rdrand=no
  case "$target" in
    i?86-*-* | \
    x86_64-*-*)
    AC_TRY_COMPILE(, [asm("rdrand %eax");],
		[ac_cv_x86_rdrand=yes], [ac_cv_x86_rdrand=no])
  esac
  ])
  if test $ac_cv_x86_rdrand = yes; then
    AC_DEFINE(_GLIBCXX_X86_RDRAND, 1,
		[ Defined if as can handle rdrand. ])
  fi
])

dnl
dnl Check whether rdseed is supported in the assembler.
AC_DEFUN([GLIBCXX_CHECK_X86_RDSEED], [
  AC_CACHE_CHECK([for rdseed support in assembler],
  ac_cv_x86_rdseed, [
  ac_cv_x86_rdseed=no
  case "$target" in
    i?86-*-* | \
    x86_64-*-*)
    AC_TRY_COMPILE(, [asm("rdseed %eax");],
		[ac_cv_x86_rdseed=yes], [ac_cv_x86_rdseed=no])
  esac
  ])
  if test $ac_cv_x86_rdseed = yes; then
    AC_DEFINE(_GLIBCXX_X86_RDSEED, 1,
		[ Defined if as can handle rdseed. ])
  fi
])

dnl
dnl Check whether get_nprocs is available in <sys/sysinfo.h>, and define _GLIBCXX_USE_GET_NPROCS.
dnl
AC_DEFUN([GLIBCXX_CHECK_GET_NPROCS], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for get_nprocs],
    glibcxx_cv_GET_NPROCS, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <sys/sysinfo.h>],
      [int n = get_nprocs();],
      [glibcxx_cv_GET_NPROCS=yes],
      [glibcxx_cv_GET_NPROCS=no])
  ])
  if test $glibcxx_cv_GET_NPROCS = yes; then
    AC_DEFINE(_GLIBCXX_USE_GET_NPROCS, 1, [Define if get_nprocs is available in <sys/sysinfo.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether sysconf(_SC_NPROCESSORS_ONLN) is available in <unistd.h>, and define _GLIBCXX_USE_SC_NPROCESSORS_ONLN.
dnl
AC_DEFUN([GLIBCXX_CHECK_SC_NPROCESSORS_ONLN], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for _SC_NPROCESSORS_ONLN],
    glibcxx_cv_SC_NPROCESSORS_ONLN, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [int n = sysconf(_SC_NPROCESSORS_ONLN);],
      [glibcxx_cv_SC_NPROCESSORS_ONLN=yes],
      [glibcxx_cv_SC_NPROCESSORS_ONLN=no])
  ])
  if test $glibcxx_cv_SC_NPROCESSORS_ONLN = yes; then
    AC_DEFINE(_GLIBCXX_USE_SC_NPROCESSORS_ONLN, 1, [Define if _SC_NPROCESSORS_ONLN  is available in <unistd.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether sysconf(_SC_NPROC_ONLN) is available in <unistd.h>, and define _GLIBCXX_USE_SC_NPROC_ONLN.
dnl
AC_DEFUN([GLIBCXX_CHECK_SC_NPROC_ONLN], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for _SC_NPROC_ONLN],
    glibcxx_cv_SC_NPROC_ONLN, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [int n = sysconf(_SC_NPROC_ONLN);],
      [glibcxx_cv_SC_NPROC_ONLN=yes],
      [glibcxx_cv_SC_NPROC_ONLN=no])
  ])
  if test $glibcxx_cv_SC_NPROC_ONLN = yes; then
    AC_DEFINE(_GLIBCXX_USE_SC_NPROC_ONLN, 1, [Define if _SC_NPROC_ONLN  is available in <unistd.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether pthread_num_processors_np is available in <pthread.h>, and define _GLIBCXX_USE_PTHREADS_NUM_PROCESSORS_NP.
dnl
AC_DEFUN([GLIBCXX_CHECK_PTHREADS_NUM_PROCESSORS_NP], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for pthreads_num_processors_np],
    glibcxx_cv_PTHREADS_NUM_PROCESSORS_NP, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <pthread.h>],
      [int n = pthread_num_processors_np();],
      [glibcxx_cv_PTHREADS_NUM_PROCESSORS_NP=yes],
      [glibcxx_cv_PTHREADS_NUM_PROCESSORS_NP=no])
  ])
  if test $glibcxx_cv_PTHREADS_NUM_PROCESSORS_NP = yes; then
    AC_DEFINE(_GLIBCXX_USE_PTHREADS_NUM_PROCESSORS_NP, 1, [Define if pthreads_num_processors_np is available in <pthread.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether pthread_cond_clockwait is available in <pthread.h> for std::condition_variable to use,
dnl and define _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT.
dnl
AC_DEFUN([GLIBCXX_CHECK_PTHREAD_COND_CLOCKWAIT], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
  ac_save_LIBS="$LIBS"
  LIBS="$LIBS -lpthread"

  AC_CACHE_CHECK([for pthread_cond_clockwait],
    glibcxx_cv_PTHREAD_COND_CLOCKWAIT, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <pthread.h>],
      [pthread_mutex_t mutex; pthread_cond_t cond; struct timespec ts; int n = pthread_cond_clockwait(&cond, &mutex, 0, &ts);],
      [glibcxx_cv_PTHREAD_COND_CLOCKWAIT=yes],
      [glibcxx_cv_PTHREAD_COND_CLOCKWAIT=no])
  ])
  if test $glibcxx_cv_PTHREAD_COND_CLOCKWAIT = yes; then
    AC_DEFINE(_GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT, 1, [Define if pthread_cond_clockwait is available in <pthread.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  LIBS="$ac_save_LIBS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether pthread_mutex_clocklock is available in <pthread.h> for std::timed_mutex to use,
dnl and define _GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK.
dnl
AC_DEFUN([GLIBCXX_CHECK_PTHREAD_MUTEX_CLOCKLOCK], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
  ac_save_LIBS="$LIBS"
  LIBS="$LIBS -lpthread"

  AC_CACHE_CHECK([for pthread_mutex_clocklock],
    glibcxx_cv_PTHREAD_MUTEX_CLOCKLOCK, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <pthread.h>],
      [pthread_mutex_t mutex; struct timespec ts; int n = pthread_mutex_clocklock(&mutex, CLOCK_REALTIME, &ts);],
      [glibcxx_cv_PTHREAD_MUTEX_CLOCKLOCK=yes],
      [glibcxx_cv_PTHREAD_MUTEX_CLOCKLOCK=no])
  ])
  if test $glibcxx_cv_PTHREAD_MUTEX_CLOCKLOCK = yes; then
    AC_DEFINE(_GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK, 1, [Define if pthread_mutex_clocklock is available in <pthread.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  LIBS="$ac_save_LIBS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether pthread_mutex_clocklock is available in <pthread.h> for std::timed_mutex to use,
dnl and define _GLIBCXX_USE_PTHREAD_MUTEX_CLOCKLOCK.
dnl
AC_DEFUN([GLIBCXX_CHECK_PTHREAD_RWLOCK_CLOCKLOCK], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
  ac_save_LIBS="$LIBS"
  LIBS="$LIBS -lpthread"

  AC_CACHE_CHECK([for pthread_rwlock_clockrdlock, pthread_wlock_clockwrlock],
    glibcxx_cv_PTHREAD_RWLOCK_CLOCKLOCK, [
    GCC_TRY_COMPILE_OR_LINK(
      [#include <pthread.h>],
      [pthread_rwlock_t rwl; struct timespec ts;]
      [int n = pthread_rwlock_clockrdlock(&rwl, CLOCK_REALTIME, &ts);]
      [int m = pthread_rwlock_clockwrlock(&rwl, CLOCK_REALTIME, &ts);],
      [glibcxx_cv_PTHREAD_RWLOCK_CLOCKLOCK=yes],
      [glibcxx_cv_PTHREAD_RWLOCK_CLOCKLOCK=no])
  ])
  if test $glibcxx_cv_PTHREAD_RWLOCK_CLOCKLOCK = yes; then
    AC_DEFINE(_GLIBCXX_USE_PTHREAD_RWLOCK_CLOCKLOCK, 1, [Define if pthread_rwlock_clockrdlock and pthread_rwlock_clockwrlock are available in <pthread.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  LIBS="$ac_save_LIBS"
  AC_LANG_RESTORE
])

dnl
dnl Check whether sysctl is available in <pthread.h>, and define _GLIBCXX_USE_SYSCTL_HW_NCPU.
dnl
AC_DEFUN([GLIBCXX_CHECK_SYSCTL_HW_NCPU], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"

  AC_CACHE_CHECK([for hw.ncpu sysctl],
    glibcxx_cv_SYSCTL_HW_NCPU, [
    GCC_TRY_COMPILE_OR_LINK(
      [
       #include <stddef.h>
       #include <sys/sysctl.h>
       ],
      [
       int count;
       size_t size = sizeof(count);
       int mib[] = { CTL_HW, HW_NCPU };
       sysctl(mib, 2, &count, &size, NULL, 0);
      ],
      [glibcxx_cv_SYSCTL_HW_NCPU=yes],
      [glibcxx_cv_SYSCTL_HW_NCPU=no])
  ])
  if test $glibcxx_cv_SYSCTL_HW_NCPU = yes; then
    AC_DEFINE(_GLIBCXX_USE_SYSCTL_HW_NCPU, 1, [Define if sysctl(), CTL_HW and HW_NCPU are available in <sys/sysctl.h>.])
  fi

  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check to see if python pretty printing can be activated.
dnl
dnl --with-python-dir=dir
dnl installs directory into $prefix/dir
AC_DEFUN([GLIBCXX_ENABLE_PYTHON], [

AC_MSG_CHECKING([for custom python install directory])
AC_ARG_WITH([python-dir],
	    AS_HELP_STRING([--with-python-dir],
			   [the location to install Python modules. This path is relative starting from the prefix.]),
	    [with_python_dir=$withval], [with_python_dir="no"])
AC_MSG_RESULT(${with_python_dir})

# Needed for installing Python modules during make install.
python_mod_dir="${with_python_dir}"
AC_SUBST(python_mod_dir)
GLIBCXX_CONDITIONAL(ENABLE_PYTHONDIR, test $python_mod_dir != no)
])

dnl
dnl Check to see if -Werror is disabled.
dnl
dnl --enable-werror/--disable-werror
AC_DEFUN([GLIBCXX_ENABLE_WERROR], [
  AC_MSG_CHECKING([for -Werror])
  GLIBCXX_ENABLE(werror,$1,,[turns on -Werror])
  AC_MSG_RESULT($enable_werror)
  GLIBCXX_CONDITIONAL(ENABLE_WERROR, test $enable_werror = yes)
])

dnl
dnl Check whether obsolescent tmpnam is available in <stdio.h>,
dnl and define _GLIBCXX_USE_TMPNAM.
dnl
AC_DEFUN([GLIBCXX_CHECK_TMPNAM], [dnl
dnl
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
dnl
  AC_CACHE_CHECK([for tmpnam], glibcxx_cv_TMPNAM, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <stdio.h>],
      [char *tmp = tmpnam(NULL);],
      [glibcxx_cv_TMPNAM=yes],
      [glibcxx_cv_TMPNAM=no])
  ])
  if test $glibcxx_cv_TMPNAM = yes; then
    AC_DEFINE(_GLIBCXX_USE_TMPNAM, 1, [Define if obsolescent tmpnam is available in <stdio.h>.])
  fi
dnl
  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check to see if sys/sdt.h exists and that it is suitable for use.
dnl Some versions of sdt.h were not compatible with C++11.
dnl
AC_DEFUN([GLIBCXX_CHECK_SDT_H], [
  # Note that this test has to be run with the C language.
  # Otherwise, sdt.h will try to include some headers from
  # libstdc++ itself.
  AC_LANG_SAVE
  AC_LANG_C
  AC_CACHE_CHECK([for suitable sys/sdt.h],
    glibcxx_cv_sys_sdt_h, [
    # Because we have to run the test in C, we use grep rather
    # than the compiler to check for the bug.  The bug is that
    # were strings without trailing whitespace, causing g++
    # to look for operator"".  The pattern searches for the fixed
    # output.
    AC_EGREP_CPP([ \",\" ], [
      #include <sys/sdt.h>
      int f() { STAP_PROBE(hi, bob); }
    ], [glibcxx_cv_sys_sdt_h=yes], [glibcxx_cv_sys_sdt_h=no])
  ])
  AC_LANG_RESTORE
  if test $glibcxx_cv_sys_sdt_h = yes; then
    AC_DEFINE(HAVE_SYS_SDT_H, 1,
              [Define to 1 if you have a suitable <sys/sdt.h> header file])
  fi
])

dnl
dnl Control whether the library should define symbols for old and new ABIs.
dnl This affects definitions of strings, stringstreams and locale facets.
dnl
dnl --disable-libstdcxx-dual-abi will use old ABI for all types.
dnl
dnl Defines:
dnl  _GLIBCXX_USE_DUAL_ABI (always defined, either to 1 or 0)
dnl
AC_DEFUN([GLIBCXX_ENABLE_LIBSTDCXX_DUAL_ABI], [
  GLIBCXX_ENABLE(libstdcxx-dual-abi,$1,,[support two versions of std::string])
  if test x$enable_symvers = xgnu-versioned-namespace; then
    # gnu-versioned-namespace is incompatible with the dual ABI.
    enable_libstdcxx_dual_abi="no"
  fi
  if test x"$enable_libstdcxx_dual_abi" != xyes; then
    AC_MSG_NOTICE([dual ABI is disabled])
    default_libstdcxx_abi="gcc4-compatible"
  fi
  GLIBCXX_CONDITIONAL(ENABLE_DUAL_ABI, test $enable_libstdcxx_dual_abi = yes)
])

dnl
dnl Check to see which ABI should be enabled by default.
dnl
dnl --with-default-libstdcxx-abi={gcc4-compatible,new}
dnl
dnl Defines:
dnl  _GLIBCXX_USE_CXX11_ABI (always defined, either to 1 or 0)
dnl
AC_DEFUN([GLIBCXX_DEFAULT_ABI], [
  if test x$enable_libstdcxx_dual_abi = xyes; then
  AC_MSG_CHECKING([for default std::string ABI to use])
  AC_ARG_WITH([default-libstdcxx-abi],
    AS_HELP_STRING([--with-default-libstdcxx-abi],
                   [set the std::string ABI to use by default]),
    [case "$withval" in
      gcc4-compatible)  default_libstdcxx_abi="gcc4-compatible" ;;
      new|cxx11)  default_libstdcxx_abi="new" ;;
      c++*|gnu++*) AC_MSG_ERROR([Supported arguments for --with-default-libstdcxx-abi have changed, use "new" or "gcc4-compatible"]) ;;
      *) AC_MSG_ERROR([Invalid argument for --with-default-libstdcxx-abi]) ;;
     esac
     ],
    [default_libstdcxx_abi="new"])
  AC_MSG_RESULT(${default_libstdcxx_abi})
  fi
  if test $default_libstdcxx_abi = "new"; then
    glibcxx_cxx11_abi=1
    glibcxx_cxx98_abi=0
  else
    glibcxx_cxx11_abi=0
    glibcxx_cxx98_abi=1
  fi
  AC_SUBST(glibcxx_cxx98_abi)
  GLIBCXX_CONDITIONAL(ENABLE_CXX11_ABI, test $glibcxx_cxx11_abi = 1)
])

dnl
dnl Check to see whether to build libstdc++fs.a
dnl
dnl --enable-libstdcxx-filesystem-ts
dnl
AC_DEFUN([GLIBCXX_ENABLE_FILESYSTEM_TS], [
  GLIBCXX_ENABLE(libstdcxx-filesystem-ts,auto,,
    [turns on ISO/IEC TS 18822 support],
    [permit yes|no|auto])

  AC_MSG_CHECKING([whether to build Filesystem TS support])
  if test x"$ac_cv_header_dirent_h" != x"yes"; then
    enable_libstdcxx_filesystem_ts=no
  fi
  if test x"$enable_libstdcxx_filesystem_ts" = x"auto"; then
    case "${target_os}" in
      freebsd*|netbsd*|openbsd*|dragonfly*|darwin*)
        enable_libstdcxx_filesystem_ts=yes
        ;;
      gnu* | linux* | kfreebsd*-gnu | knetbsd*-gnu | uclinux*)
        enable_libstdcxx_filesystem_ts=yes
        ;;
      rtems*)
        enable_libstdcxx_filesystem_ts=yes
        ;;
      solaris*)
        enable_libstdcxx_filesystem_ts=yes
        ;;
      mingw*)
        enable_libstdcxx_filesystem_ts=yes
        ;;
      *)
        enable_libstdcxx_filesystem_ts=no
        ;;
    esac
  fi
  AC_MSG_RESULT($enable_libstdcxx_filesystem_ts)
  GLIBCXX_CONDITIONAL(ENABLE_FILESYSTEM_TS, test $enable_libstdcxx_filesystem_ts = yes)
])

dnl
dnl Check whether the library calls required by the C++17 Filesystem library
dnl and the Filesystem TS are present.
dnl Defines:
dnl  HAVE_STRUCT_DIRENT_D_TYPE
dnl  _GLIBCXX_USE_REALPATH
dnl  _GLIBCXX_USE_UTIMENSAT
dnl  _GLIBCXX_USE_ST_MTIM
dnl  _GLIBCXX_USE_FCHMOD
dnl  _GLIBCXX_USE_FCHMODAT
dnl  _GLIBCXX_USE_SENDFILE
dnl  HAVE_LINK
dnl  HAVE_READLINK
dnl  HAVE_SYMLINK
dnl
AC_DEFUN([GLIBCXX_CHECK_FILESYSTEM_DEPS], [dnl
dnl
  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  ac_save_CXXFLAGS="$CXXFLAGS"
  CXXFLAGS="$CXXFLAGS -fno-exceptions"
dnl
  AC_CACHE_CHECK([for struct dirent.d_type], glibcxx_cv_dirent_d_type, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <dirent.h>],
      [
       struct dirent d;
       if (sizeof d.d_type) return 0;
      ],
      [glibcxx_cv_dirent_d_type=yes],
      [glibcxx_cv_dirent_d_type=no])
  ])
  if test $glibcxx_cv_dirent_d_type = yes; then
    AC_DEFINE(HAVE_STRUCT_DIRENT_D_TYPE, 1, [Define to 1 if `d_type' is a member of `struct dirent'.])
  fi
dnl
  AC_CACHE_CHECK([for realpath], glibcxx_cv_realpath, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [
       #include <limits.h>
       #include <stdlib.h>
       #include <unistd.h>
      ],
      [
       #if _XOPEN_VERSION < 500
       #error
       #elif _XOPEN_VERSION >= 700 || defined(PATH_MAX)
       char *tmp = realpath((const char*)NULL, (char*)NULL);
       #else
       #error
       #endif
      ],
      [glibcxx_cv_realpath=yes],
      [glibcxx_cv_realpath=no])
  ])
  if test $glibcxx_cv_realpath = yes; then
    AC_DEFINE(_GLIBCXX_USE_REALPATH, 1, [Define if usable realpath is available in <stdlib.h>.])
  fi
dnl
  AC_CACHE_CHECK([for utimensat], glibcxx_cv_utimensat, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [
	#include <fcntl.h>
	#include <sys/stat.h>
      ],
      [
	struct timespec ts[2] = { { 0, UTIME_OMIT }, { 1, 1 } };
	int i = utimensat(AT_FDCWD, "path", ts, 0);
      ],
      [glibcxx_cv_utimensat=yes],
      [glibcxx_cv_utimensat=no])
  ])
  if test $glibcxx_cv_utimensat = yes; then
    AC_DEFINE(_GLIBCXX_USE_UTIMENSAT, 1, [Define if utimensat and UTIME_OMIT are available in <sys/stat.h> and AT_FDCWD in <fcntl.h>.])
  fi
dnl
  AC_CACHE_CHECK([for utime], glibcxx_cv_utime, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [
	#include <utime.h>
      ],
      [
	struct utimbuf t = { 1, 1 };
	int i = utime("path", &t);
      ],
      [glibcxx_cv_utime=yes],
      [glibcxx_cv_utime=no])
  ])
  if test $glibcxx_cv_utime = yes; then
    AC_DEFINE(_GLIBCXX_USE_UTIME, 1, [Define if utime is available in <utime.h>.])
  fi
dnl
  AC_CACHE_CHECK([for lstat], glibcxx_cv_lstat, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [ #include <sys/stat.h> ],
      [
	struct stat st;
	int i = lstat("path", &st);
      ],
      [glibcxx_cv_lstat=yes],
      [glibcxx_cv_lstat=no])
  ])
  if test $glibcxx_cv_lstat = yes; then
    AC_DEFINE(_GLIBCXX_USE_LSTAT, 1, [Define if lstat is available in <sys/stat.h>.])
  fi
dnl
  AC_CACHE_CHECK([for struct stat.st_mtim.tv_nsec],
    glibcxx_cv_st_mtim, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [ #include <sys/stat.h> ],
      [
	struct stat st;
	return st.st_mtim.tv_nsec;
      ],
      [glibcxx_cv_st_mtim=yes],
      [glibcxx_cv_st_mtim=no])
  ])
  if test $glibcxx_cv_st_mtim = yes; then
    AC_DEFINE(_GLIBCXX_USE_ST_MTIM, 1, [Define if struct stat has timespec members.])
  fi
dnl
  AC_CACHE_CHECK([for fchmod],
    glibcxx_cv_fchmod, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <sys/stat.h>],
      [fchmod(1, S_IWUSR);],
      [glibcxx_cv_fchmod=yes],
      [glibcxx_cv_fchmod=no])
  ])
  if test $glibcxx_cv_fchmod = yes; then
    AC_DEFINE(_GLIBCXX_USE_FCHMOD, 1, [Define if fchmod is available in <sys/stat.h>.])
  fi
dnl
  AC_CACHE_CHECK([for fchmodat],
    glibcxx_cv_fchmodat, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [
	#include <fcntl.h>
	#include <sys/stat.h>
      ],
      [fchmodat(AT_FDCWD, "", 0, AT_SYMLINK_NOFOLLOW);],
      [glibcxx_cv_fchmodat=yes],
      [glibcxx_cv_fchmodat=no])
  ])
  if test $glibcxx_cv_fchmodat = yes; then
    AC_DEFINE(_GLIBCXX_USE_FCHMODAT, 1, [Define if fchmodat is available in <sys/stat.h>.])
  fi
dnl
  AC_CACHE_CHECK([for sendfile that can copy files],
    glibcxx_cv_sendfile, [dnl
    case "${target_os}" in
      gnu* | linux* | solaris* | uclinux*)
	GCC_TRY_COMPILE_OR_LINK(
	  [#include <sys/sendfile.h>],
	  [sendfile(1, 2, (off_t*)0, sizeof 1);],
	  [glibcxx_cv_sendfile=yes],
	  [glibcxx_cv_sendfile=no])
	;;
      *)
	glibcxx_cv_sendfile=no
	;;
    esac
  ])
  if test $glibcxx_cv_sendfile = yes; then
    AC_DEFINE(_GLIBCXX_USE_SENDFILE, 1, [Define if sendfile is available in <sys/sendfile.h>.])
  fi
dnl
  AC_CACHE_CHECK([for link],
    glibcxx_cv_link, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [link("", "");],
      [glibcxx_cv_link=yes],
      [glibcxx_cv_link=no])
  ])
  if test $glibcxx_cv_link = yes; then
    AC_DEFINE(HAVE_LINK, 1, [Define if link is available in <unistd.h>.])
  fi
dnl
  AC_CACHE_CHECK([for readlink],
    glibcxx_cv_readlink, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [char buf[32]; readlink("", buf, sizeof(buf));],
      [glibcxx_cv_readlink=yes],
      [glibcxx_cv_readlink=no])
  ])
  if test $glibcxx_cv_readlink = yes; then
    AC_DEFINE(HAVE_READLINK, 1, [Define if readlink is available in <unistd.h>.])
  fi
dnl
  AC_CACHE_CHECK([for symlink],
    glibcxx_cv_symlink, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [symlink("", "");],
      [glibcxx_cv_symlink=yes],
      [glibcxx_cv_symlink=no])
  ])
  if test $glibcxx_cv_symlink = yes; then
    AC_DEFINE(HAVE_SYMLINK, 1, [Define if symlink is available in <unistd.h>.])
  fi
dnl
  AC_CACHE_CHECK([for truncate],
    glibcxx_cv_truncate, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <unistd.h>],
      [truncate("", 99);],
      [glibcxx_cv_truncate=yes],
      [glibcxx_cv_truncate=no])
  ])
  if test $glibcxx_cv_truncate = yes; then
    AC_DEFINE(HAVE_TRUNCATE, 1, [Define if truncate is available in <unistd.h>.])
  fi
dnl
  AC_CACHE_CHECK([for fdopendir],
    glibcxx_cv_fdopendir, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <dirent.h>],
      [::DIR* dir = ::fdopendir(1);],
      [glibcxx_cv_fdopendir=yes],
      [glibcxx_cv_fdopendir=no])
  ])
  if test $glibcxx_cv_fdopendir = yes; then
    AC_DEFINE(HAVE_FDOPENDIR, 1, [Define if fdopendir is available in <dirent.h>.])
  fi
dnl
  AC_CACHE_CHECK([for dirfd],
    glibcxx_cv_dirfd, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <dirent.h>],
      [int fd = ::dirfd((::DIR*)0);],
      [glibcxx_cv_dirfd=yes],
      [glibcxx_cv_dirfd=no])
  ])
  if test $glibcxx_cv_dirfd = yes; then
    AC_DEFINE(HAVE_DIRFD, 1, [Define if dirfd is available in <dirent.h>.])
  fi
dnl
  AC_CACHE_CHECK([for openat],
    glibcxx_cv_openat, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <fcntl.h>],
      [int fd = ::openat(AT_FDCWD, "", 0);],
      [glibcxx_cv_openat=yes],
      [glibcxx_cv_openat=no])
  ])
  if test $glibcxx_cv_openat = yes; then
    AC_DEFINE(HAVE_OPENAT, 1, [Define if openat is available in <fcntl.h>.])
  fi
dnl
  AC_CACHE_CHECK([for unlinkat],
    glibcxx_cv_unlinkat, [dnl
    GCC_TRY_COMPILE_OR_LINK(
      [#include <fcntl.h>
       #include <unistd.h>],
      [::unlinkat(AT_FDCWD, "", AT_REMOVEDIR);],
      [glibcxx_cv_unlinkat=yes],
      [glibcxx_cv_unlinkat=no])
  ])
  if test $glibcxx_cv_unlinkat = yes; then
    AC_DEFINE(HAVE_UNLINKAT, 1, [Define if unlinkat is available in <fcntl.h>.])
  fi
dnl
  CXXFLAGS="$ac_save_CXXFLAGS"
  AC_LANG_RESTORE
])

dnl
dnl Check how size_t is mangled.  Copied from libitm.
dnl
AC_DEFUN([GLIBCXX_CHECK_SIZE_T_MANGLING], [
  AC_CACHE_CHECK([how size_t is mangled],
                 glibcxx_cv_size_t_mangling, [
    AC_TRY_COMPILE([], [extern __SIZE_TYPE__ x; extern unsigned long x;],
                   [glibcxx_cv_size_t_mangling=m], [
      AC_TRY_COMPILE([], [extern __SIZE_TYPE__ x; extern unsigned int x;],
                     [glibcxx_cv_size_t_mangling=j], [
        AC_TRY_COMPILE([],
                       [extern __SIZE_TYPE__ x; extern unsigned long long x;],
                       [glibcxx_cv_size_t_mangling=y], [
          AC_TRY_COMPILE([],
                         [extern __SIZE_TYPE__ x; extern unsigned short x;],
                         [glibcxx_cv_size_t_mangling=t], [
            AC_TRY_COMPILE([],
                           [extern __SIZE_TYPE__ x; extern __int20 unsigned x;],
                           [glibcxx_cv_size_t_mangling=u6uint20],
                           [glibcxx_cv_size_t_mangling=x])
          ])
        ])
      ])
    ])
  ])
  if test $glibcxx_cv_size_t_mangling = x; then
    AC_MSG_ERROR([Unknown underlying type for size_t])
  fi
  AC_DEFINE_UNQUOTED(_GLIBCXX_MANGLE_SIZE_T, [$glibcxx_cv_size_t_mangling],
    [Define to the letter to which size_t is mangled.])
])

dnl
dnl Determine whether std::exception_ptr symbols should be exported with
dnl the symbol versions from GCC 4.6.0 or GCC 7.1.0, depending on which
dnl release first added support for std::exception_ptr. Originally it was
dnl only supported for targets with always-lock-free atomics for int, but
dnl since GCC 7.1 it is supported for all targets.
dnl
AC_DEFUN([GLIBCXX_CHECK_EXCEPTION_PTR_SYMVER], [
  if test $enable_symvers != no; then
    AC_MSG_CHECKING([for first version to support std::exception_ptr])
    case ${target} in
      aarch64-*-* | alpha-*-* | hppa*-*-* | i?86-*-* | x86_64-*-* | \
      m68k-*-* | powerpc*-*-* | s390*-*-* | *-*-solaris* )
        ac_exception_ptr_since_gcc46=yes
        ;;
      *)
        # If the value of this macro changes then we will need to hardcode
        # yes/no here for additional targets based on the original value.
        AC_TRY_COMPILE([], [
          #if __GCC_ATOMIC_INT_LOCK_FREE <= 1
          # error atomic int not always lock free
          #endif
          ],
          [ac_exception_ptr_since_gcc46=yes],
          [ac_exception_ptr_since_gcc46=no])
        ;;
    esac
    if test x"$ac_exception_ptr_since_gcc46" = x"yes" ; then
      AC_DEFINE(HAVE_EXCEPTION_PTR_SINCE_GCC46, 1,
        [Define to 1 if GCC 4.6 supported std::exception_ptr for the target])
      AC_MSG_RESULT([4.6.0])
    else
      AC_MSG_RESULT([7.1.0])
    fi
  fi
])

dnl
dnl Check whether getentropy is present in <unistd.h>.
dnl
AC_DEFUN([GLIBCXX_CHECK_GETENTROPY], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_CACHE_CHECK([for getentropy], glibcxx_cv_getentropy, [
      GCC_TRY_COMPILE_OR_LINK(
	[#include <unistd.h>],
	[unsigned i;
	 ::getentropy(&i, sizeof(i));],
	[glibcxx_cv_getentropy=yes], [glibcxx_cv_getentropy=no])
    ])

  if test $glibcxx_cv_getentropy = yes; then
    AC_DEFINE(HAVE_GETENTROPY, 1, [Define if getentropy is available in <unistd.h>.])
  fi
  AC_LANG_RESTORE
])

dnl
dnl Check whether arc4random is present in <stdlib.h>.
dnl
AC_DEFUN([GLIBCXX_CHECK_ARC4RANDOM], [

  AC_LANG_SAVE
  AC_LANG_CPLUSPLUS
  AC_CACHE_CHECK([for arc4random], glibcxx_cv_arc4random, [
      GCC_TRY_COMPILE_OR_LINK(
	[#include <stdlib.h>],
	[unsigned i = ::arc4random();],
	[glibcxx_cv_arc4random=yes], [glibcxx_cv_arc4random=no])
    ])

  if test $glibcxx_cv_arc4random = yes; then
    AC_DEFINE(HAVE_ARC4RANDOM, 1, [Define if arc4random is available in <stdlib.h>.])
  fi
  AC_LANG_RESTORE
])

dnl
dnl Check to see whether to build libstdc++_libbacktrace.a
dnl
dnl --enable-libstdcxx-backtrace
dnl
AC_DEFUN([GLIBCXX_ENABLE_BACKTRACE], [
  GLIBCXX_ENABLE(libstdcxx-backtrace,auto,,
    [turns on libbacktrace support],
    [permit yes|no|auto])

  # Most of this is adapted from libsanitizer/configure.ac

  BACKTRACE_CPPFLAGS=

  # libbacktrace only needs atomics for int, which we've already tested
  if test "$glibcxx_cv_atomic_int" = "yes"; then
    BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DHAVE_ATOMIC_FUNCTIONS=1"
  fi

  # Test for __sync support.
  AC_CACHE_CHECK([__sync extensions],
  [glibcxx_cv_sys_sync],
  [GCC_TRY_COMPILE_OR_LINK(
     [int i;],
     [__sync_bool_compare_and_swap (&i, i, i);
     __sync_lock_test_and_set (&i, 1);
     __sync_lock_release (&i);],
     [glibcxx_cv_sys_sync=yes],
     [glibcxx_cv_sys_sync=no])
  ])
  if test "$glibcxx_cv_sys_sync" = "yes"; then
    BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DHAVE_SYNC_FUNCTIONS=1"
  fi

  # Check for dl_iterate_phdr.
  AC_CHECK_HEADERS(link.h)
  if test "$ac_cv_header_link_h" = "no"; then
    have_dl_iterate_phdr=no
  else
    # When built as a GCC target library, we can't do a link test.
    AC_EGREP_HEADER([dl_iterate_phdr], [link.h], [have_dl_iterate_phdr=yes],
		    [have_dl_iterate_phdr=no])
  fi
  if test "$have_dl_iterate_phdr" = "yes"; then
    BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DHAVE_DL_ITERATE_PHDR=1"
  fi

  # Check for the fcntl function.
  if test -n "${with_target_subdir}"; then
     case "${host}" in
     *-*-mingw*) have_fcntl=no ;;
     *) have_fcntl=yes ;;
     esac
  else
    AC_CHECK_FUNC(fcntl, [have_fcntl=yes], [have_fcntl=no])
  fi
  if test "$have_fcntl" = "yes"; then
    BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DHAVE_FCNTL=1"
  fi

  AC_CHECK_DECLS(strnlen)

  # Check for getexecname function.
  if test -n "${with_target_subdir}"; then
     case "${host}" in
     *-*-solaris2*) have_getexecname=yes ;;
     *) have_getexecname=no ;;
     esac
  else
    AC_CHECK_FUNC(getexecname, [have_getexecname=yes], [have_getexecname=no])
  fi
  if test "$have_getexecname" = "yes"; then
    BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DHAVE_GETEXECNAME=1"
  fi

# The library needs to be able to read the executable itself.  Compile
# a file to determine the executable format.  The awk script
# filetype.awk prints out the file type.
AC_CACHE_CHECK([output filetype],
[glibcxx_cv_sys_filetype],
[filetype=
AC_COMPILE_IFELSE(
  [AC_LANG_PROGRAM([int i;], [int j;])],
  [filetype=`${AWK} -f $srcdir/../libbacktrace/filetype.awk conftest.$ac_objext`],
  [AC_MSG_FAILURE([compiler failed])])
glibcxx_cv_sys_filetype=$filetype])

# Match the file type to decide what files to compile.
FORMAT_FILE=
case "$glibcxx_cv_sys_filetype" in
elf*) FORMAT_FILE="elf.lo" ;;
*) AC_MSG_WARN([could not determine output file type])
   FORMAT_FILE="unknown.lo"
   enable_libstdcxx_backtrace=no
   ;;
esac
AC_SUBST(FORMAT_FILE)

# ELF defines.
elfsize=
case "$glibcxx_cv_sys_filetype" in
elf32) elfsize=32 ;;
elf64) elfsize=64 ;;
esac
BACKTRACE_CPPFLAGS="$BACKTRACE_CPPFLAGS -DBACKTRACE_ELF_SIZE=$elfsize"

  AC_MSG_CHECKING([whether to build libbacktrace support])
  if test "$enable_libstdcxx_backtrace" = "auto"; then
    enable_libstdcxx_backtrace=no
  fi
  if test "$enable_libstdcxx_backtrace" = "yes"; then
    BACKTRACE_SUPPORTED=1

    AC_CHECK_HEADERS(sys/mman.h)
    case "${host}" in
      *-*-msdosdjgpp) # DJGPP has sys/man.h, but no mmap
	have_mmap=no ;;
      *-*-*)
	have_mmap="$ac_cv_header_sys_mman_h" ;;
    esac

    if test "$have_mmap" = "no"; then
      VIEW_FILE=read.lo
      ALLOC_FILE=alloc.lo
    else
      VIEW_FILE=mmapio.lo
      AC_PREPROC_IFELSE([AC_LANG_SOURCE([
    #include <sys/mman.h>
    #if !defined(MAP_ANONYMOUS) && !defined(MAP_ANON)
      #error no MAP_ANONYMOUS
    #endif
    ])], [ALLOC_FILE=mmap.lo], [ALLOC_FILE=alloc.lo])
    fi
    AC_SUBST(VIEW_FILE)
    AC_SUBST(ALLOC_FILE)

    BACKTRACE_USES_MALLOC=0
    if test "$ALLOC_FILE" = "alloc.lo"; then
      BACKTRACE_USES_MALLOC=1
    fi

    if test "$ac_has_gthreads" = "yes"; then
      BACKTRACE_SUPPORTS_THREADS=1
    else
      BACKTRACE_SUPPORTS_THREADS=0
    fi
    AC_SUBST(BACKTRACE_CPPFLAGS)
    AC_SUBST(BACKTRACE_SUPPORTED)
    AC_SUBST(BACKTRACE_USES_MALLOC)
    AC_SUBST(BACKTRACE_SUPPORTS_THREADS)
    AC_DEFINE(HAVE_STACKTRACE, 1, [Define if the <stacktrace> header is supported.])
  else
    BACKTRACE_SUPPORTED=0
    BACKTRACE_USES_MALLOC=0
    BACKTRACE_SUPPORTS_THREADS=0
  fi
  AC_MSG_RESULT($enable_libstdcxx_backtrace)
  GLIBCXX_CONDITIONAL(ENABLE_BACKTRACE, [test "$enable_libstdcxx_backtrace" = yes])
])

# Macros from the top-level gcc directory.
m4_include([../config/gc++filt.m4])
m4_include([../config/tls.m4])
m4_include([../config/gthr.m4])
m4_include([../config/cet.m4])
