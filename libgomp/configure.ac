# Process this file with autoconf to produce a configure script, like so:
# aclocal -I ../config && autoconf && autoheader && automake

AC_PREREQ(2.59)
AC_INIT([GNU OpenMP Runtime Library], 1.0,,[libgomp])
AC_CONFIG_HEADER(config.h)

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
LIBGOMP_ENABLE(version-specific-runtime-libs, no, ,
   [Specify that runtime libraries should be installed in a compiler-specific directory],
   permit yes|no)
AC_MSG_RESULT($enable_version_specific_runtime_libs)

AC_MSG_CHECKING([for --enable-linux-futex])
LIBGOMP_ENABLE(linux-futex, yes,
  [Use the Linux futex system call],
  permit yes|no)
AC_MSG_RESULT($enable_linux_futex)

# -------
# -------

# Gets build, host, target, *_vendor, *_cpu, *_os, etc.
#
# You will slowly go insane if you do not grok the following fact:  when
# building this library, the top-level /target/ becomes the library's /host/.
#
# configure then causes --target to default to --host, exactly like any
# other package using autoconf.  Therefore, 'target' and 'host' will
# always be the same.  This makes sense both for native and cross compilers
# just think about it for a little while.  :-)
#
# Also, if this library is being configured as part of a cross compiler, the
# top-level configure script will pass the "real" host as $with_cross_host.
#
# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}

# Sets up automake.  Must come after AC_CANONICAL_SYSTEM.  Each of the
# following is magically included in AUTOMAKE_OPTIONS in each Makefile.am.
#  1.9.0:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  no-dependencies:  turns off auto dependency generation (just for now)
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is required.
#  -Wno-override: ... and this one, since we do want this in testsuite.
AM_INIT_AUTOMAKE([1.9.0 foreign -Wall -Wno-portability -Wno-override])
AM_ENABLE_MULTILIB(, ..)

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${enable_version_specific_runtime_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    toolexecdir='$(libdir)/gcc/$(target_alias)'
    toolexeclibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_alias)'
      toolexeclibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_alias)'
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

# Check the compiler.
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

# In order to override CFLAGS_FOR_TARGET, all of our special flags go
# in XCFLAGS.  But we need them in CFLAGS during configury.  So put them
# in both places for now and restore CFLAGS at the end of config.
save_CFLAGS="$CFLAGS"

# Add -Wall -Werror if we are using GCC.
if test "x$GCC" = "xyes"; then
  XCFLAGS="$XCFLAGS -Wall -Werror"
fi

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PATH_PROG(PERL, perl, perl-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# Configure libtool
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

# We need gfortran to compile parts of the library
# We can't use AC_PROG_FC because it expects a fully working gfortran.
#AC_PROG_FC(gfortran)
FC="$GFORTRAN"
AC_PROG_FC(gfortran)
FCFLAGS="$FCFLAGS -Wall"

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

# Check header files.
AC_STDC_HEADERS
AC_HEADER_TIME
AC_CHECK_HEADERS(unistd.h semaphore.h sys/time.h)
AC_CHECK_HEADER([pthread.h],[],
  [AC_MSG_ERROR([Pthreads are required to build libgomp])])

# Check to see if -pthread or -lpthread is needed.  Prefer the former.
XPCFLAGS=""
CFLAGS="$CFLAGS -pthread"
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM(
  [#include <pthread.h>
   void *g(void *d) { return NULL; }],
  [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
 [XPCFLAGS=" -Wc,-pthread"],
 [CFLAGS="$save_CFLAGS" LIBS="-lpthread $LIBS"
  AC_LINK_IFELSE(
   [AC_LANG_PROGRAM(
    [#include <pthread.h>
     void *g(void *d) { return NULL; }],
    [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
   [],
   [AC_MSG_ERROR([Pthreads are required to build libgomp])])])

# Check for functions needed.
AC_CHECK_FUNCS(getloadavg clock_gettime)

# At least for glibc, clock_gettime is in librt.  But don't pull that
# in if it still doesn't give us the function we want.
if test $ac_cv_func_clock_gettime = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [LIBS="-lrt $LIBS"
     AC_DEFINE(HAVE_CLOCK_GETTIME,
	       [Define to 1 if you have the `clock_gettime' function.])])
fi

# See if we support thread-local storage.
LIBGOMP_CHECK_TLS

# See what sort of export controls are availible.
LIBGOMP_CHECK_ATTRIBUTE_VISIBILITY
LIBGOMP_CHECK_ATTRIBUTE_DLLEXPORT
LIBGOMP_CHECK_ATTRIBUTE_ALIAS
LIBGOMP_ENABLE_SYMVERS

# Get target configury.
. ${srcdir}/configure.tgt
CFLAGS="$save_CFLAGS $XCFLAGS"

# Check for __sync_val_compare_and_swap, but only after the target has
# had a chance to set XCFLAGS.
LIBGOMP_CHECK_SYNC_BUILTINS

XCFLAGS="$XCFLAGS$XPCFLAGS"

AC_SUBST(config_path)
AC_SUBST(XCFLAGS)
AC_SUBST(XLDFLAGS)

# Cleanup and exit.
CFLAGS="$save_CFLAGS"
AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Set up the set of libraries that we need to link against for libgomp.
# Note that the GOMP_DRIVER_SPEC in gcc.c will force -pthread for -fopenmp,
# which will force linkage against -lpthread (or equivalent for the system).
# That's not 100% ideal, but about the best we can do easily.
if test $enable_shared = yes; then
  link_gomp="-lgomp %{static: $LIBS}"
else
  link_gomp="-lgomp $LIBS"
fi
AC_SUBST(link_gomp)

AM_CONDITIONAL([USE_FORTRAN], [test "$ac_cv_fc_compiler_gnu" = yes])

AC_CONFIG_FILES(Makefile testsuite/Makefile libgomp.spec)
AC_OUTPUT
