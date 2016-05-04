#  Copyright (C) 2011-2016, Intel Corporation
#  All rights reserved.
#  
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#    * Neither the name of Intel Corporation nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#  
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
#  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
#  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
#  WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
#  POSSIBILITY OF SUCH DAMAGE.
#  
#  *********************************************************************
#  
#  PLEASE NOTE: This file is a downstream copy of a file mainitained in
#  a repository at cilkplus.org. Changes made to this file that are not
#  submitted through the contribution process detailed at
#  http://www.cilkplus.org/submit-cilk-contribution will be lost the next
#  time that a new version is released. Changes only submitted to the
#  GNU compiler collection or posted to the git repository at
#  https://bitbucket.org/intelcilkruntime/intel-cilk-runtime.git are
#  not tracked.
#  
#  We welcome your contributions to this open source project. Thank you
#  for your assistance in helping us improve Cilk Plus.

AC_INIT([Cilk Runtime Library], [2.0], [cilk@intel.com])
AC_PREREQ([2.64])

# Needed to define ${target}.  Needs to be very early to avoid annoying
# warning about calling AC_ARG_PROGRAM before AC_CANONICAL_SYSTEM
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

# Test for GNU extensions. Will define _GNU_SOURCE if they're available
AC_USE_SYSTEM_EXTENSIONS

AM_INIT_AUTOMAKE(foreign no-dist)

AM_MAINTAINER_MODE

AM_ENABLE_MULTILIB(, ..)

# Build a DLL on Windows
# AC_LIBTOOL_WIN32_DLL
AC_PROG_CC
AC_PROG_CXX
# AC_PROG_LIBTOOL
# AC_CONFIG_MACRO_DIR([..])
AC_CONFIG_FILES([Makefile libcilkrts.spec])
AC_FUNC_ALLOCA

# Check whether the target supports protected visibility.
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -Werror"
AC_TRY_COMPILE([void __attribute__((visibility("protected"))) foo(void) { }],
	       [], libcilkrts_cv_have_attribute_visibility=yes,
	       libcilkrts_cv_have_attribute_visibility=no)
CFLAGS="$save_CFLAGS"
if test $libcilkrts_cv_have_attribute_visibility = yes; then
  AC_DEFINE(HAVE_ATTRIBUTE_VISIBILITY, 1,
    [Define to 1 if the target supports __attribute__((visibility(...))).])
fi

# Get target configury.
. ${srcdir}/configure.tgt
if test -n "$UNSUPPORTED"; then
   AC_MSG_ERROR([Configuration ${target} is unsupported.])
fi

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE([version-specific-runtime-libs],
  AC_HELP_STRING([--enable-version-specific-runtime-libs],
                 [Specify that runtime libraries should be installed in a compi
ler-specific directory]),
  [case "$enableval" in
    yes) enable_version_specific_runtime_libs=yes ;;
    no)  enable_version_specific_runtime_libs=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs
]);;
   esac],
  [enable_version_specific_runtime_libs=no])
AC_MSG_RESULT($enable_version_specific_runtime_libs)


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

# Set config_dir based on the target.  config_dir specifies where to get
# target-specific files.  The generic implementation is incomplete, but
# contains information on what's needed
case "${target}" in

  x86_64-*-*)
    config_dir="x86"
    ;;

  i?86-*-*)
    config_dir="x86"
    ;;

  arm-*-*)
    config_dir="arm"
    ;;

  *)
    config_dir="generic"
    ;;

esac
AC_SUBST(config_dir)

# We have linker scripts for appropriate operating systems
linux_linker_script=no
case "${host}" in
    *-*-linux*)
        linux_linker_script=yes
        ;;
esac
AM_CONDITIONAL(LINUX_LINKER_SCRIPT, test "$linux_linker_script" = "yes")

mac_linker_script=no
case "${host}" in
    *-*-apple*)
        mac_linker_script=yes
        ;;
esac
AM_CONDITIONAL(MAC_LINKER_SCRIPT, test "$mac_linker_script" = "yes")

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

AC_SUBST(lt_cv_dlopen_libs)

# Check to see if -pthread or -lpthread is needed.  Prefer the former.
# Note that the CILK_SELF_SPEC in gcc.c may force -pthread.
# In case the pthread.h system header is not found, this test will fail.
XCFLAGS=""
XLDFLAGS=""
CFLAGS="$CFLAGS -pthread"
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM(
  [#include <pthread.h>
   void *g(void *d) { return NULL; }],
  [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
 [XCFLAGS=" -Wc,-pthread"],
 [CFLAGS="$save_CFLAGS" LIBS="-lpthread $LIBS"
  AC_LINK_IFELSE(
   [AC_LANG_PROGRAM(
    [#include <pthread.h>
     void *g(void *d) { return NULL; }],
    [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
   [],
   [AC_MSG_ERROR([Pthreads are required to build libcilkrts])])])

# Check for pthread_{,attr_}[sg]etaffinity_np.
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM(
  [#include <pthread.h>],
  [cpu_set_t cpuset;
   pthread_attr_t attr;
   pthread_getaffinity_np (pthread_self (), sizeof (cpu_set_t), &cpuset);
   if (CPU_ISSET (0, &cpuset))
     CPU_SET (1, &cpuset);
   else
     CPU_ZERO (&cpuset);
   pthread_setaffinity_np (pthread_self (), sizeof (cpu_set_t), &cpuset);
   pthread_attr_init (&attr);
   pthread_attr_getaffinity_np (&attr, sizeof (cpu_set_t), &cpuset);
   pthread_attr_setaffinity_np (&attr, sizeof (cpu_set_t), &cpuset);])],
  AC_DEFINE(HAVE_PTHREAD_AFFINITY_NP, 1,
[       Define if pthread_{,attr_}{g,s}etaffinity_np is supported.]))

# Every c++ lib is linking by default with -nostdlib, which leads to the
# fact, that proper pthread library will not be given at link time. We have
# to hard-code that.
case "${target}" in

  *android*)
    XLDFLAGS="$XLDFLAGS -lc"
     ;;
  *)
    XLDFLAGS="$XLDFLAGS -lpthread"
    ;;

esac

AC_SUBST(XCFLAGS)
AC_SUBST(XLDFLAGS)

CFLAGS="$save_CFLAGS"

if test $enable_shared = yes; then
  link_cilkrts="-lcilkrts %{static: $LIBS}"
else
  link_cilkrts="-lcilkrts $LIBS"
fi
AC_SUBST(link_cilkrts)


# Must be last
AC_OUTPUT
