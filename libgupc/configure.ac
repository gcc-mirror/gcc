# Process this file with autoconf to produce a configure script, like so:
# aclocal -I .. -I ../config && autoconf && autoheader && automake
#
#   Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
#   2010, 2011
#   Free Software Foundation, Inc. 
#   Contributed by Gary Funck <gary@intrepid.com>
#   Derived from libgomp/configure.ac
#
#This file is part of GNU UPC.
#
#GNU UPC is free software; you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation; either version 2, or (at your option)
#any later version.
#
#GNU UPC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with GNU UPC; see the file COPYING.  If not, write to
#the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#02111-1307, USA.

AC_PREREQ(2.64)
AC_INIT([GNU UPC Runtime Library], 1.0,,[libgupc])
AC_CONFIG_SRCDIR([include/upc.h])
AC_CONFIG_HEADERS([config.h])

# -------
# Options
# -------

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
LIBGUPC_ENABLE(version-specific-runtime-libs, no, ,
   [Specify that runtime libraries should be installed in a compiler-specific directory],
   permit yes|no)
AC_MSG_RESULT($enable_version_specific_runtime_libs)

# We would like our source tree to be readonly. However when releases or
# pre-releases are generated, the flex/bison generated files as well as the
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do 
# just that.
AC_MSG_CHECKING([for --enable-generated-files-in-srcdir])
LIBGUPC_ENABLE(generated-files-in-srcdir, no, ,
   [put copies of generated files in source dir intended for creating source 
    tarballs for users without texinfo bison or flex.],
   permit yes|no)
AC_MSG_RESULT($enable_generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test "$enable_generated_files_in_srcdir" = yes)


# -------
# Get build environment
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
#  1.11.1:  minimum required version
#  no-define:  PACKAGE and VERSION will not be #define'd in config.h (a bunch
#              of other PACKAGE_* variables will, however, and there's nothing
#              we can do about that; they come from AC_INIT).
#  foreign:  we don't follow the normal rules for GNU packages (no COPYING
#            file in the top srcdir, etc, etc), so stop complaining.
#  -Wall:  turns on all automake warnings...
#  -Wno-portability:  ...except this one, since GNU make is required.
#  -Wno-override: ... and this one, since we do want this in testsuite.
AM_INIT_AUTOMAKE([1.11.1 foreign -Wall -Wno-portability -Wno-override])
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
  *)
    AC_MSG_ERROR(['$enable_version_specific_runtime_libs' is not set.])
    ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

# If the language specific compiler does not exist, but the "gcc" directory 
# does, we do not build anything. Note, $r is set by the top-level Makefile.
# Note that when we look for the compiler, we search both with and without
# extension to handle cross and canadian cross builds. 
compiler_name=cc1upc
rm -f skip-this-dir
AC_MSG_CHECKING(if compiler $compiler_name has been built)
AC_CACHE_VAL(upc_cv_compiler_exists,
[
  upc_cv_compiler_exists=yes
  if test -n "$r"; then
    if test -d "$r"/gcc; then
      if test -f "$r"/gcc/$compiler_name \
	   -o -f "$r"/gcc/$compiler_name.exe; then
	true
      else
	upc_cv_compiler_exists=no
      fi
    fi
  fi
])
AC_MSG_RESULT($upc_cv_compiler_exists)
AC_MSG_CHECKING([if UPC driver, 'xgupc' and 'gupc',  has been built])
AC_CACHE_VAL(upc_cv_driver_exists,
[ 
  upc_cv_driver_exists=yes
  if test -n "$r"; then
    if test -d "$r"/gcc; then
      if (test -f "$r"/gcc/xgupc && test -f "$r"/gcc/gupc) \
          || (test -f "$r"/gcc/xgupc.exe && "$r"/gcc/gupc.exe); then
	true
      else
	upc_cv_driver_exists=no
      fi
    fi
  fi
])
AC_MSG_RESULT($upc_cv_driver_exists)
if test x$upc_cv_compiler_exists = xno \
     -o x$upc_cv_driver_exists = xno
then
  echo "rm -f config.cache config.log multilib.out" > skip-this-dir
  rm -f Makefile conftest* confdefs* core
  exit 0
fi

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

#
# If UPCFLAGS hasn't been set explicitly then default to "$CFLAGS".
#
if test x"$UPCFLAGS" = "x"; then
  UPCFLAGS="$CFLAGS"
fi

AC_SUBST(UPCFLAGS)

# In order to override CFLAGS_FOR_TARGET, all of our special flags go
# in XCFLAGS.  But we need them in CFLAGS during configury.  So put them
# in both places for now and restore CFLAGS at the end of config.
save_CFLAGS="$CFLAGS"

# Add -Wall -Werror if we are using GCC.
if test "x$GCC" = "xyes"; then
  XCFLAGS="$XCFLAGS -Wall -Werror"
fi

dnl The UPC compiler is used to compile some parts of the runtime
dnl This hack makes sure that we pick up the multiflags that
dnl may have been tacked onto the $CC command.
if echo "$CC" | grep '/xgcc ' >/dev/null 2>&1; then
  UPC=`echo "$CC" | sed -e 's=/xgcc =/xgupc ='`
else
  AC_MSG_ERROR([Can't create UPC from CC because it doesn't contain the
  string '/xgcc '.  The value of CC is: "$CC"])
fi
AM_PROG_UPC()

# Find other programs we need.
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, ranlib-not-found-in-path-error)
AC_PATH_PROG(PERL, perl, perl-not-found-in-path-error)
AC_PATH_PROG(ADDR2LINE, addr2line, addr2line-not-found-in-path-error)
AC_PROG_MAKE_SET
AC_PROG_INSTALL

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER([MAKEINFO], [makeinfo], [--version],
                   [GNU texinfo.* \([0-9][0-9.]*\)],
                   [4.[4-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
AM_CONDITIONAL(BUILD_INFO, test $gcc_cv_prog_makeinfo_modern = "yes")


# libgupc cannot be built as shared library
enable_shared=no
# Configure libtool
AC_DISABLE_SHARED
AM_PROG_LIBTOOL

AM_MAINTAINER_MODE

# For libtool versioning info, format is CURRENT:REVISION:AGE
libtool_VERSION=1:0:0
AC_SUBST(libtool_VERSION)

# Check header files.
AC_STDC_HEADERS
ACX_HEADER_STRING
GCC_HEADER_STDINT(gstdint.h)
AC_HEADER_TIME
AC_HEADER_SYS_WAIT

AC_CHECK_HEADERS([execinfo.h fcntl.h limits.h netdb.h netinet/in.h])
AC_CHECK_HEADERS([sched.h semaphore.h stddef.h stdlib.h sys/loadavg.h])
AC_CHECK_HEADERS([sys/socket.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_MODE_T
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_DECLS([sys_siglist])
AC_C_VOLATILE
AC_CHECK_TYPES([ptrdiff_t])

# Check to see if -pthread or -lpthread is needed.  Prefer the former.
# In case the pthread.h system header is not found, this test will fail.
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
   [AC_MSG_NOTICE([Pthreads are not supported in this libgupc build])])])

# Check for functions needed.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_STAT
AC_CHECK_FUNCS([clock_gettime ftruncate getcwd gethostbyname gethostname])
AC_CHECK_FUNCS([getloadavg memset mkdir munmap socket])
AC_CHECK_FUNCS([strcasecmp strdup strerror strtol strtoull])
AC_CHECK_FUNCS([backtrace backtrace_symbols backtrace_symbols_fd])
AC_SYS_LARGEFILE

# Check for broken semaphore implementation on darwin.
# sem_init returns: sem_init error: Function not implemented.
case "$host" in
  *-darwin*)
    AC_DEFINE(HAVE_BROKEN_POSIX_SEMAPHORES, 1,
	Define if the POSIX Semaphores do not work on your system.)
    ;;
esac

GCC_LINUX_FUTEX(:)

# Check for pthread_{,attr_}[sg]etaffinity_np.
AC_LINK_IFELSE(
 [AC_LANG_PROGRAM(
  [#define _GNU_SOURCE
   #include <pthread.h>],
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
    [Define  to 1 if pthread_{,attr_}{g,s}etaffinity_np is supported.]))

# At least for glibc, clock_gettime is in librt.  But don't pull that
# in if it still doesn't give us the function we want.
if test $ac_cv_func_clock_gettime = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [LIBS="-lrt $LIBS"
     AC_DEFINE(HAVE_CLOCK_GETTIME, 1,
	       [Define to 1 if you have the `clock_gettime' function.])])
fi

# See if we support thread-local storage.
GCC_CHECK_TLS

# See what sort of export controls are availible.
LIBGUPC_CHECK_ATTRIBUTE_VISIBILITY
LIBGUPC_CHECK_ATTRIBUTE_DLLEXPORT
LIBGUPC_CHECK_ATTRIBUTE_ALIAS
LIBGUPC_ENABLE_SYMVERS

if test $enable_symvers = gnu; then
  AC_DEFINE(LIBGUPC_GNU_SYMBOL_VERSIONING, 1,
	    [Define to 1 if GNU symbol versioning is used for libgupc.])
fi

CFLAGS="$save_CFLAGS $XCFLAGS"

# Check for __sync_val_compare_and_swap, but only after the target has
# had a chance to set XCFLAGS.
LIBGUPC_CHECK_SYNC_BUILTINS

XCFLAGS="$XCFLAGS"


# Cleanup and exit.
CFLAGS="$save_CFLAGS"
AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_MSG_CHECKING([for thread model used by GCC])
target_thread_file=`$CC -v 2>&1 | sed -n 's/^Thread model: //p'`
AC_MSG_RESULT([$target_thread_file]) 

if test $target_thread_file != single; then
  AC_DEFINE(HAVE_GTHR_DEFAULT, 1,
        [Define if the compiler has a thread header that is non single.])
fi 

AC_MSG_CHECKING([UPC shared pointer representation])
cat > conftest.upc << _ACEOF
#define _XSTR(X) #X
#define _STR(S) _XSTR(S)
#ifdef __UPC_VADDR_FIRST__
  GUPCR_PTS_VADDR_FIRST=1
#else
  GUPCR_PTS_VADDR_FIRST=0
#endif
GUPCR_PTS_VADDR_SIZE=_STR(__UPC_VADDR_SIZE__)
GUPCR_PTS_PHASE_SIZE=_STR(__UPC_PHASE_SIZE__)
GUPCR_PTS_THREAD_SIZE=_STR(__UPC_THREAD_SIZE__)
#if __UPC_PTS_STRUCT_REP__
  GUPCR_PTS_REP="struct"
  GUPCR_PTS_STRUCT_REP=1
  GUPCR_PTS_VADDR_TYPE=_STR(__UPC_VADDR_TYPE__)
  GUPCR_PTS_PHASE_TYPE=_STR(__UPC_PHASE_TYPE__)
  GUPCR_PTS_THREAD_TYPE=_STR(__UPC_THREAD_TYPE__)
  #if __UPC_PTS_ALIGN__
    GUPCR_PTS_ALIGN=_STR(__UPC_PTS_ALIGN__)
  #endif
#else
  GUPCR_PTS_REP="packed"
  GUPCR_PTS_PACKED_REP=1
#endif
_ACEOF
eval `$UPC -DNO_GCC_UPC_LIB=1 -E conftest.upc | grep '^ *GUPCR_[[A-Z0-9_]]*='`
rm -f conftest.upc
AC_MSG_RESULT($GUPCR_PTS_REP)
if test -z "$GUPCR_PTS_REP"; then
  AC_MSG_ERROR([can't determine UPC sptr representation])
fi
for cfg in \
     GUPCR_PTS_PACKED_REP \
     GUPCR_PTS_PHASE_SIZE \
     GUPCR_PTS_PHASE_TYPE \
     GUPCR_PTS_REP \
     GUPCR_PTS_STRUCT_REP \
     GUPCR_PTS_ALIGN \
     GUPCR_PTS_THREAD_SIZE \
     GUPCR_PTS_THREAD_TYPE \
     GUPCR_PTS_VADDR_FIRST \
     GUPCR_PTS_VADDR_SIZE \
     GUPCR_PTS_VADDR_TYPE ; do
  eval "cval=\$$cfg"
  if test -n "$cval"; then
    AC_MSG_CHECKING([value of $cfg])
    AC_MSG_RESULT($cval)
  fi
done
if test "$with_upc_pts" -a "$with_upc_pts" != "$GUPCR_PTS_REP" ; then
  AC_MSG_ERROR([--with-upc-pts=$with_upc_pts is incompatible with the
UPC compiler's supported pointer-to-shared representation ($GUPCR_PTS_REP)])
fi
AC_DEFINE_UNQUOTED(GUPCR_PTS_VADDR_SIZE,[$GUPCR_PTS_VADDR_SIZE],
  [Size of shared pointer's vaddr field (in bits)])
AC_DEFINE_UNQUOTED(GUPCR_PTS_THREAD_SIZE,[$GUPCR_PTS_THREAD_SIZE],
  [Size of shared pointer's thread field (in bits)])
AC_DEFINE_UNQUOTED(GUPCR_PTS_PHASE_SIZE,[$GUPCR_PTS_PHASE_SIZE],
  [Size of shared pointer's phase field (in bits)])
if test "$GUPCR_PTS_REP" = "struct"; then
  AC_DEFINE(GUPCR_PTS_STRUCT_REP,[1],
    [Whether UPC shared pointers use the 'struct' representation])
  AC_DEFINE_UNQUOTED(GUPCR_PTS_VADDR_TYPE,[$GUPCR_PTS_VADDR_TYPE],
    [The data type of the 'vaddr' field in a UPC shared pointer])
  AC_DEFINE_UNQUOTED(GUPCR_PTS_THREAD_TYPE,[$GUPCR_PTS_THREAD_TYPE],
    [The data type of the 'thread' field in a UPC shared pointer])
  AC_DEFINE_UNQUOTED(GUPCR_PTS_PHASE_TYPE,[$GUPCR_PTS_PHASE_TYPE],
    [The data type of the 'phase' field in a UPC shared pointer])
  if test -n "$GUPCR_PTS_ALIGN"; then
    AC_DEFINE_UNQUOTED(GUPCR_PTS_ALIGN,[$GUPCR_PTS_ALIGN],
      [The required alignment for the UPC struct shared pointer representation])
  fi
else
  AC_DEFINE(GUPCR_PTS_PACKED_REP,[1],
    [Whether UPC pointers-to-shared use the 'packed' representation])
fi
if test "$GUPCR_PTS_VADDR_FIRST" -eq 1; then
  AC_DEFINE(GUPCR_PTS_VADDR_FIRST,[1],
    [Whether the 'vaddr' field comes first (ie, [[vaddr,thread,phase]])])
fi

dnl Select target specific runtime implementation.

AC_MSG_CHECKING([for UPC runtime model])
AC_ARG_WITH(upc-runtime,
[AS_HELP_STRING([--with-upc-runtime=MODEL],
		[specify the runtime implementation model for UPC,
		 where MODEL may only be: 'SMP' (Symmetric Multiprocessing)
		 in the current implementation
		 [default='SMP']])],
[
  case "$withval" in
changequote(,)dnl
    [Ss][Mm][Pp])
      upc_runtime_model=smp
      ;;
changequote([,])dnl
    *)
      AC_MSG_ERROR([$withval is an invalid runtime model for --with-upc-runtime])
      ;;
  esac
],
[
  upc_runtime_model=smp
])
AC_MSG_RESULT([$upc_runtime_model])

AC_MSG_CHECKING([for UPC link script support])
AC_ARG_ENABLE(upc-link-script,
[AS_HELP_STRING([--enable-upc-link-script],
                [enable UPC's use of a custom linker script;
                 this will define the UPC shared section as a
		 no load section on targets where this feature
		 is supported (requires GNU LD).])],
[
  case $enableval in
  yes | no) ;;
  *)
    AC_MSG_ERROR([--enable-upc-link-script accepts only yes or no.])
  esac
],
[
  enable_upc_link_script=no
])
if test "$enable_upc_link_script" = yes; then
  if test "$with_gnu_ld" = yes; then
    gen_ld_script=${srcdir}/${targ_runtime}/gen-upc-ld-script.pl
    if ! ${LD} --verbose | ${PERL} $gen_ld_script 1>/dev/null ; then
      # if gen-upc-ld-script did not accept the output of the linker
      # then disable UPC link script support.
      AC_MSG_ERROR([Selected linker does not support linker scripts.])
    fi
  fi
fi
AC_MSG_RESULT($enable_upc_link_script)
if test "$enable_upc_link_script" = yes; then
  AC_DEFINE(HAVE_UPC_LINK_SCRIPT, 1,
    [Define to 1 if UPC link script is supported.])
fi

if test "$upc_runtime_model" = "smp"; then
  targ_runtime_pthreads=
  LIBGUPC_GCC_TLS_SUPPORTED
  if test "$upc_cv_gcc_tls_supported"x = "yes"x ; then
    targ_runtime_pthreads=yes
  fi
  AC_MSG_CHECKING(for Linux scheduling affinity)
  AC_TRY_RUN(
  changequote(<<,>>)dnl
  <<#define _GNU_SOURCE
  #include <sched.h>
  cpu_set_t mask;
  main ()
  {
  unsigned int len = sizeof(mask);
  if (sched_getaffinity(0, len, &mask) < 0) {
      return 1;
      }
  return 0;
  }
  >>,
  changequote([, ])dnl
      [upc_affinity=yes],
      [upc_affinity=no],
      [upc_affinity=no]
  )
  if test "x$enable_upc_affinity" = "xyes" &&
    test "x$upc_affinity" = "xno"; then
      AC_MSG_ERROR([scheduling affinity enabled but not supported - use --disable-upc-affinity])
  fi
  if test "x$enable_upc_affinity" = "xno"; then
    upc_affinity=no;
  fi
  AC_MSG_RESULT([$upc_affinity])
  dnl Check if NUMA available
  AC_MSG_CHECKING(if NUMA available)
  save_LIBS="$LIBS"
  LIBS="-lnuma"
  AC_TRY_RUN(
  changequote(<<,>>)dnl
  <<#include <numa.h>
  int
  main ()
  {
    if (numa_available() < 0) return 1;
    else return 0;
  }
  >>,
  changequote([, ])dnl
      [upc_numa=yes],
      [upc_numa=no],
      [upc_numa=no],
  )
  LIBS="$save_LIBS"
  if test "x$enable_upc_numa" = "xyes" &&
    test "x$upc_numa" = "xno"; then
      AC_MSG_ERROR([NUMA enabled but not supported - use --disable-upc-numa])
  fi
  if test "x$enable_upc_numa" = "xno" \
	  -o "x$enable_upc_affinity" = "xno"; then
    upc_numa=no;
  fi
  AC_MSG_RESULT([$upc_numa])
  AC_MSG_CHECKING([for UPC GUM debugging support])
  AC_ARG_ENABLE(upc-gum-debug,
  [AS_HELP_STRING([--enable-upc-gum-debug],
                  [enable UPC's source level debugging with GDB and GUM
                   support (requires UPC aware GDB).])],
  [
    case $enableval in
    yes | no) ;;
    *)
      AC_MSG_ERROR([--enable-upc-gum-debug accepts only yes or no.])
    esac
    upc_gum_debug=$enableval
  ],
  [
    upc_gum_debug=no
  ])
  AC_MSG_RESULT([$upc_gum_debug])
  if test "x${upc_gum_debug}" = "xyes"; then
    AC_DEFINE_UNQUOTED(GUPCR_HAVE_GUM_DEBUG,[1],
    [Define if UPC GUM debug server is supported])
  fi
  # Use the generic (reference implementation)
  use_generic_collectives="yes"

fi #### End: SMP runtime configuration

# Get target configury.
. ${srcdir}/configure.tgt

# configure.tgt sets these substitution variables.
# It also sets 'upc_crtstuff=yes' if upc-crtstuff needs
# to be compiled.
AC_SUBST(config_path)
AC_SUBST(XCFLAGS)
AC_SUBST(XLDFLAGS)
AC_SUBST(link_upc_spec)
AC_SUBST(upc_crtstuff_cflags)
AC_SUBST(upc_crtbegin_spec)
AC_SUBST(upc_crtend_spec)
AC_SUBST(upc_crtstuff_mak)
AC_SUBST(upc_crtstuff_objs)

dnl Define ARG_UNUSED macro
AH_BOTTOM([
#ifndef ARG_UNUSED
# define ARG_UNUSED(NAME) NAME __attribute__ ((__unused__))
#endif
])

dnl Build the runtime, based upon the --with-upc-runtime setting.
AM_CONDITIONAL(LIBGUPC_SMP_RUNTIME, [test "$upc_runtime_model" = "smp"])

AM_CONDITIONAL(LIBGUPC_CRTSTUFF, [test "$upc_crtstuff" = "yes"])
AM_CONDITIONAL(LIBGUPC_LINK_SCRIPT, [test "$enable_upc_link_script" = "yes"])
AM_CONDITIONAL(LIBGUPC_GENERIC_COLLECTIVES,
               [test "$use_generic_collectives" = "yes"])

dnl 'smp' runtime specific settings
AM_CONDITIONAL(LIBGUPC_PTHREADS, [test "$targ_runtime_pthreads" = "yes"])
AM_CONDITIONAL(LIBGUPC_AFFINITY, [test "$upc_affinity" = "yes"])
AM_CONDITIONAL(LIBGUPC_NUMA, [test "$upc_numa" = "yes"])
AM_CONDITIONAL(LIBGUPC_GUM, [test "$upc_gum_debug" = "yes"])

AH_TOP(
[/* Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2, or (at
   your option) any later version.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this library; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.

   As a special exception, if you link this library with files
   compiled with a GNU compiler to produce an executable, this does
   not cause the resulting executable to be covered by the GNU General
   Public License.  This exception does not however invalidate any
   other reasons why the executable file might be covered by the GNU
   General Public License.  */
#ifndef __CONFIG_H__
#define __CONFIG_H__ 1
])
AH_VERBATIM(GUPCR_AAA_BEGIN_CFG,[//begin gupcr_config_h])
AH_VERBATIM(GUPCR_ZZZ_END_CFG,[//end gupcr_config_h])
AH_BOTTOM(
[
#endif /* __CONFIG_H__ */
])

AC_CONFIG_FILES([Makefile testsuite/Makefile])
AC_CONFIG_FILES([upc-crtbegin.spec libgupc.spec upc-crtend.spec])
AC_OUTPUT
