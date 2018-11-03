#    Copyright (c) 2014 Intel Corporation.  All Rights Reserved.
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions
#    are met:
#
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in the
#        documentation and/or other materials provided with the distribution.
#      * Neither the name of Intel Corporation nor the names of its
#        contributors may be used to endorse or promote products derived
#        from this software without specific prior written permission.
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Process this file with autoconf to produce a configure script, like so:
# aclocal -I .. -I ../config && autoconf && automake

AC_INIT([MIC Offload Runtime Library], [1.0], ,[liboffloadmic])

AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_INIT_AUTOMAKE(foreign no-dist)
AM_MAINTAINER_MODE

AC_PROG_CC
AC_PROG_CXX
AC_CONFIG_FILES([Makefile liboffloadmic_host.spec liboffloadmic_target.spec])
AM_ENABLE_MULTILIB(, ..)
AC_CONFIG_SUBDIRS(plugin)
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([mm_malloc.h], [], [AC_MSG_ERROR(["Couldn't find mm_malloc.h"])])
AC_CHECK_FUNCS([__secure_getenv secure_getenv])

# Get target configure.
. ${srcdir}/configure.tgt
if test -n "$UNSUPPORTED"; then
  AC_MSG_ERROR([Configuration ${target} is unsupported])
fi

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE([version-specific-runtime-libs],
  AC_HELP_STRING([--enable-version-specific-runtime-libs],
		 [Specify that runtime libraries should be installed in a compiler-specific directory]),
  [case "$enableval" in
    yes) enable_version_specific_runtime_libs=yes ;;
    no)  enable_version_specific_runtime_libs=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
   esac],
  [enable_version_specific_runtime_libs=no])
AC_MSG_RESULT($enable_version_specific_runtime_libs)

# Make sure liboffloadmic is enabled
case "$enable_liboffloadmic" in
  host | target)
    ;;
  *)
    AC_MSG_ERROR([Liboffloadmic is disabled]) ;;
esac
AM_CONDITIONAL(LIBOFFLOADMIC_HOST, [test x"$enable_liboffloadmic" = xhost])

# Calculate toolexeclibdir.
# Also toolexecdir, though it's only used in toolexeclibdir.
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

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
# Forbid libtool to hardcode RPATH, because we want to be able to specify
# library search directory using LD_LIBRARY_PATH
hardcode_into_libs=no
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

AC_SUBST(lt_cv_dlopen_libs)

if test $enable_shared = yes; then
  link_offloadmic_host="-loffloadmic_host %{static: $LIBS}"
  link_offloadmic_target="-loffloadmic_target %{static: $LIBS}"
else
  link_offloadmic_host="-loffloadmic_host $LIBS"
  link_offloadmic_target="-loffloadmic_target $LIBS"
fi
AC_SUBST(link_offloadmic_host)
AC_SUBST(link_offloadmic_target)

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

# Must be last
AC_OUTPUT
