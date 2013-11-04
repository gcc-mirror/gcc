#  @copyright
#  Copyright (C) 2011-2013, Intel Corporation
#  All rights reserved.
#  
#  @copyright
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
#  @copyright
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

AC_INIT([Cilk Runtime Library], [2.0], [cilk@intel.com])
AC_PREREQ([2.64])

# Needed to define ${target}.  Needs to be very early to avoid annoying
# warning about calling AC_ARG_PROGRAM before AC_CANONICAL_SYSTEM
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)
GCC_LIBSTDCXX_RAW_CXX_FLAGS
AM_INIT_AUTOMAKE(foreign no-dist)

AM_MAINTAINER_MODE

# Build a DLL on Windows
# AC_LIBTOOL_WIN32_DLL
AC_PROG_CXX
AC_PROG_CC
# AC_PROG_LIBTOOL
# AC_CONFIG_MACRO_DIR([..])
AC_CONFIG_FILES([Makefile])
AM_ENABLE_MULTILIB(, ..)
AC_FUNC_ALLOCA

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

AM_PROG_LIBTOOL
AC_SUBST(toolexecdir)
AC_SUBST(toolexeclibdir)

# Must be last
AC_OUTPUT
