#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT(package-unused, version-unused, libsanitizer)
AC_CONFIG_SRCDIR([include/sanitizer/common_interface_defs.h])

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

# Do not delete or change the following two lines.  For why, see
# http://gcc.gnu.org/ml/libstdc++/2003-07/msg00451.html
AC_CANONICAL_SYSTEM
target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)
GCC_LIBSTDCXX_RAW_CXX_FLAGS

AM_INIT_AUTOMAKE(foreign no-dist)
AM_ENABLE_MULTILIB(, ..)
AM_MAINTAINER_MODE

# Calculate toolexeclibdir
# Also toolexecdir, though it's only used in toolexeclibdir
case ${version_specific_libs} in
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

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AM_PROG_AS

AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL

AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AC_CHECK_SIZEOF([void *])

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Get target configury.
unset TSAN_SUPPORTED
unset LSAN_SUPPORTED
. ${srcdir}/configure.tgt
AM_CONDITIONAL(TSAN_SUPPORTED, [test "x$TSAN_SUPPORTED" = "xyes"])
AM_CONDITIONAL(LSAN_SUPPORTED, [test "x$LSAN_SUPPORTED" = "xyes"])

case "$host" in
  *-*-darwin*) MAC_INTERPOSE=true ; enable_static=no ;;
  *) MAC_INTERPOSE=false ;;
esac
AM_CONDITIONAL(USING_MAC_INTERPOSE, $MAC_INTERPOSE)

AC_CONFIG_FILES([Makefile])

AC_CONFIG_FILES(AC_FOREACH([DIR], [interception sanitizer_common lsan asan ubsan], [DIR/Makefile ]),
  [cat > vpsed$$ << \_EOF
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

if test "x$TSAN_SUPPORTED" = "xyes"; then
  AC_CONFIG_FILES(AC_FOREACH([DIR], [tsan], [DIR/Makefile ]), 
    [cat > vpsed$$ << \_EOF
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
fi

AC_OUTPUT
