# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.64])
AC_INIT(package-unused, version-unused, libmpx)

# -------
# Options
# -------
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

# See if supported.
unset LIBMPX_SUPPORTED
AC_MSG_CHECKING([for target support for Intel MPX runtime library])
echo "int i[[sizeof (void *) == 4 ? 1 : -1]] = { __x86_64__ };" > conftest.c
if AC_TRY_COMMAND([${CC} ${CFLAGS} -c -o conftest.o conftest.c 1>&AS_MESSAGE_LOG_FD])
then
    LIBMPX_SUPPORTED=no
else
    LIBMPX_SUPPORTED=yes
fi
rm -f conftest.o conftest.c
AC_MSG_RESULT($LIBMPX_SUPPORTED)
AM_CONDITIONAL(LIBMPX_SUPPORTED, [test "x$LIBMPX_SUPPORTED" = "xyes"])

link_libmpx="-lpthread"
AC_SUBST(link_libmpx)

AM_INIT_AUTOMAKE(foreign no-dist no-dependencies)
AM_ENABLE_MULTILIB(, ..)
AM_MAINTAINER_MODE

AC_GNU_SOURCE
AC_CHECK_FUNCS([secure_getenv])

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

# Check for programs.
m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AM_PROG_CC_C_O

AC_SUBST(CFLAGS)

# Newer automakes demand CCAS and CCASFLAGS.
: ${CCAS='$(CC)'}
: ${CCASFLAGS='$(CFLAGS)'}
AC_SUBST(CCAS)
AC_SUBST(CCASFLAGS)

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

# Check we may build wrappers library
echo "test:  bndmov %bnd0, %bnd1" > conftest.s
if AC_TRY_COMMAND([$AS -o conftest.o conftest.s 1>&AS_MESSAGE_LOG_FD])
then
    mpx_as=yes
else
    mpx_as=no
    echo "configure: no MPX support fo as" >&AS_MESSAGE_LOG_FD
fi
rm -f conftest.o conftest.s
AM_CONDITIONAL(MPX_AS_SUPPORTED, [test "x$mpx_as" = "xyes"])

# Configure libtool
AC_LIBTOOL_DLOPEN
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

XCFLAGS="-Wall -Wextra"
AC_SUBST(XCFLAGS)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_CONFIG_FILES([Makefile libmpx.spec])
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_FILES(AC_FOREACH([DIR], [mpxrt mpxwrap], [DIR/Makefile ]),
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

AC_OUTPUT
