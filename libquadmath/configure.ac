# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_PREREQ(2.64)
AC_INIT([GCC Quad-precision Math Library], 0.1,,[libquadmath])
AC_CONFIG_HEADER(config.h)

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

AM_INIT_AUTOMAKE([1.9.0 foreign subdir-objects no-dist -Wall -Werror])

AC_PROG_CC
AM_PROG_CC_C_O

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


# Configure libtool
AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AM_MAINTAINER_MODE
AM_ENABLE_MULTILIB(, ..)

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

AC_CHECK_LIB([m],[sqrtl],[AC_DEFINE([HAVE_SQRTL],[1],[libm includes sqrtl])])
AC_CHECK_LIB([m],[cbrtl],[AC_DEFINE([HAVE_CBRTL],[1],[libm includes cbrtl])])

# Check for symbol versioning (copied from libssp).
AC_MSG_CHECKING([whether symbol versioning is supported])
save_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS -fPIC -shared -Wl,--version-script,./conftest.map"
cat > conftest.map <<EOF
FOO_1.0 {
  global: *foo*; bar; local: *;
};
EOF
AC_TRY_LINK([int foo;],[],[quadmath_use_symver=gnu],[quadmath_use_symver=no])
if test x$quadmath_use_symver = xno; then
  case "$target_os" in
    solaris2*)
      LDFLAGS="$save_LDFLAGS"
      LDFLAGS="$LDFLAGS -fPIC -shared -Wl,-M,./conftest.map"
      # Sun ld cannot handle wildcards and treats all entries as undefined.
      cat > conftest.map <<EOF
FOO_1.0 {
  global: foo; local: *;
};
EOF
      AC_TRY_LINK([int foo;],[],[quadmath_use_symver=sun],[quadmath_use_symver=no])
      ;;
  esac
fi
LDFLAGS="$save_LDFLAGS"
AC_MSG_RESULT($quadmath_use_symver)
AM_CONDITIONAL(LIBQUAD_USE_SYMVER, [test "x$quadmath_use_symver" != xno])
AM_CONDITIONAL(LIBQUAD_USE_SYMVER_GNU, [test "x$quadmath_use_symver" = xgnu])
AM_CONDITIONAL(LIBQUAD_USE_SYMVER_SUN, [test "x$quadmath_use_symver" = xsun])

AC_MSG_CHECKING([whether __float128 is supported])
  AC_TRY_LINK([
    typedef _Complex float __attribute__((mode(TC))) __complex128;

    __float128 foo (__float128 x)
    {

     __complex128 z1, z2;

     z1 = x;
     z2 = x / 7.Q;
     z2 /= z1;

     return (__float128) z2;
    }

    __float128 bar (__float128 x)
    {
      return x * __builtin_huge_valq ();
    }
  ],[
    foo (1.2Q);
    bar (1.2Q);
  ],[
    libquad_have_float128=yes
  ],[
    libquad_have_float128=no
])
AC_MSG_RESULT([$libquad_have_float128])

dnl
dnl Enable the following for a stand-alone library:
dnl
dnl if test $libquad_have_float128 = no; then
dnl   AC_MSG_ERROR([__float128 support is required to build this library.])
dnl fi

AM_CONDITIONAL(BUILD_LIBQUADMATH, [test "x$libquad_have_float128" = xyes])

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES(Makefile)
AC_OUTPUT
