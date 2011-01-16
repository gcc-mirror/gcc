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
ACX_NONCANONICAL_TARGET

target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AM_INIT_AUTOMAKE([1.9.0 foreign subdir-objects no-dist -Wall -Wno-override -Werror])

AC_MSG_CHECKING([for --enable-version-specific-runtime-libs])
AC_ARG_ENABLE(version-specific-runtime-libs,
AS_HELP_STRING([--enable-version-specific-runtime-libs],
 [specify that runtime libraries should be installed in a compiler-specific directory]),
[case "$enableval" in
 yes) version_specific_libs=yes ;;
 no)  version_specific_libs=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[version_specific_libs=no])
AC_MSG_RESULT($version_specific_libs)

GCC_NO_EXECUTABLES

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER([MAKEINFO], [makeinfo], [--version],
                   [GNU texinfo.* \([0-9][0-9.]*\)],
                   [4.[4-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
AM_CONDITIONAL(BUILD_INFO, test $gcc_cv_prog_makeinfo_modern = "yes")

ACX_BUGURL([http://gcc.gnu.org/bugs.html])

# Configure libtool
AM_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

AM_MAINTAINER_MODE
AM_ENABLE_MULTILIB(, ..)

AC_LANG_C
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename_force([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

AM_PROG_CC_C_O

if test "x$GCC" != "xyes"; then
  AC_MSG_ERROR([libquadmath must be built with GCC])
fi
AC_PROG_CPP

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

AC_CHECK_HEADERS(fenv.h)

# If available, sqrtl and cbrtl speed up the calculation -
# but they are not required
if test x$gcc_no_link != xyes; then
  AC_CHECK_LIB([m],[sqrtl],[AC_DEFINE([HAVE_SQRTL],[1],[libm includes sqrtl])])
  AC_CHECK_LIB([m],[cbrtl],[AC_DEFINE([HAVE_CBRTL],[1],[libm includes cbrtl])])
  AC_CHECK_LIB([m],[feholdexcept],[AC_DEFINE([HAVE_FEHOLDEXCEPT],[1],[libm includes feholdexcept])])
  AC_CHECK_LIB([m],[fesetround],[AC_DEFINE([HAVE_FESETROUND],[1],[libm includes fesetround])])
  AC_CHECK_LIB([m],[feupdateenv],[AC_DEFINE([HAVE_FEUPDATEENV],[1],[libm includes feupdateenv])])
  AC_CHECK_LIB([m],[fesetenv],[AC_DEFINE([HAVE_FESETENV],[1],[libm includes fesetenv])])
  AC_CHECK_LIB([m],[fetestexcept],[AC_DEFINE([HAVE_FETESTEXCEPT],[1],[libm includes fetestexcept])])
else
  if test "x$ac_cv_lib_m_sqrtl" = x""yes; then
    AC_DEFINE([HAVE_SQRTL],[1],[libm includes sqrtl])  
  fi
  if test "x$ac_cv_lib_m_cbrtl" = x""yes; then
    AC_DEFINE([HAVE_CBRTL],[1],[libm includes cbrtl])  
  fi
  if test "x$ac_cv_lib_m_feholdexcept" = x""yes; then
    AC_DEFINE([HAVE_FEHOLDEXCEPT],[1],[libm includes feholdexcept])
  fi
  if test "x$ac_cv_lib_m_fesetround" = x""yes; then
    AC_DEFINE([HAVE_FESETROUND],[1],[libm includes fesetround])
  fi
  if test "x$ac_cv_lib_m_feupdateenv" = x""yes; then
    AC_DEFINE([HAVE_FEUPDATEENV],[1],[libm includes feupdateenv])
  fi
  if test "x$ac_cv_lib_m_fesetenv" = x""yes; then
    AC_DEFINE([HAVE_FESETENV],[1],[libm includes fesetenv])
  fi
  if test "x$ac_cv_lib_m_fetestexcept" = x""yes; then
    AC_DEFINE([HAVE_FETESTEXCEPT],[1],[libm includes fetestexcept])
  fi
fi

# Check for symbol versioning (copied from libssp).
AC_MSG_CHECKING([whether symbol versioning is supported])
if test x$gcc_no_link = xyes; then
  # If we cannot link, we cannot build shared libraries, so do not use
  # symbol versioning.
  quadmath_use_symver=no
else
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
fi
AC_MSG_RESULT($quadmath_use_symver)
AM_CONDITIONAL(LIBQUAD_USE_SYMVER, [test "x$quadmath_use_symver" != xno])
AM_CONDITIONAL(LIBQUAD_USE_SYMVER_GNU, [test "x$quadmath_use_symver" = xgnu])
AM_CONDITIONAL(LIBQUAD_USE_SYMVER_SUN, [test "x$quadmath_use_symver" = xsun])

AC_CACHE_CHECK([whether __float128 is supported], [libquad_cv_have_float128],
  [GCC_TRY_COMPILE_OR_LINK([
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
    libquad_cv_have_float128=yes
  ],[
    libquad_cv_have_float128=no
])])
AM_CONDITIONAL(BUILD_LIBQUADMATH, [test "x$libquad_cv_have_float128" = xyes])

AC_CACHE_SAVE

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi


# We would like our source tree to be readonly. However when releases or
# pre-releases are generated, the flex/bison generated files as well as the
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do 
# just that.
AC_MSG_CHECKING(generated-files-in-srcdir)
AC_ARG_ENABLE(generated-files-in-srcdir,
AS_HELP_STRING([--enable-generated-files-in-srcdir],
 [put copies of generated files in source dir intended for creating source tarballs for users without texinfo bison or flex]),
[case "$enableval" in
 yes) enable_generated_files_in_srcdir=yes ;;
 no)  enable_generated_files_in_srcdir=no ;;
 *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
 esac],
[enable_generated_files_in_srcdir=no])
AC_MSG_RESULT($enable_generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test "$enable_generated_files_in_srcdir" = yes)


AC_CONFIG_FILES(Makefile)
AC_OUTPUT
