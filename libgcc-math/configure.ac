# Process this file with autoconf to produce a configure script, like so:
# aclocal && autoconf && autoheader && automake

AC_PREREQ(2.59)
AC_INIT(libgcc-math, 1.0)
AC_CONFIG_SRCDIR(configure.ac)
AC_CANONICAL_SYSTEM

AM_INIT_AUTOMAKE

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

AM_MAINTAINER_MODE
AC_EXEEXT

AM_ENABLE_MULTILIB(, ..)

target_alias=${target_alias-$host_alias}
AC_SUBST(target_alias)

AC_LANG_C
# The same as in boehm-gc and libstdc++. Have to borrow it from there.
# We must force CC to /not/ be precious variables; otherwise
# the wrong, non-multilib-adjusted value will be used in multilibs.
# As a side effect, we have to subst CFLAGS ourselves.

m4_rename([_AC_ARG_VAR_PRECIOUS],[real_PRECIOUS])
m4_define([_AC_ARG_VAR_PRECIOUS],[])
AC_PROG_CC
m4_rename([real_PRECIOUS],[_AC_ARG_VAR_PRECIOUS])

AC_SUBST(CFLAGS)

if test "x$GCC" != "xyes"; then
  AC_MSG_ERROR([libgcc-math must be built with GCC])
fi
AC_PROG_CPP
AM_PROG_AS

AC_MSG_CHECKING([whether hidden visibility is supported])
AC_TRY_COMPILE([
void __attribute__((visibility ("hidden"))) bar (void) {}],,
[gccm_hidden=yes],[gccm_hidden=no])
AC_MSG_RESULT($gccm_hidden)
if test x$gccm_hidden = xyes; then
  AC_DEFINE([HAVE_HIDDEN_VISIBILITY],[1],[__attribute__((visibility ("hidden"))) supported])
fi

AC_MSG_CHECKING([whether symbol versioning is supported])
cat > conftest.map <<EOF
FOO_1.0 {
  global: *foo*; bar; local: *;
};
EOF
save_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS -fPIC -shared -Wl,--version-script,./conftest.map"
AC_TRY_LINK([int foo;],[],[gccm_use_symver=yes],[gccm_use_symver=no])
LDFLAGS="$save_LDFLAGS"
AC_MSG_RESULT($gccm_use_symver)
AM_CONDITIONAL(LIBGCCM_USE_SYMVER, [test "x$gccm_use_symver" = xyes])

AC_MSG_CHECKING([whether we are a 32bit target])
save_CFLAGS="$CFLAGS"
CFLAGS="-O2"
AC_TRY_LINK(,[
if (sizeof(int) == 4 && sizeof(long) == 4 && sizeof(void *) == 4)
  ;
else
  undefined_function ();
],
target_ilp32=yes,
target_ilp32=no)
CFLAGS="$save_CFLAGS"
AM_CONDITIONAL(TARGET_ILP32, [test "x$target_ilp32" = xyes])


AM_PROG_LIBTOOL
AC_SUBST(enable_shared)
AC_SUBST(enable_static)

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

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi


# Check for target library dependencies
cannot_build=no

AC_MSG_CHECKING([whether math.h conflicts with defining __isinf])
AC_TRY_LINK([
#include "math.h"
int __isinf (double x)
{
}
], [
return __isinf (1.0);
], [
AC_MSG_RESULT([no])
], [
AC_MSG_RESULT([yes])
cannot_build=yes
])


# Now check which parts we include in the library.

arch_subdirs=
case "${target}" in
  i?86-* | x86_64-* )
# Handle multilib cases
    if test "x$target_ilp32" = xyes; then
      arch_subdirs="i386"
      arch_libraries="i386/libsse2.la"
      arch_maps="i386/sse2.map"
    fi ;;
  *)
    ;;
esac
AC_SUBST(arch_subdirs)
AC_SUBST(arch_libraries)
AC_SUBST(arch_maps)
if test "$cannot_build" = "yes"; then
  arch_subdirs=
fi
AM_CONDITIONAL(BUILD_LIBGCC_MATH, [test "x$arch_subdirs" != x])


AC_CONFIG_FILES([Makefile i386/Makefile])
AC_OUTPUT
