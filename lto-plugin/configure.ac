AC_INIT([LTO plugin for ld], 0.1,,[lto-plugin])
AC_CANONICAL_SYSTEM
GCC_TOPLEV_SUBDIRS
AM_INIT_AUTOMAKE([foreign no-dist])
AM_MAINTAINER_MODE
AC_ARG_WITH(libiberty,
  [AS_HELP_STRING([--with-libiberty=PATH],
    [specify the directory where to find libiberty [../libiberty]])],
  [], with_libiberty=../libiberty)
AC_SUBST(with_libiberty)
AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC
AC_SYS_LARGEFILE
ACX_PROG_CC_WARNING_OPTS([-Wall], [ac_lto_plugin_warn_cflags])

# Check whether -static-libgcc is supported.
saved_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS -static-libgcc"
AC_MSG_CHECKING([for -static-libgcc])
AC_LINK_IFELSE([AC_LANG_SOURCE([
  int main() {}])], [have_static_libgcc=yes], [have_static_libgcc=no])
AC_MSG_RESULT($have_static_libgcc); 
LDFLAGS="$saved_LDFLAGS"
# Need -Wc to get it through libtool.
if test "x$have_static_libgcc" = xyes; then
   ac_lto_plugin_ldflags="-Wc,-static-libgcc"
fi

# Enable --enable-host-bind-now
AC_ARG_ENABLE(host-bind-now,
[AS_HELP_STRING([--enable-host-bind-now],
       [link host code as BIND_NOW])])
AC_SUBST(enable_host_bind_now)

if test x$enable_host_bind_now = xyes; then
  ac_lto_plugin_ldflags="$ac_lto_plugin_ldflags -Wl,-z,now"
fi

AC_SUBST(ac_lto_plugin_ldflags)

GCC_CET_HOST_FLAGS(CET_HOST_FLAGS)
AC_SUBST(CET_HOST_FLAGS)

if test x"$host_subdir" = x.; then
   gcc_build_dir=../gcc
else
   gcc_build_dir=../../$host_subdir/gcc
fi
AC_SUBST(gcc_build_dir)

# Used for constructing correct paths for offload compilers.
accel_dir_suffix=
real_target_noncanonical=${target_noncanonical}
if test x"$enable_as_accelerator_for" != x; then
  accel_dir_suffix=/accel/${target_noncanonical}
  real_target_noncanonical=${enable_as_accelerator_for}
fi
AC_SUBST(accel_dir_suffix)
AC_SUBST(real_target_noncanonical)

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

AC_MSG_CHECKING([whether symbol versioning is supported])
lto_plugin_use_symver=no
if test x$gcc_no_link = xyes; then
  # If we cannot link, we cannot build shared libraries, so do not use
  # symbol versioning.
  lto_plugin_use_symver=no
else
  save_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS -fPIC -shared -Wl,--version-script,./conftest.map"
  cat > conftest.map <<EOF
{
  global: *foo*; bar; local: *;
};
EOF
  AC_TRY_LINK([int foo;],[],[lto_plugin_use_symver=gnu],[lto_plugin_use_symver=no])
  if test x$lto_plugin_use_symver = xno; then
    case "$target_os" in
      solaris2*)
	LDFLAGS="$save_LDFLAGS"
	LDFLAGS="$LDFLAGS -fPIC -shared -Wl,-M,./conftest.map"
	# Sun ld cannot handle wildcards and treats all entries as undefined.
	cat > conftest.map <<EOF
{
  global: foo; local: *;
};
EOF
	AC_TRY_LINK([int foo;],[],[lto_plugin_use_symver=sun],[lto_plugin_use_symver=no])
	  ;;
    esac
  fi
  LDFLAGS="$save_LDFLAGS"
fi
AC_MSG_RESULT($lto_plugin_use_symver)
AM_CONDITIONAL(LTO_PLUGIN_USE_SYMVER, [test "x$lto_plugin_use_symver" != xno])
AM_CONDITIONAL(LTO_PLUGIN_USE_SYMVER_GNU, [test "x$lto_plugin_use_symver" = xgnu])
AM_CONDITIONAL(LTO_PLUGIN_USE_SYMVER_SUN, [test "x$lto_plugin_use_symver" = xsun])

# Check for thread headers.
use_locking=no
ac_lto_plugin_extra_ldflags=

case $host in
  riscv*)
    # do not use locking as pthread depends on libatomic
    ;;
  *-linux*)
    use_locking=yes
    ;;
esac

if test x$use_locking = xyes; then
  AC_CHECK_HEADER(pthread.h,
    [AC_DEFINE(HAVE_PTHREAD_LOCKING, 1, [Define if the system provides pthread locking mechanism.])])

  ac_lto_plugin_extra_ldflags="-pthread"
fi

AC_SUBST(ac_lto_plugin_extra_ldflags)

AM_PROG_LIBTOOL
AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])
ACX_LT_HOST_FLAGS
AC_SUBST(target_noncanonical)
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_HEADER_SYS_WAIT
AC_CONFIG_FILES(Makefile)
AC_CONFIG_HEADERS(config.h)
AC_OUTPUT
