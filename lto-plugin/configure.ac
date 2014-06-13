AC_PREREQ(2.64)
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
AC_PROG_CC
AC_SYS_LARGEFILE
ACX_PROG_CC_WARNING_OPTS([-Wall], [ac_lto_plugin_warn_cflags])

# Check whether -static-libgcc is supported.
saved_LDFLAGS="$LDFLAGS"
LDFLAGS="$LDFLAGS -static-libgcc"
AC_MSG_CHECKING([for -static-libgcc])
AC_LINK_IFELSE([
  int main() {}], [have_static_libgcc=yes], [have_static_libgcc=no])
AC_MSG_RESULT($have_static_libgcc); 
LDFLAGS="$saved_LDFLAGS"
# Need -Wc to get it through libtool.
if test "x$have_static_libgcc" = xyes; then
   ac_lto_plugin_ldflags="-Wc,-static-libgcc"
fi
AC_SUBST(ac_lto_plugin_ldflags)

AM_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AC_SUBST(target_noncanonical)
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_HEADER_SYS_WAIT
AC_CONFIG_FILES(Makefile)
AC_CONFIG_HEADERS(config.h)
AC_OUTPUT
