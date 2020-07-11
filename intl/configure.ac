AC_INIT
AC_USE_SYSTEM_EXTENSIONS
AC_CONFIG_SRCDIR(gettext.c)
AC_CONFIG_HEADER(config.h)
AC_CONFIG_MACRO_DIR(../config)
AM_GNU_GETTEXT_VERSION(0.12.1)
AM_GNU_GETTEXT([], [need-ngettext])

# This replaces the extensive use of DEFS in the original Makefile.in.
AC_DEFINE(IN_LIBINTL, 1, [Define because this is libintl.])
AC_DEFINE(IN_LIBRARY, 1, [Define because this is a library.])
AC_DEFINE(DEPENDS_ON_LIBICONV, 1, [Define because we depend on libiconv.])
AC_DEFINE(ENABLE_RELOCATABLE, 1, [Define to enable relocation.])
AC_DEFINE(NO_XMALLOC, 1, [Define if there is no xmalloc.])
AC_DEFINE(set_relocation_prefix, libintl_set_relocation_prefix,
[Define this entry point correctly.])
AC_DEFINE(relocate, libintl_relocate,
[Define this entry point correctly.])

MISSING=`cd $ac_aux_dir && ${PWDCMD-pwd}`/missing
AC_CHECK_PROGS([ACLOCAL], [aclocal], [$MISSING aclocal])
AC_CHECK_PROGS([AUTOCONF], [autoconf], [$MISSING autoconf])
AC_CHECK_PROGS([AUTOHEADER], [autoheader], [$MISSING autoheader])

AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable rules only needed by maintainers],,
enable_maintainer_mode=no)
if test "x$enable_maintainer_mode" = xno; then
  MAINT='#'
else
  MAINT=
fi
AC_SUBST(MAINT)

# Additional info for config.intl.
AC_SUBST(LIBINTL_DEP)
AC_SUBST(INCINTL)

LIBINTL_DEP=
INCINTL=
case $USE_INCLUDED_LIBINTL in
  yes)
    LIBINTL=`echo $LIBINTL | sed 's,${top_builddir},&/..,' `
    LTLIBINTL=`echo $LTLIBINTL | sed 's,${top_builddir},&/..,' `
    LIBINTL_DEP='${top_builddir}/../intl/libintl.a'
    INCINTL='-I${top_builddir}/../intl'
    ;;
esac

BISON3_YES='#'
BISON3_NO=
if test "$INTLBISON" != :; then
  ac_bison3=no
  AC_MSG_CHECKING([bison 3 or later])
changequote(<<,>>)dnl
  ac_prog_version=`$INTLBISON --version 2>&1 | sed -n 's/^.*GNU Bison.* \([0-9]*\.[0-9.]*\).*$/\1/p'`
  case $ac_prog_version in
    [3-9].*)
changequote([,])dnl
      ac_prog_version="$ac_prog_version, bison3"; ac_bison3=yes;;
    *) ac_prog_version="$ac_prog_version, old";;
  esac
  AC_MSG_RESULT([$ac_prog_version])
  if test $ac_bison3 = yes; then
    BISON3_YES=
    BISON3_NO='#'
  fi
fi
AC_SUBST(BISON3_YES)
AC_SUBST(BISON3_NO)

AC_CONFIG_FILES(Makefile config.intl)
AC_OUTPUT
