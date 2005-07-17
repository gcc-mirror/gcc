dnl Check whether the target supports TLS.
AC_DEFUN([LIBMUDFLAP_CHECK_TLS], [
  LIBMUDFLAP_ENABLE(tls, yes, [Use thread-local storage])
  AC_CACHE_CHECK([whether the target supports thread-local storage],
		 have_tls, [
    AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
      [dnl If the test case passed with dynamic linking, try again with
      dnl static linking.  This fails at least with some older Red Hat
      dnl releases.
      save_LDFLAGS="$LDFLAGS"
      LDFLAGS="-static $LDFLAGS"
      AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
		    [have_tls=yes], [have_tls=no], [])
      LDFLAGS="$save_LDFLAGS"],
      [have_tls=no],
      [AC_COMPILE_IFELSE([__thread int foo;], [have_tls=yes], [have_tls=no])]
    )])
  if test "$enable_tls $have_tls" = "yes yes"; then
    AC_DEFINE(HAVE_TLS, 1,
	      [Define to 1 if the target supports thread-local storage.])
  fi])

dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libgfortran.

sinclude(../libtool.m4)
dnl The lines below arrange for aclocal not to bring an installed
dnl libtool.m4 into aclocal.m4, while still arranging for automake to
dnl add a definition of LIBTOOL to Makefile.in.
ifelse(,,,[AC_SUBST(LIBTOOL)
AC_DEFUN([AM_PROG_LIBTOOL])
AC_DEFUN([AC_LIBTOOL_DLOPEN])
AC_DEFUN([AC_PROG_LD])
])

dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libstdc++-v3.

dnl
dnl LIBMUDFLAP_ENABLE
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, permit a|b|c)
dnl    (FEATURE, DEFAULT, HELP-ARG, HELP-STRING, SHELL-CODE-HANDLER)
dnl
dnl See docs/html/17_intro/configury.html#enable for documentation.
dnl
m4_define([LIBMUDFLAP_ENABLE],[dnl
m4_define([_g_switch],[--enable-$1])dnl
m4_define([_g_help],[AC_HELP_STRING(_g_switch$3,[$4 @<:@default=$2@:>@])])dnl
 AC_ARG_ENABLE($1,_g_help,
  m4_bmatch([$5],
   [^permit ],
     [[
      case "$enableval" in
       m4_bpatsubst([$5],[permit ])) ;;
       *) AC_MSG_ERROR(Unknown argument to enable/disable $1) ;;
          dnl Idea for future:  generate a URL pointing to
          dnl "onlinedocs/configopts.html#whatever"
      esac
     ]],
   [^$],
     [[
      case "$enableval" in
       yes|no) ;;
       *) AC_MSG_ERROR(Argument to enable/disable $1 must be yes or no) ;;
      esac
     ]],
   [[$5]]),
  [enable_]m4_bpatsubst([$1],-,_)[=][$2])
m4_undefine([_g_switch])dnl
m4_undefine([_g_help])dnl
])
