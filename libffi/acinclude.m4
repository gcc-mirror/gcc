sinclude(../libtool.m4)
dnl The lines below arrange for aclocal not to bring libtool.m4
dnl AC_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[
AC_DEFUN([AC_PROG_LIBTOOL],)
AC_SUBST(LIBTOOL)
])
