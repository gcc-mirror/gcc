dnl Available from the GNU Autoconf Macro Archive at:
dnl http://www.gnu.org/software/ac-archive/htmldoc/ac_prog_java.html
dnl
dnl Changes for gjdoc: check for "kaffe, gij, java", not only "kaffe, java"
dnl Changes for GNU Classpath: check for "cacao, jamvm" as well
dnl
AC_DEFUN([AC_PROG_JAVA],[
AC_REQUIRE([AC_EXEEXT])dnl
if test "x$JAVAPREFIX" = x; then
        test "x$JAVA" = x && AC_CHECK_PROGS(JAVA, cacao$EXEEXT jamvm$EXEEXT kaffe$EXEEXT gij$EXEEXT java$EXEEXT)
else
        test "x$JAVA" = x && AC_CHECK_PROGS(JAVA, cacao$EXEEXT jamvm$EXEEXT kaffe$EXEEXT gij$EXEEXT java$EXEEXT, $JAVAPREFIX)
fi
test "x$JAVA" = x && AC_MSG_ERROR([no acceptable Java virtual machine found in \$PATH])
AC_PROG_JAVA_WORKS
AC_PROVIDE([$0])dnl
])
