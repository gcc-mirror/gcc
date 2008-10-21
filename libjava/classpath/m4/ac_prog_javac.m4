dnl @synopsis AC_PROG_JAVAC
dnl
dnl AC_PROG_JAVAC tests an existing Java compiler. It uses the
dnl environment variable JAVAC then tests in sequence various common
dnl Java compilers. For political reasons, it starts with the free
dnl ones.
dnl
dnl If you want to force a specific compiler:
dnl
dnl - at the configure.in level, set JAVAC=yourcompiler before calling
dnl AC_PROG_JAVAC
dnl
dnl - at the configure level, setenv JAVAC
dnl
dnl You can use the JAVAC variable in your Makefile.in, with @JAVAC@.
dnl
dnl *Warning*: its success or failure can depend on a proper setting of
dnl the CLASSPATH env. variable.
dnl
dnl TODO: allow to exclude compilers (rationale: most Java programs
dnl cannot compile with some compilers like guavac).
dnl
dnl Note: This is part of the set of autoconf M4 macros for Java
dnl programs. It is VERY IMPORTANT that you download the whole set,
dnl some macros depend on other. Unfortunately, the autoconf archive
dnl does not support the concept of set of macros, so I had to break it
dnl for submission. The general documentation, as well as the sample
dnl configure.in, is included in the AC_PROG_JAVA macro.
dnl
dnl @category Java
dnl @author Stephane Bortzmeyer <bortzmeyer@pasteur.fr>
dnl @version 2000-07-19
dnl @license GPLWithACException
dnl
dnl Modified to remove jikes by Andrew John Hughes on 2008-02-11

AC_DEFUN([AC_PROG_JAVAC],[
AC_REQUIRE([AC_EXEEXT])dnl
ECJ_OPTS="-warn:-deprecation,serial,unusedImport"
JAVAC_OPTS="-Xlint:unchecked,cast,divzero,empty,finally,overrides"
GCJ_OPTS="-g"
if test "x$JAVAPREFIX" = x; then
        test "x$JAVAC" = x && AC_CHECK_PROGS(JAVAC, ["ecj$EXEEXT $ECJ_OPTS"] ["ecj-3.3$EXEEXT $ECJ_OPTS"] ["ecj-3.2$EXEEXT $ECJ_OPTS"] ["javac$EXEEXT $JAVAC_OPTS"] "gcj$EXEEXT -C")
else
        test "x$JAVAC" = x && AC_CHECK_PROGS(JAVAC, ["ecj$EXEEXT $ECJ_OPTS"] ["ecj-3.3$EXEEXT $ECJ_OPTS"] ["ecj-3.2$EXEEXT $ECJ_OPTS"] ["javac$EXEEXT $JAVAC_OPTS"] "gcj$EXEEXT -C", $JAVAPREFIX)
fi
test "x$JAVAC" = x && AC_MSG_ERROR([no acceptable Java compiler found in \$PATH])
AC_CACHE_CHECK([if $JAVAC is a version of gcj], ac_cv_prog_javac_is_gcj, [
if $JAVAC --version | grep gcj > /dev/null; then
  ac_cv_prog_javac_is_gcj=yes;
  JAVAC="$JAVAC $GCJ_OPTS";
fi])
AC_SUBST(JAVAC_IS_GCJ, $ac_cv_prog_javac_is_gcj)
AM_CONDITIONAL(GCJ_JAVAC, test x"${JAVAC_IS_GCJ}" = xyes)
dnl GCJ LOCAL
if test "$enable_java_maintainer_mode" = yes; then
AC_PROG_JAVAC_WORKS
fi
dnl END GCJ LOCAL
AC_PROVIDE([$0])dnl
])
