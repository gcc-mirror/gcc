dnl @synopsis AC_PROG_JAVAC_WORKS
dnl
dnl Internal use ONLY.
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
dnl Modified to test for 1.5 by Andrew John Hughes on 2008-02-11

AC_DEFUN([AC_PROG_JAVAC_WORKS],[
AC_CACHE_CHECK([if $JAVAC works], ac_cv_prog_javac_works, [
JAVA_TEST=Object.java
CLASS_TEST=Object.class
cat << \EOF > $JAVA_TEST
/* [#]line __oline__ "configure" */
package java.lang;

public class Object
{
  static <T> void doStuff()
  {
  }
}
EOF
if test x$JAVAC_IS_GCJ = xyes; then
  CMD="$JAVAC $JAVACFLAGS -fsource=1.5 -ftarget=1.5 $JAVA_TEST"
else
  CMD="$JAVAC $JAVACFLAGS -source 1.5 -target 1.5 $JAVA_TEST"
fi
if AC_TRY_COMMAND($CMD) >/dev/null 2>&1; then
  ac_cv_prog_javac_works=yes
else
  AC_MSG_ERROR([The Java compiler $JAVAC failed (see config.log, check the CLASSPATH?)])
  echo "configure: failed program was:" >&AC_FD_CC
  cat $JAVA_TEST >&AC_FD_CC
fi
rm -f $JAVA_TEST $CLASS_TEST
])
AC_PROVIDE([$0])dnl
])
