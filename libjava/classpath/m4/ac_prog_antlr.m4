
AC_DEFUN([AC_LIB_ANTLR],[
  AC_MSG_CHECKING([for the ANTLR parser generator JAR file])
  if test -z "$ANTLR_JAR"; then
    for antlr_lib_home in `ls -d /usr/local/share/antlr* 2> /dev/null` \
	/usr/share/antlr/lib /usr/share/java /usr/lib;
	do
	    if test -f "$antlr_lib_home/antlr.jar"; then
	      ANTLR_JAR="$antlr_lib_home/antlr.jar"
	      break
	    fi
     done
  fi
  test -f $ANTLR_JAR || AC_MSG_ERROR([specified ANTLR jar file $ANTLR_JAR not found.]) \
        && AC_MSG_RESULT([$ANTLR_JAR])
  AC_SUBST(ANTLR_JAR)
  AC_PROVIDE([$0])dnl
])

AC_DEFUN([AC_PROG_ANTLR],[
  AC_REQUIRE([AC_PROG_JAVA])dnl
  AC_CHECK_TOOLS([ANTLR], [cantlr runantlr antlr])
  if test "x$ANTLR" = x; then
      if test -z "$JAVA"; then
        AC_MSG_ERROR(Failed to find either an antlr binary or a suitable Java runtime for ANTLR.)
      else
        ANTLR="$JAVA -classpath $ANTLR_JAR antlr.Tool"
      fi
  fi
  AC_SUBST(ANTLR)
  AC_MSG_CHECKING([for antlr $1.$2.$3 or better])
  antlr_version_str=`$ANTLR 2>&1 | head -n 1 | sed '/.*Version */!d; s///;q'`
  if test "$antlr_version_str"; then
    antlr_version_regex='s/\([[[:digit:]]]\+\)\.\([[[:digit:]]]\+\)\.\([[[:digit:]]]\+\).*$/'
    antlr_version_major=`echo $antlr_version_str | sed "$antlr_version_regex\\1/"`
    antlr_version_minor=`echo $antlr_version_str | sed "$antlr_version_regex\\2/"`
    antlr_version_micro=`echo $antlr_version_str | sed "$antlr_version_regex\\3/"`
    (test $antlr_version_major -gt $1 || \
    (test $antlr_version_major -eq $1 && \
     test $antlr_version_minor -gt $2) || \
    (test $antlr_version_major -eq $1 && \
     test $antlr_version_minor -eq $2 && \
     test $antlr_version_micro -ge $3))
  fi
  AC_MSG_RESULT($antlr_version_major.$antlr_version_minor.$antlr_version_micro)
])
