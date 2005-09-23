dnl Used by aclocal to generate configure

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_FIND_JAVAC],
[
  user_specified_javac=

  CLASSPATH_WITH_GCJ
  CLASSPATH_WITH_JIKES
  CLASSPATH_WITH_KJC
  CLASSPATH_WITH_GCJX
  CLASSPATH_WITH_ECJ

  if test "x${user_specified_javac}" = x; then
    AM_CONDITIONAL(FOUND_GCJ, test "x${GCJ}" != x)
    AM_CONDITIONAL(FOUND_JIKES, test "x${JIKES}" != x)
    AM_CONDITIONAL(FOUND_ECJ, test "x${ECJ}" != x)
  else
    AM_CONDITIONAL(FOUND_GCJ, test "x${user_specified_javac}" = xgcj)
    AM_CONDITIONAL(FOUND_JIKES, test "x${user_specified_javac}" = xjikes)
    AM_CONDITIONAL(FOUND_ECJ, test "x${user_specified_javac}" = xecj)
  fi
  AM_CONDITIONAL(FOUND_KJC, test "x${user_specified_javac}" = xkjc)
  AM_CONDITIONAL(FOUND_GCJX, test "x${user_specified_javac}" = xgcjx)

  if test "x${GCJ}" = x && test "x${JIKES}" = x && test "x${user_specified_javac}" != xkjc && test "x${user_specified_javac}" != xgcjx; then
      # FIXME: use autoconf error function
      echo "configure: cannot find javac, try --with-gcj, --with-jikes, --with-kjc, or --with-gcjx" 1>&2
      exit 1    
  fi
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_GCJ],
[
  AC_ARG_WITH([gcj],
	      [AS_HELP_STRING(--with-gcj,bytecode compilation with gcj)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_GCJ(${withval})
    else
      if test "x${withval}" != xno; then
        CLASSPATH_CHECK_GCJ
      fi
    fi
    user_specified_javac=gcj
  ],
  [
    CLASSPATH_CHECK_GCJ
  ])
  AC_SUBST(GCJ)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_GCJ],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      GCJ="$1"
    else
      AC_PATH_PROG(GCJ, "$1")
    fi
  else
    AC_PATH_PROG(GCJ, "gcj")
  fi  

  if test "x$GCJ" != x; then
    ## GCC version 2 puts out version messages that looked like:
    ##   2.95

    ## GCC version 3 puts out version messages like:
    ##   gcj (GCC) 3.3.3
    ##   Copyright (C) 2003 Free Software Foundation, Inc.
    ##   This is free software; see the source for copying conditions.  There is NO
    ##   warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    AC_MSG_CHECKING(gcj version)
    ## Take the output from gcj --version and extract just the version number
    ## into GCJ_VERSION.
    ## (we need to do this to be compatible with both GCC 2 and GCC 3 version
    ##  numbers)
    ## 
    ## First, we get rid of everything before the first number on that line.
    ## Assume that the first number on that line is the start of the
    ## version.
    ##
    ## Second, while we're at it, go ahead and get rid of the first character
    ## that is not part of a version number (i.e., is neither a digit nor
    ## a dot).
    ##
    ## Third, quit, so that we won't process the second and subsequent lines.
    GCJ_VERSION=`$GCJ --version | sed -e 's/^@<:@^0-9@:>@*//' -e 's/@<:@^.0-9@:>@@<:@^.0-9@:>@*//' -e 'q'` 
    GCJ_VERSION_MAJOR=`echo "$GCJ_VERSION" | cut -d '.' -f 1`
    GCJ_VERSION_MINOR=`echo "$GCJ_VERSION" | cut -d '.' -f 2`

    if expr "$GCJ_VERSION_MAJOR" \< 3 > /dev/null; then
      GCJ=""
    fi
    if expr "$GCJ_VERSION_MAJOR" = 3 > /dev/null; then
      if expr "$GCJ_VERSION_MINOR" \< 3; then
        GCJ=""
      fi
    fi
    if test "x$GCJ" != x; then
      AC_MSG_RESULT($GCJ_VERSION)
    else
      AC_MSG_WARN($GCJ_VERSION: gcj 3.3 or higher required)
    fi
  fi 
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_JIKES],
[
  AC_ARG_WITH([jikes],
	      [AS_HELP_STRING(--with-jikes,bytecode compilation with jikes)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_JIKES(${withval})
    else
      if test "x${withval}" != xno; then
        CLASSPATH_CHECK_JIKES
      fi
    fi
    user_specified_javac=jikes
  ],
  [ 
    CLASSPATH_CHECK_JIKES
  ])
  AC_SUBST(JIKES)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_JIKES],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      JIKES="$1"
    else
      AC_PATH_PROG(JIKES, "$1")
    fi
  else
    AC_PATH_PROG(JIKES, "jikes")
  fi
  if test "x$JIKES" != "x"; then
    dnl Require at least version 1.19
    AC_MSG_CHECKING(jikes version)
    JIKES_VERSION=`$JIKES --version | awk '/^Jikes Compiler/' | cut -d ' ' -f 5`
    JIKES_VERSION_MAJOR=`echo "$JIKES_VERSION" | cut -d '.' -f 1`
    JIKES_VERSION_MINOR=`echo "$JIKES_VERSION" | cut -d '.' -f 2`
    if expr "$JIKES_VERSION_MAJOR" = 1 > /dev/null; then
      if expr "$JIKES_VERSION_MINOR" \< 19 > /dev/null; then
        JIKES=""
      fi
    fi
    if test "x$JIKES" != "x"; then
      AC_MSG_RESULT($JIKES_VERSION)
    else
      AC_MSG_WARN($JIKES_VERSION: jikes 1.19 or higher required)
    fi

    JIKESENCODING=
    if test -n "`$JIKES --help 2>&1 | grep encoding`"; then
       JIKESENCODING='-encoding UTF-8'
    fi
    AC_SUBST(JIKESENCODING)
  fi
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_KJC],
[
  AC_ARG_WITH([kjc], 
  	      [AS_HELP_STRING(--with-kjc,bytecode compilation with kjc)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_KJC(${withval})
    else
      if test "x${withval}" != xno; then
        CLASSPATH_CHECK_KJC
      fi
    fi
    user_specified_javac=kjc
  ],
  [ 
    CLASSPATH_CHECK_KJC
  ])
  AC_SUBST(KJC)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_KJC],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      KJC="$1"
    else
      AC_PATH_PROG(KJC, "$1")
    fi
  else
    AC_PATH_PROG(KJC, "kJC")
  fi
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_GCJX],
[
  AC_ARG_WITH([gcjx], 
  	      [AS_HELP_STRING(--with-gcjx,bytecode compilation with gcjx)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_GCJX(${withval})
    else
      if test "x${withval}" != xno; then
        CLASSPATH_CHECK_GCJX
      fi
    fi
    user_specified_javac=gcjx
  ],
  [ 
    CLASSPATH_CHECK_GCJX
  ])
  AC_SUBST(GCJX)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_GCJX],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      GCJX="$1"
    else
      AC_PATH_PROG(GCJX, "$1")
    fi
  else
    AC_PATH_PROG(GCJX, "gcjx")
  fi
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_JAVAH],
[
  AC_ARG_WITH([javah],
	      [AS_HELP_STRING(--with-javah,specify path or name of a javah-like program)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_JAVAH(${withval})
    else
      CLASSPATH_CHECK_JAVAH
    fi
  ],
  [ 
    CLASSPATH_CHECK_JAVAH
  ])
  AM_CONDITIONAL(USER_SPECIFIED_JAVAH, test "x${USER_JAVAH}" != x)
  AC_SUBST(USER_JAVAH)
])

dnl -----------------------------------------------------------
dnl Checking for a javah like program 
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_JAVAH],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      USER_JAVAH="$1"
    else
      AC_PATH_PROG(USER_JAVAH, "$1")
    fi
  else
    for javah_name in gcjh javah; do
      AC_PATH_PROG(USER_JAVAH, "$javah_name")
      if test "x${USER_JAVAH}" != x; then
        break
      fi
    done
  fi
  
#  if test "x${USER_JAVAH}" = x; then
#    echo "configure: cannot find javah" 1>&2
#    exit 1
#  fi
])

dnl -----------------------------------------------------------
dnl CLASSPATH_WITH_CLASSLIB - checks for user specified classpath additions
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_CLASSLIB],
[
  AC_ARG_WITH([classpath],
	      [AS_HELP_STRING(--with-classpath,specify path to a classes.zip like file)],
  [
    if test "x${withval}" = xyes; then
      # set user classpath to CLASSPATH from env
      AC_MSG_CHECKING(for classlib)
      USER_CLASSLIB=${CLASSPATH}
      AC_SUBST(USER_CLASSLIB)
      AC_MSG_RESULT(${USER_CLASSLIB})
      conditional_with_classlib=true      
    elif test "x${withval}" != x && test "x${withval}" != xno; then
      # set user classpath to specified value
      AC_MSG_CHECKING(for classlib)
      USER_CLASSLIB=${withval}
      AC_SUBST(USER_CLASSLIB)
      AC_MSG_RESULT(${withval})
      conditional_with_classlib=true
    fi
  ],
  [ conditional_with_classlib=false ])
  AM_CONDITIONAL(USER_SPECIFIED_CLASSLIB, test "x${conditional_with_classlib}" = xtrue)

  AC_ARG_WITH([vm-classes],
	      [AS_HELP_STRING(--with-vm-classes,specify path to VM override source files)], [vm_classes="$with_vm_classes"],
	      [vm_classes='${top_srcdir}/vm/reference'])
  AC_SUBST(vm_classes)
])

dnl -----------------------------------------------------------
dnl CLASSPATH_WITH_GLIBJ - specify what to install
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_GLIBJ],
[
  AC_PATH_PROG(ZIP, zip)
  AC_ARG_WITH([glibj],
              [AS_HELP_STRING([--with-glibj],[define what to install (zip|flat|both|none|build) [default=zip]])],
              [
                if test "x${withval}" = xyes || test "x${withval}" = xzip; then
		  install_class_files=no
		  build_class_files=yes
		  use_zip=yes
		elif test "x${withval}" = xboth; then
		  install_class_files=yes
		  build_class_files=yes
		  use_zip=yes
		elif test "x${withval}" = xflat; then
		  install_class_files=yes
		  build_class_files=yes
		  use_zip=no
                elif test "x${withval}" = xno || test "x${withval}" = xnone; then
		  install_class_files=no
		  build_class_files=no
		  use_zip=no
		elif test "x${withval}" = xbuild; then
		  install_class_files=no
		  build_class_files=yes
		  use_zip=no
                else
		  AC_MSG_ERROR([unknown value given to --with-glibj])
                fi
	      ],
  	      [
		install_class_files=no
		use_zip=yes
	      ])
  AM_CONDITIONAL(INSTALL_GLIBJ_ZIP, test "x${use_zip}" = xyes)
  AM_CONDITIONAL(INSTALL_CLASS_FILES, test "x${install_class_files}" = xyes)
  AM_CONDITIONAL(BUILD_CLASS_FILES, test "x${build_class_files}" = xyes)

  AC_ARG_ENABLE([examples],
		[AS_HELP_STRING(--enable-examples,enable build of the examples [default=yes])],
		[case "${enableval}" in
		  yes) EXAMPLESDIR="examples" ;;
		  no) EXAMPLESDIR="" ;;
		  *) AC_MSG_ERROR(bad value ${enableval} for --enable-examples) ;;
		esac],
		[EXAMPLESDIR="examples"])
  if test "x${use_zip}" = xno && test "x${install_class_files}" = xno; then
    EXAMPLESDIR=""
  fi
  AC_SUBST(EXAMPLESDIR)
])

dnl -----------------------------------------------------------
dnl Enable generation of API documentation, with gjdoc if it
dnl has been compiled to an executable (or a suitable script
dnl is in your PATH) or using the argument as gjdoc executable.
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_GJDOC],
[
  AC_ARG_WITH([gjdoc],
              AS_HELP_STRING([--with-gjdoc],
			     [generate documentation using gjdoc (default is NO)]),
              [if test "x${withval}" = xno; then
	         WITH_GJDOC=no;
	       elif test "x${withval}" = xyes -o "x{withval}" = x; then
	         WITH_GJDOC=yes;
	         AC_PATH_PROG(GJDOC, gjdoc, "no")
		 if test "x${GJDOC}" = xno; then
		   AC_MSG_ERROR("gjdoc executable not found");
		 fi
	       else
	         WITH_GJDOC=yes
		 GJDOC="${withval}"
		 AC_CHECK_FILE(${GJDOC}, AC_SUBST(GJDOC),
		               AC_MSG_ERROR("Cannot use ${withval} as gjdoc executable since it doesn't exist"))
	       fi],
              [WITH_GJDOC=no])

  AM_CONDITIONAL(CREATE_API_DOCS, test "x${WITH_GJDOC}" = xyes)
])

dnl -----------------------------------------------------------
dnl Enable regeneration of parsers using jay
dnl http://www.informatik.uni-osnabrueck.de/alumni/bernd/jay/
dnl -----------------------------------------------------------
AC_DEFUN([REGEN_WITH_JAY],
[
  AC_ARG_WITH([jay],
              [AS_HELP_STRING(--with-jay,Regenerate the parsers with jay must be given the path to the jay executable)],
  [
    if test -d "${withval}"; then
      JAY_DIR_PATH="${withval}"
      AC_PATH_PROG(JAY, jay, "no", ${JAY_DIR_PATH})
      if test "x${JAY}" = xno; then
        AC_MSG_ERROR("jay executable not found");
      fi
    else
      JAY_DIR_PATH=$(dirname "${withval}")
      JAY="${withval}"
      AC_SUBST(JAY)
    fi
    JAY_SKELETON="${JAY_DIR_PATH}/skeleton"
    AC_CHECK_FILE(${JAY_SKELETON}, AC_SUBST(JAY_SKELETON),
	AC_MSG_ERROR("Expected skeleton file in $(dirname ${withval})"))
    JAY_FOUND=yes
  ],
  [
    JAY_FOUND=no
  ])
  AM_CONDITIONAL(REGEN_PARSERS, test "x${JAY_FOUND}" = xyes)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_ECJ],
[
  AC_ARG_WITH([ecj],
	      [AS_HELP_STRING(--with-ecj,bytecode compilation with ecj)],
  [
    if test "x${withval}" != x && test "x${withval}" != xyes && test "x${withval}" != xno; then
      CLASSPATH_CHECK_ECJ(${withval})
    else
      if test "x${withval}" != xno; then
        CLASSPATH_CHECK_ECJ
      fi
    fi
    user_specified_javac=ecj
  ],
  [ 
    CLASSPATH_CHECK_ECJ
  ])
  AC_SUBST(ECJ)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_CHECK_ECJ],
[
  if test "x$1" != x; then
    if test -f "$1"; then
      ECJ="$1"
    else
      AC_PATH_PROG(ECJ, "$1")
    fi
  else
    AC_PATH_PROG(ECJ, "ecj")
  fi
])
