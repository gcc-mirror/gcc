dnl Used by aclocal to generate configure

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
    AC_PATH_PROGS([USER_JAVAH],[gjavah gjavah-4.3 gjavah-4.2 gjavah-4.1 gcjh-wrapper-4.1 gcjh-4.1 javah])
  fi
  
  if test "x${USER_JAVAH}" = x; then
    AC_MSG_ERROR([can not find javah])
  fi
])

dnl -----------------------------------------------------------
dnl CLASSPATH_WITH_CLASSLIB - checks for user specified classpath additions
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_WITH_CLASSLIB],
[
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

  AC_MSG_CHECKING(for a jar-like tool)
  AC_ARG_WITH([jar],
	      [AS_HELP_STRING([--with-jar=PATH], [define to use a jar style tool])],
	      [
	        case "${withval}" in
      		  yes)
		    JAR=yes
        	    ;;
      		  no)
		    JAR=no
  		    AC_MSG_RESULT(${JAR})
		    ;;
		  *)
    		    if test -f "${withval}"; then
          	      JAR="${withval}"
		      AC_MSG_RESULT(${JAR})
        	    else
	  	      AC_MSG_RESULT([not found])
	              AC_MSG_ERROR([The jar tool ${withval} was not found.])
        	    fi
		    ;;
     		esac
  	      ],
	      [
		JAR=yes
	      ])
  if test x"${JAR}" = "xyes"; then
    AC_MSG_RESULT([trying fastjar, gjar and jar])
    AC_PATH_PROGS([JAR], [fastjar gjar jar])
    if test x"${RHINO_JAR}" = "xyes"; then
      AC_MSG_RESULT([not found])
    fi
  fi
  if test x"${JAR}" = "xno" && test x"${ZIP}" = ""; then
    AC_MSG_ERROR([No zip or jar tool found.])
  fi
  AM_CONDITIONAL(WITH_JAR, test x"${JAR}" != "xno" && test x"${JAR}" != "xyes")
  AC_SUBST(JAR)
  
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
  if test "x${build_class_files}" = xno; then
    EXAMPLESDIR=""
  fi
  AC_SUBST(EXAMPLESDIR)

  AC_ARG_ENABLE([tools],
		[AS_HELP_STRING(--enable-tools,enable build of the tools [default=yes])],
		[case "${enableval}" in
		  yes) TOOLSDIR="tools" ;;
		  no) TOOLSDIR="" ;;
		  *) AC_MSG_ERROR(bad value ${enableval} for --enable-tools) ;;
		esac],
		[TOOLSDIR="tools"])
  if test "x${build_class_files}" = xno; then
    TOOLSDIR=""
  fi
  AC_SUBST(TOOLSDIR)
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
  if test "x${WITH_GJDOC}" = xyes; then
    AC_MSG_CHECKING([version of GJDoc])
    gjdoc_version=$(${GJDOC} --version|cut -d ' ' -f2)
    AC_MSG_RESULT(${gjdoc_version})
    case ${gjdoc_version} in
      0.7.9) ;;
      0.8*) ;;
      0.9*) ;;
      1*) ;;
      *) AC_MSG_ERROR([Building documentation requires GJDoc >= 0.7.9, ${gjdoc_version} found.]) ;;
    esac
  fi
])

dnl -----------------------------------------------------------
dnl Enable regeneration of parsers using jay
dnl http://www.informatik.uni-osnabrueck.de/alumni/bernd/jay/
dnl -----------------------------------------------------------
AC_DEFUN([REGEN_WITH_JAY],
[
  AC_ARG_WITH([jay],
              [AS_HELP_STRING(--with-jay[=DIR|PATH],Regenerate the parsers with jay)],
  [
    AC_MSG_CHECKING([whether to regenerate parsers with jay])
    JAY_FOUND=no
    JAY_DIR_PATH=
    if test "x${withval}" = xno; then
      AC_MSG_RESULT(no)
    elif test "x${withval}" = xyes; then
      AC_MSG_RESULT(yes)
      JAY_DIR_PATH="/usr/share/jay"
    elif test -d "${withval}"; then
      AC_MSG_RESULT(yes)
      JAY_DIR_PATH="${withval}"
    elif test -f "${withval}"; then
      AC_MSG_RESULT(yes)
      JAY_DIR_PATH=`dirname "${withval}"`
      JAY="${withval}"
    else
        AC_MSG_ERROR(jay not found at ${withval})
    fi

    if test "x${JAY_DIR_PATH}" != x; then
      AC_PATH_PROG(JAY, jay, "no", ${JAY_DIR_PATH}:${PATH})
      if test "x${JAY}" = xno; then
        AC_MSG_ERROR(jay executable not found);
      fi
      JAY_SKELETON="${JAY_DIR_PATH}/skeleton"
      AC_CHECK_FILE(${JAY_SKELETON}, AC_SUBST(JAY_SKELETON),
          AC_MSG_ERROR(Expected skeleton file in ${JAY_DIR_PATH}))
      JAY_FOUND=yes
    fi
  ],
  [
    AC_MSG_CHECKING([whether to regenerate parsers with jay])
    AC_MSG_RESULT(no)
    JAY_FOUND=no
  ])
  AM_CONDITIONAL(REGEN_PARSERS, test "x${JAY_FOUND}" = xyes)
])

dnl -----------------------------------------------------------
dnl GCJ LOCAL: Calculate toolexeclibdir
dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_TOOLEXECLIBDIR],
[
  multi_os_directory=`$CC -print-multi-os-directory`
  case $multi_os_directory in
    .) toolexeclibdir=${libdir} ;; # Avoid trailing /.
    *) toolexeclibdir=${libdir}/${multi_os_directory} ;;
  esac
  AC_SUBST(toolexeclibdir)
])

dnl -----------------------------------------------------------
AC_DEFUN([CLASSPATH_JAVAC_MEM_CHECK],
[
  JAVA_TEST=Test.java
  CLASS_TEST=Test.class
  cat << \EOF > $JAVA_TEST
  /* [#]line __oline__ "configure" */
  public class Test 
  {
    public static void main(String[] args)
    {
      System.out.println("Hello World");
    }
  }
EOF
  if test x$JAVAC_IS_GCJ != xyes; then
    AC_MSG_CHECKING([whether javac supports -J])
    $JAVAC $JAVACFLAGS -J-Xmx768M -sourcepath '' $JAVA_TEST
    javac_result=$?
    if test "x$javac_result" = "x0"; then
      AC_MSG_RESULT([yes])
      JAVAC_MEM_OPT="-J-Xmx768M"
    else
      AC_MSG_RESULT([no])
    fi
  fi
  rm -f $JAVA_TEST $CLASS_TEST
  AC_SUBST(JAVAC_MEM_OPT)
])

dnl ---------------------------------------------------------------
dnl CLASSPATH_COND_IF(COND, SHELL-CONDITION, [IF-TRUE], [IF-FALSE])
dnl ---------------------------------------------------------------
dnl Automake 1.11 can emit conditional rules for AC_CONFIG_FILES,
dnl using AM_COND_IF.  This wrapper uses it if it is available,
dnl otherwise falls back to code compatible with Automake 1.9.6.
AC_DEFUN([CLASSPATH_COND_IF],
[m4_ifdef([AM_COND_IF],
  [AM_COND_IF([$1], [$3], [$4])],
  [if $2; then
     m4_default([$3], [:])
   else
     m4_default([$4], [:])
   fi
])])
