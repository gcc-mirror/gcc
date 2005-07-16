dnl Process this file with autoconf to produce a configure script.

dnl -----------------------------------------------------------
dnl Turning off cache for debug reasons
dnl -----------------------------------------------------------
dnl define([AC_CACHE_LOAD], )dnl
dnl define([AC_CACHE_SAVE], )dnl

AC_INIT([GNU Classpath],[0.17+cvs],[classpath@gnu.org],[classpath])
AC_CONFIG_SRCDIR(java/lang/System.java)

AC_CANONICAL_TARGET

dnl -----------------------------------------------------------
dnl Fold all IA-32 CPU architectures into "x86"
dnl -----------------------------------------------------------
if expr ${target_cpu} : '.*86' > /dev/null; then target_cpu=x86; fi

dnl -----------------------------------------------------------
dnl We will not track/change lib version until we reach version 1.0
dnl at which time we'll have to be more anal about such things
dnl -----------------------------------------------------------
AC_SUBST(LIBVERSION, "0:0:0")
CLASSPATH_MODULE="-module -version-info ${LIBVERSION} -no-undefined"
AC_SUBST(CLASSPATH_MODULE)

AC_PREREQ(2.59)
AM_INIT_AUTOMAKE([1.9.0 gnu std-options tar-ustar])
AC_CONFIG_HEADERS([include/config.h])
AC_PREFIX_DEFAULT(/usr/local/classpath)

dnl -----------------------------------------------------------
dnl Enable JNI libraries (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([jni],
              [AS_HELP_STRING(--enable-jni,compile JNI source [default=yes])],
              [case "${enableval}" in 
                yes) COMPILE_JNI=yes; COMPILE_JAVA=yes ;;
                no) COMPILE_JNI=no ;;
                *) COMPILE_JNI=yes; COMPILE_JAVA=yes ;;
              esac],
              [COMPILE_JNI=yes])
AM_CONDITIONAL(CREATE_JNI_LIBRARIES, test "x${COMPILE_JNI}" = xyes)

dnl -----------------------------------------------------------
dnl Enable core JNI libraries (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([core-jni],
              [AS_HELP_STRING(--enable-core-jni,
	                      compile JNI sources for core [default=yes])],
              [case "${enableval}" in 
                yes) COMPILE_CORE_JNI=yes; COMPILE_JAVA=yes ;;
                no) COMPILE_CORE_JNI=no ;;
                *) COMPILE_CORE_JNI=yes; COMPILE_JAVA=yes ;;
              esac],
              [COMPILE_CORE_JNI=yes])
AM_CONDITIONAL(CREATE_CORE_JNI_LIBRARIES, test "x${COMPILE_CORE_JNI}" = xyes)

dnl -----------------------------------------------------------
dnl Whether to compile with -Werror or not (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([Werror],
              [AS_HELP_STRING(--disable-Werror,whether to compile C code with -Werror which turns any compiler warning into a compilation failure [default=no])],
              [case "${enableval}" in
                yes) ENABLE_WERROR=yes ;;
                no) ENABLE_WERROR=no ;;
                *) ENABLE_WERROR=yes ;;
              esac],
              [ENABLE_WERROR=no])

dnl -----------------------------------------------------------
dnl Default AWT toolkit
dnl -----------------------------------------------------------
AC_ARG_ENABLE(default-toolkit,
  AS_HELP_STRING([--enable-default-toolkit],
                 [fully qualified class name of default AWT toolkit]))
default_toolkit=$enable_default_tookit
if test "$default_toolkit" = ""; then
   default_toolkit=gnu.java.awt.peer.gtk.GtkToolkit
fi
AC_SUBST(default_toolkit)

dnl -----------------------------------------------------------
dnl Native libxml/xslt library (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([xmlj],
              [AS_HELP_STRING(--enable-xmlj,compile native libxml/xslt library [default=no])],
              [case "${enableval}" in
                yes) COMPILE_XMLJ=yes ;;
                no) COMPILE_XMLJ=no ;;
                *) COMPILE_XMLJ=no ;;
              esac],
              [COMPILE_XMLJ=no])
AM_CONDITIONAL(CREATE_XMLJ_LIBRARY, test "x${COMPILE_XMLJ}" = xyes)

dnl -----------------------------------------------------------
dnl GTK native peer (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gtk-peer],
              [AS_HELP_STRING(--disable-gtk-peer,compile GTK native peers (disabled by --disable-jni) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_GTK_PEER=yes ;;
                no) COMPILE_GTK_PEER=no ;;
                *) COMPILE_GTK_PEER=yes ;;
              esac],
              [COMPILE_GTK_PEER=yes])
AM_CONDITIONAL(CREATE_GTK_PEER_LIBRARIES, test "x${COMPILE_GTK_PEER}" = xyes)

dnl -----------------------------------------------------------
dnl GTK native peer error checking 
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gtk-peers],,AC_MSG_ERROR([No --enable-gtk-peers (or --disable-gtk-peers) option; you want --enable-gtk-peer]))

dnl ------------------------------------------------------------
dnl determine whether to enable the cairo GTK Graphics2D backend
dnl ------------------------------------------------------------
AC_ARG_ENABLE([gtk-cairo],
              [AS_HELP_STRING(--enable-gtk-cairo,build the cairo Graphics2D implementation on GTK [default=no])],
              [case "${enableval}" in
                yes) GTK_CAIRO_ENABLED=true ;;
                no) GTK_CAIRO_ENABLED=false ;;
                *) GTK_CAIRO_ENABLED=true ;;
              esac],
              [GTK_CAIRO_ENABLED=false])
AC_SUBST(GTK_CAIRO_ENABLED)
if test "x${GTK_CAIRO_ENABLED}" = xtrue; then
  AC_DEFINE(GTK_CAIRO, 1, [defined if cairo support was built in])
fi
AM_CONDITIONAL(GTK_CAIRO, test "x${GTK_CAIRO_ENABLED}" = xtrue)

dnl -----------------------------------------------------------
dnl Regenerate headers at build time (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([regen-headers],
              [AS_HELP_STRING(--enable-regen-headers,automatically regenerate JNI headers [default=no])],
              [case "${enableval}" in
                yes) REGENERATE_JNI_HEADERS=yes ;;
                no) REGENERATE_JNI_HEADERS=no ;;
                *) REGENERATE_JNI_HEADERS=no ;;
              esac],
              [REGENERATE_JNI_HEADERS=no])
AM_CONDITIONAL(CREATE_JNI_HEADERS, test "x${REGENERATE_JNI_HEADERS}" = xyes)

AC_PROG_LN_S
AC_PROG_INSTALL

dnl -----------------------------------------------------------
dnl Checks for programs.
dnl -----------------------------------------------------------
dnl AC_PROG_CXX
dnl Initialize libtool
AC_DISABLE_STATIC
AC_PROG_LIBTOOL
dnl AC_PROG_AWK
AC_PROG_CC
AC_PROG_CPP

if test "x${COMPILE_JNI}" = xyes; then
  AC_HEADER_STDC

  dnl Checking sizeof void * is needed for fdlibm to work properly on ppc64, 
  dnl at least. 
  AC_COMPILE_CHECK_SIZEOF(void *)

  dnl Checking for endianess.
  AC_C_BIGENDIAN_CROSS
  
  dnl We check for sys/filio.h because Solaris 2.5 defines FIONREAD there.
  dnl On that system, sys/ioctl.h will not include sys/filio.h unless
  dnl BSD_COMP is defined; just including sys/filio.h is simpler.
  dnl Check for crt_externs.h on Darwin.
  AC_CHECK_HEADERS([unistd.h sys/types.h sys/config.h sys/ioctl.h \
		    asm/ioctls.h \
		    inttypes.h stdint.h utime.h sys/utime.h sys/filio.h \
		    sys/time.h \
		    sys/select.h \
		    crt_externs.h \
                    fcntl.h])

  AC_EGREP_HEADER(uint32_t, stdint.h, AC_DEFINE(HAVE_INT32_DEFINED, 1, [Define to 1 if you have uint32_t]))
  AC_EGREP_HEADER(uint32_t, inttypes.h, AC_DEFINE(HAVE_INT32_DEFINED, 1, [Define to 1 if you have uint32_t]))
  AC_EGREP_HEADER(u_int32_t, sys/types.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1, [Define to 1 if you have BSD u_int32_t]))
  AC_EGREP_HEADER(u_int32_t, sys/config.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1, [Define to 1 if you have BSD u_int32_t]))

  AC_CHECK_FUNCS([ftruncate fsync select \
		  gethostname socket strerror fork pipe execve open close \
		  lseek fstat read write htonl memset htons connect \
		  getsockname getpeername bind listen accept \
		  recvfrom send sendto setsockopt getsockopt time mktime \
		  localtime_r \
		  strerror_r \
                  fcntl])

  AC_HEADER_TIME
  AC_STRUCT_TM
  AC_STRUCT_TIMEZONE

  AC_MSG_CHECKING([for tm_gmtoff in struct tm])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]],[[struct tm tim; tim.tm_gmtoff = 0;]])],
  [AC_DEFINE(STRUCT_TM_HAS_GMTOFF, 1, [Define if struct tm has tm_gmtoff field.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)
   AC_MSG_CHECKING([for global timezone variable])
   dnl FIXME: we don't want a link check here because that won't work
   dnl when cross-compiling.  So instead we make an assumption that
   dnl the header file will mention timezone if it exists.
   dnl Don't find the win32 function timezone
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[void i(){long z2 = 2*timezone;}]])],
     [AC_DEFINE(HAVE_TIMEZONE, 1, [Define if global 'timezone' exists.])
      AC_MSG_RESULT(yes)],
     [AC_MSG_RESULT(no)
       AC_MSG_CHECKING([for global _timezone variable])
       dnl FIXME: As above, don't want link check
       AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[long z2 = _timezone;]])],
         [AC_DEFINE(HAVE_UNDERSCORE_TIMEZONE, 1,
            [Define if your platform has the global _timezone variable.])
          AC_MSG_RESULT(yes)],
          [AC_MSG_RESULT(no)])])])

  AC_C_CONST

  dnl See if we HAVE_ICONV, how ICONV_CONST is set and LTLIBICONV
  AM_ICONV

  dnl When using gcc we want warnings, lots of warnings :-)
  if test "x${GCC}" = xyes; then
    dnl We want ISO C90 pedantic ansi, but with longlong (jlong) support
    dnl and modern POSIX and BSD C library functions/prototypes.

    dnl Warning flags for (almost) everybody.
    dnl Should probably be configurable
    WARNING_CFLAGS='-pedantic -W -Wall -Wmissing-declarations -Wwrite-strings -Wmissing-prototypes -Wno-long-long'
    AC_SUBST(WARNING_CFLAGS)

    dnl Strict warning flags which not every module uses.
    dnl Should probably be configurable.
    STRICT_WARNING_CFLAGS=-Wstrict-prototypes
    AC_SUBST(STRICT_WARNING_CFLAGS)

    dnl Whether or not to add -Werror, also not used by all modueles.
    dnl Can be configured by --disable-Werror
    ERROR_CFLAGS=
    if test "x${ENABLE_WERROR}" = xyes; then
       ERROR_CFLAGS='-Werror'
    fi
    AC_SUBST(ERROR_CFLAGS)
  fi

  dnl Check for libxml and libxslt libraries (when xmlj is enabled).
  if test "x${COMPILE_XMLJ}" = xyes; then
    PKG_CHECK_MODULES(XML, libxml-2.0 >= 2.6.8)
    PKG_CHECK_MODULES(XSLT, libxslt >= 1.1.11)
    AC_SUBST(XML_LIBS)
    AC_SUBST(XML_CFLAGS)
    AC_SUBST(XSLT_LIBS)
    AC_SUBST(XSLT_CFLAGS)
  fi

  dnl Check for AWT related gthread/gtk
  if test "x${COMPILE_GTK_PEER}" = xyes; then
     AC_PATH_XTRA
     if test "$no_x" = yes; then
         AC_MSG_ERROR([GTK+ peers requested but no X library available])
     fi
     dnl We explicitly want the XTest Extension for Robot support.
     AC_CHECK_LIB([Xtst], [XTestQueryExtension], [true],
		  [AC_MSG_ERROR([libXtst NOT found, required for GdkRobot])],
		  [${X_LIBS}])
     PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4 gthread-2.0 >= 2.2 gdk-pixbuf-2.0)
     AC_SUBST(GTK_CFLAGS)
     AC_SUBST(GTK_LIBS)
  fi

  if test "x${enable_gtk_cairo}" = xyes; then
    PKG_CHECK_MODULES(CAIRO, cairo >= 0.5.0)
    PKG_CHECK_MODULES(PANGOFT2, pangoft2)
  fi
  AC_SUBST(CAIRO_LIBS)
  AC_SUBST(CAIRO_CFLAGS)
  AC_SUBST(PANGOFT2_LIBS)
  AC_SUBST(PANGOFT2_CFLAGS)
fi

CLASSPATH_WITH_JAVAH

dnl ----------------------------------------------------------- 
dnl Add the include files for the native abstraction layer.
dnl Used by AM_CPPFLAGS in the different modules.
dnl -----------------------------------------------------------
CLASSPATH_INCLUDES="-I\$(top_srcdir)/include -I\$(top_srcdir)/native/jni/classpath -I\$(top_srcdir)/native/target/Linux -I\$(top_srcdir)/native/target/generic"
AC_SUBST(CLASSPATH_INCLUDES)

dnl -----------------------------------------------------------
if test "x${COMPILE_JNI}" = xyes; then
  AC_MSG_CHECKING(jni_md.h support)
  if test -f ${srcdir}/include/jni_md-${target_cpu}-${target_os}.h; then
    AC_MSG_RESULT(yes)
  else
    target_cpu=x86
    target_os=linux-gnu 
    AC_MSG_WARN(no, using x86-linux-gnu)
  fi
  ac_sources="include/jni_md-${target_cpu}-${target_os}.h"
  ac_dests="include/jni_md.h"
  while test -n "$ac_sources"; do
    set $ac_dests; ac_dest=$1; shift; ac_dests=$*
    set $ac_sources; ac_source=$1; shift; ac_sources=$*
    ac_config_links_1="$ac_config_links_1 $ac_dest:$ac_source"
  done
  AC_CONFIG_LINKS([$ac_config_links_1])
fi

CLASSPATH_FIND_JAVAC

CLASSPATH_WITH_CLASSLIB

dnl -----------------------------------------------------------
dnl Initialize maintainer mode
dnl -----------------------------------------------------------
AM_MAINTAINER_MODE

dnl -----------------------------------------------------------
dnl Enable debugging statements at compile time.  By default
dnl these statements should be optimized out of the bytecode
dnl produced by an optimizing Java compiler and not hinder 
dnl performance because debugging is turned off by default.
dnl -----------------------------------------------------------
AC_ARG_ENABLE([debug],
              [AS_HELP_STRING(--enable-debug,enable runtime debugging code)],
              [case "${enableval}" in 
                yes) 
              	LIBDEBUG="true"
                AC_DEFINE(DEBUG, 1, [Define to 1 if you want native library runtime debugging code enabled]) 
              	;;
                no)   LIBDEBUG="false" ;;
                *) 	AC_MSG_ERROR(bad value ${enableval} for --enable-debug) ;;
              esac],
              [LIBDEBUG="false"])
AC_SUBST(LIBDEBUG)

dnl -----------------------------------------------------------
dnl Enable execution of all static initializer loadLibrary() 
dnl calls at compile time.  By default most people will not
dnl want to disable this, but some VMs (gcj) don't need or want 
dnl them.
dnl -----------------------------------------------------------
AC_ARG_ENABLE([load-library],
              [AS_HELP_STRING(--enable-load-library,enable to use JNI native methods [default=yes])],
              [case "${enableval}" in 
                yes) INIT_LOAD_LIBRARY="true" ;;
                no) INIT_LOAD_LIBRARY="false" ;;
                *) AC_MSG_ERROR(bad value ${enableval} for --enable-load-library) ;;
              esac],
              [INIT_LOAD_LIBRARY="true"])
AC_SUBST(INIT_LOAD_LIBRARY)


dnl -----------------------------------------------------------
dnl Should the VM explicitly run class initialization subfunctions for
dnl java.lang.System?   (default is false -- the subfunctions will be run 
dnl automatically by the class initializer)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([java-lang-system-explicit-initialization],
              [AS_HELP_STRING(--enable-java-lang-system-explicit-initialization,will the VM explicitly invoke java.lang.System's static initialization methods [default=no])],
              [case "${enableval}" in 
                yes|true) JAVA_LANG_SYSTEM_EXPLICIT_INITIALIZATION="true" ;;
                no|false) JAVA_LANG_SYSTEM_EXPLICIT_INITIALIZATION="false" ;;
                *) AC_MSG_ERROR(bad value ${enableval} for --enable-java-lang-system-explicit-initialization) ;;
              esac],
              [JAVA_LANG_SYSTEM_EXPLICIT_INITIALIZATION="false"])
AC_SUBST(JAVA_LANG_SYSTEM_EXPLICIT_INITIALIZATION)


dnl -----------------------------------------------------------
dnl avoiding automake complaints
dnl -----------------------------------------------------------
REMOVE=""
AC_SUBST(REMOVE)

dnl -----------------------------------------------------------
dnl This is probably useless.
dnl -----------------------------------------------------------
AC_PATH_PROG(SH, sh)
AC_PATH_PROG(MKDIR, mkdir)
AC_PATH_PROG(CP, cp)
AC_PATH_PROG(DATE, date)

dnl -----------------------------------------------------------
dnl According to the GNU coding guide, we shouldn't require find, 
dnl and zip, however GNU provides both so it should be okay 
dnl -----------------------------------------------------------
AC_PATH_PROG(FIND, find)

dnl -----------------------------------------------------------
dnl Specify what to install (install only glibj.zip by default)
dnl -----------------------------------------------------------
CLASSPATH_WITH_GLIBJ

dnl -----------------------------------------------------------
dnl Enable API documentation generation (disabled by default)
dnl -----------------------------------------------------------
CLASSPATH_WITH_GJDOC

dnl -----------------------------------------------------------
dnl Whether to use jay to regenerate parsers.
dnl -----------------------------------------------------------
REGEN_WITH_JAY

dnl -----------------------------------------------------------
dnl This sets the build-time default, which can now be overridden 
dnl by setting the system property gnu.classpath.awt.gtk.portable.native.sync
dnl to "true" or "false".
dnl -----------------------------------------------------------
AC_ARG_ENABLE([portable-native-sync],
              [AS_HELP_STRING(--enable-portable-native-sync,synchronize VM threads portably)],
              [case "${enableval}" in 
                yes) 
                     AC_DEFINE(PORTABLE_NATIVE_SYNC, 1, [Define if you want to synchronize VM threads portably by default; undef otherwise])
                     ;;
                no)  ;;
                *)   
                     AC_MSG_ERROR(bad value ${enableval} for --enable-portable-native-sync) 
                     ;;
              esac],
              [])

dnl -----------------------------------------------------------
dnl output files
dnl -----------------------------------------------------------
AC_CONFIG_FILES([Makefile
doc/Makefile
doc/api/Makefile
external/Makefile
external/sax/Makefile
external/w3c_dom/Makefile
gnu/classpath/Configuration.java
include/Makefile
native/Makefile
native/fdlibm/Makefile
native/jni/Makefile
native/jni/classpath/Makefile
native/jni/java-io/Makefile
native/jni/java-lang/Makefile
native/jni/java-net/Makefile
native/jni/java-nio/Makefile
native/jni/java-util/Makefile
native/jni/gtk-peer/Makefile
native/jni/xmlj/Makefile
native/target/Makefile
native/target/Linux/Makefile
native/target/generic/Makefile
resource/Makefile
scripts/Makefile
scripts/classpath.spec
lib/Makefile
lib/gen-classlist.sh
examples/Makefile])
AC_CONFIG_COMMANDS([gen-classlist],[chmod 755 lib/gen-classlist.sh])
AC_OUTPUT
