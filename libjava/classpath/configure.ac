dnl Process this file with autoconf to produce a configure script.

dnl -----------------------------------------------------------
dnl Turning off cache for debug reasons
dnl -----------------------------------------------------------
dnl define([AC_CACHE_LOAD], )dnl
dnl define([AC_CACHE_SAVE], )dnl

AC_INIT([GNU Classpath],[0.99.1-pre],[classpath@gnu.org],[classpath])
AC_CONFIG_SRCDIR(java/lang/System.java)
AC_CONFIG_MACRO_DIR([m4])

dnl GCJ LOCAL
AC_CONFIG_AUX_DIR(../..)
dnl END GCJ LOCAL

AC_CANONICAL_TARGET

dnl GCJ LOCAL
AC_ARG_ENABLE(java-maintainer-mode,
	AS_HELP_STRING([--enable-java-maintainer-mode],
	[allow rebuilding of .class and .h files]))
AM_CONDITIONAL(JAVA_MAINTAINER_MODE, test "$enable_java_maintainer_mode" = yes)
dnl END GCJ LOCAL

dnl GCJ LOCAL
# We would like to our source tree to be readonly.  However when releases or
# pre-releases are generated, the man pages need to be included as they are
# converted from the texi files via perl which we don't require end users to
# have installed.
# Therefore we have --enable-generated-files-in-srcdir to do just that.

AC_MSG_CHECKING([whether to place generated files in the source directory])
  dnl generated-files-in-srcdir is disabled by default
  AC_ARG_ENABLE(generated-files-in-srcdir, 
[  --enable-generated-files-in-srcdir
                          put copies of generated files in source dir
                          intended for creating source tarballs for users
                          without texinfo, perl, bison or flex.],
      generated_files_in_srcdir=$enableval,
      generated_files_in_srcdir=no)

AC_MSG_RESULT($generated_files_in_srcdir)
AM_CONDITIONAL(GENINSRC, test x$generated_files_in_srcdir = xyes)
dnl END GCJ LOCAL

# Find the rest of the source tree framework.
AM_ENABLE_MULTILIB(, ../..)

AM_CONDITIONAL(INSTALL_BINARIES, test -z "${with_multisubdir}")

dnl -----------------------------------------------------------
dnl Fold all IA-32 CPU architectures into "x86"
dnl -----------------------------------------------------------
if expr ${target_cpu} : '.*86' > /dev/null; then target_cpu=x86; fi

dnl -----------------------------------------------------------
dnl We will not track/change lib version until we reach version 1.0
dnl at which time we'll have to be more anal about such things
dnl -----------------------------------------------------------
AC_SUBST(LIBVERSION, "0:0:0")
case "$host_os" in
     darwin*)
	cp_module=""
	;;
	*)
	cp_module="-module"
	;;
esac

CLASSPATH_MODULE="${cp_module} -version-info ${LIBVERSION} -no-undefined"
AC_SUBST(CLASSPATH_MODULE)

CLASSPATH_CONVENIENCE="-no-undefined"
AC_SUBST(CLASSPATH_CONVENIENCE)

AC_PREREQ(2.64)
AM_INIT_AUTOMAKE([1.9.0 no-dist gnu std-options tar-ustar -Wno-portability])
AC_CONFIG_HEADERS([include/config.h])
AC_PREFIX_DEFAULT(/usr/local/classpath)

dnl GCC LOCAL
GCC_NO_EXECUTABLES

dnl -----------------------------------------------------------
dnl Enable collections.jar (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([collections],
              [AS_HELP_STRING(--enable-collections,create collections.jar [default=no])],
              [case x"${enableval}" in
                xyes) COMPILE_COLLECTIONS=yes; COLLECTIONS_PREFIX="\"gnu/java/util/collections\"" ;;
                xno) COMPILE_COLLECTIONS=no ;;
		x) COMPILE_COLLECTIONS=yes; COLLECTIONS_PREFIX="\"gnu/java/util/collections\"" ;;
                *) COMPILE_COLLECTIONS=yes; COLLECTIONS_PREFIX="\"${enableval}\"" ;;
              esac],
              [COMPILE_COLLECTIONS=no])
AM_CONDITIONAL(CREATE_COLLECTIONS, test "x${COMPILE_COLLECTIONS}" = xyes)

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
dnl Default Preference Backend
dnl -----------------------------------------------------------
AC_ARG_ENABLE([default-preferences-peer],
              [AS_HELP_STRING([--enable-default-preferences-peer@<:@=peer type or class name@:>@],
                              [specify one of: "gconf" [default] for a GConf based backend, "file" for a file based one, "memory" for a transient one, or a fully qualified class name implementing java.util.prefs.PreferencesFactory])],
              [case "${enableval}" in 
                file) DEFAULT_PREFS_PEER=gnu.java.util.prefs.FileBasedFactory  ;;
                gconf|yes|true) DEFAULT_PREFS_PEER=gnu.java.util.prefs.GConfBasedFactory  ;;
                memory) DEFAULT_PREFS_PEER=gnu.java.util.prefs.MemoryBasedFactory  ;;
                no|false) AC_MSG_ERROR(bad value '${enableval}' for --enable-default-preferences-peer) ;;
                *) DEFAULT_PREFS_PEER=${enableval} ;;
              esac],
              [DEFAULT_PREFS_PEER=gnu.java.util.prefs.GConfBasedFactory])
dnl AC_SUBST(DEFAULT_PREFS_PEER)

dnl -----------------------------------------------------------
dnl GConf native peer (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gconf-peer],
              [AS_HELP_STRING(--disable-gconf-peer,compile GConf native peers (disabled by --disable-jni) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_GCONF_PEER=yes ;;
                no) COMPILE_GCONF_PEER=no ;;
                *) COMPILE_GCONF_PEER=yes ;;
              esac],
              [COMPILE_GCONF_PEER=yes])
AM_CONDITIONAL(CREATE_GCONF_PEER_LIBRARIES, test "x${COMPILE_GCONF_PEER}" = xyes)

dnl -----------------------------------------------------------
dnl GConf native peer error checking
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gconf-peers],,AC_MSG_ERROR([No --enable-gconf-peers (or --disable-gconf-peers) option; you want --enable-gconf-peer]))

dnl ------------------------------------------------------------
dnl GStreamer based sound provider backend (disabled by default)
dnl ------------------------------------------------------------
AC_ARG_ENABLE([gstreamer-peer],
              [AS_HELP_STRING(--enable-gstreamer-peer,compile GStreamer native peers (disabled by --disable-jni) [default=no])],
              [case "${enableval}" in
                yes) COMPILE_GSTREAMER_PEER=yes ;;
                no) COMPILE_GSTREAMER_PEER=no ;;
                *) COMPILE_GSTREAMER_PEER=default ;;
              esac],
              [COMPILE_GSTREAMER_PEER=default])
AM_CONDITIONAL(CREATE_GSTREAMER_PEER_LIBRARIES, test "x${COMPILE_GSTREAMER_PEER}" = xyes)

dnl -----------------------------------------------------------
dnl GStreamer native peer error checking
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gstreamer-peers],,AC_MSG_ERROR([No --enable-gstreamer-peers (or --disable-gstreamer-peers) option; you want --enable-gstreamer-peer]))

dnl ------------------------------------------------------------
dnl Whether to compile with -Werror or not (disabled by default)
dnl ------------------------------------------------------------
AC_ARG_ENABLE([Werror],
              [AS_HELP_STRING(--enable-Werror,whether to compile C code with -Werror which turns any compiler warning into a compilation failure [default=no])],
              [case "${enableval}" in
                yes) ENABLE_WERROR=yes ;;
                no) ENABLE_WERROR=no ;;
                *) ENABLE_WERROR=no ;;
              esac],
              [ENABLE_WERROR=no])

dnl -----------------------------------------------------------
dnl Default AWT toolkit
dnl -----------------------------------------------------------
AC_ARG_ENABLE(default-toolkit,
  AS_HELP_STRING([--enable-default-toolkit],
                 [fully qualified class name of default AWT toolkit]))
default_toolkit=$enable_default_toolkit
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
dnl ALSA code (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([alsa],
              [AS_HELP_STRING(--disable-alsa,compile ALSA providers (enable by --enable-alsa) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_ALSA=yes ;;
                no) COMPILE_ALSA=no ;;
                *) COMPILE_ALSA=yes ;;
              esac],
	      [AC_CHECK_HEADERS([alsa/asoundlib.h],
				[AC_CHECK_LIB([asound], [snd_seq_open], COMPILE_ALSA=yes,COMPILE_ALSA=no)],
				COMPILE_ALSA=no)])
AM_CONDITIONAL(CREATE_ALSA_LIBRARIES, test "x${COMPILE_ALSA}" = xyes)

dnl -----------------------------------------------------------
dnl DSSI code (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([dssi],
              [AS_HELP_STRING(--disable-dssi,compile DSSI providers (enable by --enable-dssi) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_DSSI=yes ;;
                no) COMPILE_DSSI=no ;;
                *) COMPILE_DSSI=yes ;;
              esac],
              [COMPILE_DSSI=no
	       AC_CHECK_HEADERS([dssi.h], [
	         AC_CHECK_HEADERS([jack/jack.h],COMPILE_DSSI=yes)])])
AM_CONDITIONAL(CREATE_DSSI_LIBRARIES, test "x${COMPILE_DSSI}" = xyes)

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

dnl -----------------------------------------------------------
dnl Qt native peer (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([qt-peer],
              [AS_HELP_STRING(--enable-qt-peer,compile Qt4 native peers (disabled by --disable-jni) [default=no])],
              [case "${enableval}" in
                yes) COMPILE_QT_PEER=yes ;;
                no) COMPILE_QT_PEER=no ;;
                *) COMPILE_QT_PEER=yes ;;
              esac],
              [COMPILE_QT_PEER=no])
AM_CONDITIONAL(CREATE_QT_PEER_LIBRARIES, test "x${COMPILE_QT_PEER}" = xyes)

dnl -----------------------------------------------------------
dnl Plugin (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([plugin],
              [AS_HELP_STRING(--disable-plugin,compile gcjwebplugin (disabled by --disable-plugin) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_PLUGIN=yes ;;
                no) COMPILE_PLUGIN=no ;;
                *) COMPILE_PLUGIN=yes ;;
              esac],
              [COMPILE_PLUGIN=no])
AM_CONDITIONAL(CREATE_PLUGIN, test "x${COMPILE_PLUGIN}" = xyes)

dnl -----------------------------------------------------------
dnl Native java.math.BigInteger (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gmp],
              [AS_HELP_STRING(--enable-gmp,
                              compile native java.math.BigInteger library (disabled by --disable-gmp) [default=yes])],
              [case "${enableval}" in
                yes|true) COMPILE_GMP=yes ;;
                no|false) COMPILE_GMP=no ;;
                *) COMPILE_GMP=yes ;;
              esac],
              [COMPILE_GMP=yes])

dnl -----------------------------------------------------------
dnl GJDoc (enabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([gjdoc],
              [AS_HELP_STRING(--disable-gjdoc,compile GJDoc (disabled by --disable-gjdoc) [default=yes])],
              [case "${enableval}" in
                yes) COMPILE_GJDOC=yes ;;
                no) COMPILE_GJDOC=no ;;
                *) COMPILE_GJDOC=yes ;;
              esac],
              [COMPILE_GJDOC=yes])
AM_CONDITIONAL(CREATE_GJDOC, test "x${COMPILE_GJDOC}" = xyes)

dnl GCJ LOCAL: Calculates and substitutes toolexeclibdir.  $libdir is
dnl defined to the same value for all multilibs.  We define toolexeclibdir
dnl so that we can refer to the multilib installation directories from
dnl classpath's build files.
dnl -----------------------------------------------------------
case ${host_alias} in
  "") host_noncanonical=${build_noncanonical} ;;
  *) host_noncanonical=${host_alias} ;;
esac
case ${target_alias} in
  "") target_noncanonical=${host_noncanonical} ;;
  *) target_noncanonical=${target_alias} ;;
esac
AC_SUBST(target_noncanonical)

AC_ARG_ENABLE(version-specific-runtime-libs,
  AS_HELP_STRING([--enable-version-specific-runtime-libs],    
                 [specify that runtime libraries should be installed in a compiler-specific directory]),
    [case "$enableval" in
      yes) version_specific_libs=yes ;;
      no)  version_specific_libs=no ;;
      *)   AC_MSG_ERROR([Unknown argument to enable/disable version-specific libs]);;
     esac],
    [version_specific_libs=no]
)
CLASSPATH_TOOLEXECLIBDIR

dnl -----------------------------------------------------------
dnl Sets the native libraries installation dir
dnl -----------------------------------------------------------
dnl GCJ LOCAL: default to ${toolexeclibdir}/gcj-${gcc_version}-${libgcj_soversion}
AC_ARG_WITH([native-libdir],
	    [AS_HELP_STRING(--with-native-libdir,sets the installation directory for native libraries [default='${libdir}/${PACKAGE}'])],
	    [
	     nativeexeclibdir=${withval}
	    ],
	    [
	     nativeexeclibdir='${toolexeclibdir}/gcj-'`cat ${srcdir}/../../gcc/BASE-VER`-`awk -F: '/^[[^#]].*:/ { print $1 }' ${srcdir}/../libtool-version`
	    ])

AC_SUBST(nativeexeclibdir)

dnl -----------------------------------------------------------
dnl Sets the Java library installation dir.
dnl -----------------------------------------------------------
AC_ARG_WITH([glibj-dir],
	    [AS_HELP_STRING(--with-glibj-dir,sets the installation directory for glibj.zip [default='${libdir}/${PACKAGE}'])],
	    [
	     glibjdir=${withval}
	    ],
	    [
	     glibjdir='${datadir}/${PACKAGE}'
	    ])

AC_SUBST(glibjdir)

dnl -----------------------------------------------------------
dnl Sets the Antlr jar to use for compiling gjdoc
dnl -----------------------------------------------------------
AC_ARG_WITH([antlr-jar],
            [AS_HELP_STRING([--with-antlr-jar=file],[Use ANTLR from the specified jar file])],
            [
             ANTLR_JAR=$withval
            ],
            [
             ANTLR_JAR="$ANTLR_JAR"
            ])

dnl -----------------------------------------------------------
dnl Regenerate headers at build time (enabled if not found)
dnl -----------------------------------------------------------
AC_MSG_CHECKING([whether to regenerate the headers])
AC_ARG_ENABLE([regen-headers],
              [AS_HELP_STRING(--enable-regen-headers,automatically regenerate JNI headers [default=yes if headers don't exist])],
              [case "${enableval}" in
                yes) REGENERATE_JNI_HEADERS=yes ;;
                no) REGENERATE_JNI_HEADERS=no ;;
                *) REGENERATE_JNI_HEADERS=yes ;;
              esac],
              [if test -e ${srcdir}/include/java_lang_VMSystem.h; then
			REGENERATE_JNI_HEADERS=no ;
		   else
			REGENERATE_JNI_HEADERS=yes ;
	       fi])
AC_MSG_RESULT(${REGENERATE_JNI_HEADERS})
AM_CONDITIONAL(CREATE_JNI_HEADERS, test "x${REGENERATE_JNI_HEADERS}" = xyes)

dnl ------------------------------------------------------------------------
dnl Regenerate GJDoc parser at build time (enabled if not found)
dnl ------------------------------------------------------------------------
AC_MSG_CHECKING([whether to regenerate the GJDoc parser])
AC_ARG_ENABLE([regen-gjdoc-parser],
              [AS_HELP_STRING(--enable-regen-gjdoc-parser,automatically regenerate the GJDoc parser [default=yes if generated source doesn't exist])],
              [case "${enableval}" in
                yes) REGENERATE_GJDOC_PARSER=yes ;;
                no) REGENERATE_GJDOC_PARSER=no ;;
                *) REGENERATE_GJDOC_PARSER=yes ;;
              esac],
              [if test -e ${srcdir}/tools/generated/gnu/classpath/tools/gjdoc/expr/JavaLexer.java; then
			REGENERATE_GJDOC_PARSER=no ;
		   else
			REGENERATE_GJDOC_PARSER=yes ;
	       fi])
AC_MSG_RESULT(${REGENERATE_GJDOC_PARSER})
AM_CONDITIONAL(CREATE_GJDOC_PARSER, test "x${REGENERATE_GJDOC_PARSER}" = xyes)

dnl -----------------------------------------------------------
dnl Enable tool wrapper binaries (disabled by default)
dnl -----------------------------------------------------------
AC_ARG_ENABLE([tool-wrappers],
              [AS_HELP_STRING(--enable-tool-wrappers,create tool wrapper binaries [default=no])],
              [case x"${enableval}" in
                xyes)
                  COMPILE_WRAPPERS=yes;
                  AC_CHECK_HEADERS([ltdl.h],, [AC_MSG_ERROR(cannot find ltdl.h)])
                  AC_CHECK_LIB(ltdl, lt_dlopen,, [AC_MSG_ERROR(cannot find libltdl)])
                  ;;
                xno) COMPILE_WRAPPERS=no ;;
		x) COMPILE_WRAPPERS=yes ;;
                *) COMPILE_WRAPPERS=yes ;;
              esac],
              [COMPILE_WRAPPERS=no])
AM_CONDITIONAL(CREATE_WRAPPERS, test "x${COMPILE_WRAPPERS}" = xyes)

AC_PROG_LN_S
AC_PROG_INSTALL

dnl -----------------------------------------------------------
dnl Checks for programs.
dnl -----------------------------------------------------------

dnl Initialize libtool
AC_DISABLE_STATIC
LT_INIT
AC_PROG_AWK
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CPP
AC_PROG_CXX

# Handle -Werror default case.
if test "$ENABLE_WERROR" = default; then
  case "$host_os" in
    *linux*)
      if test "$GCC" = yes; then
        ENABLE_WERROR=yes
      fi
      ;;
  esac
fi

if test "x${COMPILE_COLLECTIONS}" = xyes; then
  AC_PATH_PROG(PERL, [perl])
  AC_SUBST(PERL)
  AC_SUBST(COLLECTIONS_PREFIX)
  AC_CONFIG_FILES([lib/mkcollections.pl])
  AC_CONFIG_COMMANDS([mkcollections.pl],[chmod 755 lib/mkcollections.pl])
fi

if test "x${COMPILE_JNI}" = xyes; then
  GCC_ATTRIBUTE_UNUSED

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
  dnl Check for netinet/in_systm.h, netinet/ip.h and net/if.h for Windows CE.
  dnl Check for sys/loadavg.h for getloadavg() on Solaris 9.
  dnl Check for sys/sockio.h for SIOCGIFFLAGS on OpenSolaris.
  AC_CHECK_HEADERS([unistd.h sys/types.h sys/config.h sys/ioctl.h \
		    asm/ioctls.h \
		    inttypes.h stdint.h utime.h sys/utime.h sys/filio.h \
		    sys/time.h \
		    sys/select.h \
		    crt_externs.h \
                    fcntl.h \
		    sys/mman.h \
		    magic.h \
                    sys/event.h sys/epoll.h \
		    ifaddrs.h \
		    netinet/in_systm.h netinet/ip.h net/if.h \
		    sys/loadavg.h sys/sockio.h])

  AC_EGREP_HEADER(uint32_t, stdint.h, AC_DEFINE(HAVE_INT32_DEFINED, 1, [Define to 1 if you have uint32_t]))
  AC_EGREP_HEADER(uint32_t, inttypes.h, AC_DEFINE(HAVE_INT32_DEFINED, 1, [Define to 1 if you have uint32_t]))
  AC_EGREP_HEADER(u_int32_t, sys/types.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1, [Define to 1 if you have BSD u_int32_t]))
  AC_EGREP_HEADER(u_int32_t, sys/config.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1, [Define to 1 if you have BSD u_int32_t]))

  AC_SEARCH_LIBS([clock_gettime], [rt])
  AC_SEARCH_LIBS([inet_pton],[nsl])
  AC_CHECK_LIB([socket], [gethostname]) 

  AC_CHECK_FUNCS([ftruncate fsync select \
		  gethostname socket strerror fork pipe execve open close \
		  lseek fstat read readv write writev htonl memset htons connect \
		  getsockname getpeername bind listen accept \
		  recvfrom send sendto setsockopt getsockopt time mktime clock_gettime \
		  gethostbyname_r localtime_r \
		  strerror_r \
                  fcntl \
		  statvfs \  
		  mmap munmap mincore msync madvise getpagesize sysconf \
		  lstat readlink \
 		  inet_aton inet_addr inet_pton \
		  getifaddrs kqueue kevent epoll_create \
                  getloadavg])

  LIBMAGIC=
  AC_CHECK_LIB(magic, magic_open, LIBMAGIC=-lmagic)
  AC_SUBST(LIBMAGIC)

  AC_MSG_CHECKING([whether struct sockaddr_in6 is in netinet/in.h])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>]], [[struct sockaddr_in6 addr6;]])],
                    [AC_DEFINE(HAVE_INET6, 1,
                     [Define if inet6 structures are defined in netinet/in.h.])
                     AC_MSG_RESULT(yes)],
                    [AC_MSG_RESULT(no)])

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
  AC_C_INLINE
  AC_C_ATTRIBUTE
  AX_FUNC_WHICH_GETHOSTBYNAME_R

  dnl See if we HAVE_ICONV, how ICONV_CONST is set and LTLIBICONV
  AM_ICONV

  dnl When using gcc we want warnings, lots of warnings :-)
  if test "x${GCC}" = xyes; then
    dnl We want ISO C90 ansi, but with longlong (jlong) support
    dnl and modern POSIX and BSD C library functions/prototypes.

    dnl Warning flags for (almost) everybody.
    dnl Should probably be configurable
    WARNING_CFLAGS='-W -Wall -Wmissing-declarations -Wwrite-strings -Wmissing-prototypes -Wno-long-long'
    AC_SUBST(WARNING_CFLAGS)

    dnl CFLAGS that are used for all native code.  We want to compile
    dnl everything with unwinder data so that backtrace() will always
    dnl work.
    EXTRA_CFLAGS='-fexceptions -fasynchronous-unwind-tables'
    AC_SUBST(EXTRA_CFLAGS)

    dnl Strict warning flags which not every module uses.
    dnl Should probably be configurable.
    STRICT_WARNING_CFLAGS='-Wstrict-prototypes -pedantic'
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
    dnl Check if we can link against the XTest library and set
    dnl HAVE_XTEST accordingly.
    AC_CHECK_LIB([Xtst], [XTestQueryExtension],
                 [AC_DEFINE(HAVE_XTEST, 1, [Define to 1 if you have libXtst.])[XTEST_LIBS="$XTEST_LIBS -X11 -lXtst"]],
                 [true],
                 [${X_LIBS}])


    PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.8 gthread-2.0 >= 2.2 gdk-pixbuf-2.0)
    PKG_CHECK_MODULES(FREETYPE2, freetype2)
    PKG_CHECK_MODULES(PANGOFT2, pangoft2)
    PKG_CHECK_MODULES(CAIRO, cairo >= 1.1.8)
    dnl Check if we can link against the XRender library and set
    dnl HAVE_XRENDER accordingly.
    AC_CHECK_LIB([Xrender], [XRenderQueryExtension],
		 [AC_DEFINE(HAVE_XRENDER, 1, [Define to 1 if you have libXrender.])[X_EXTRA_LIBS="$X_EXTRA_LIBS -lXrender"]],
		 [true],
		 [${X_LIBS}])

    dnl Check if we can link against the XRandR library and set
    dnl HAVE_XRANDR accordingly.
    AC_CHECK_LIB([Xrandr], [XRRQueryExtension],
                 [AC_DEFINE(HAVE_XRANDR, 1, [Define to 1 if you have libXrandr.])[X_EXTRA_LIBS="$X_EXTRA_LIBS -lXrandr"]],
                 [true],
                 [${X_LIBS}])

    AC_SUBST(GTK_CFLAGS)
    AC_SUBST(GTK_LIBS)
    AC_SUBST(FREETYPE2_LIBS)
    AC_SUBST(FREETYPE2_CFLAGS)
    AC_SUBST(PANGOFT2_LIBS)
    AC_SUBST(PANGOFT2_CFLAGS)
    AC_SUBST(XTEST_LIBS)
  fi

  dnl gconf-peer
  if  test "x${COMPILE_GCONF_PEER}" = xyes; then
    PKG_CHECK_MODULES(GCONF, gconf-2.0 >= 2.6.0)
    AC_SUBST(GCONF_CFLAGS)
    AC_SUBST(GCONF_LIBS)
    dnl we also need gdk for locking
    PKG_CHECK_MODULES(GDK, gdk-2.0 >= 2.8)
    AC_SUBST(GDK_CFLAGS)
    AC_SUBST(GDK_LIBS)
    dnl check if the config value was given form the command line,
    dnl if not, overwrite the default if we have the gconf backend
    dnl compiled in
    USE_GCONF_PREFS_PEER=$enable_default_preferences_peer
    if test "$USE_GCONF_PREFS_PEER" = ""; then
      DEFAULT_PREFS_PEER=gnu.java.util.prefs.GConfBasedFactory
    fi
  fi

  dnl gstreamer-peer
  if  test "x${COMPILE_GSTREAMER_PEER}" = xyes; then
    GST_MAJORMINOR=0.10
    GST_REQUIRED=0.10.10

    dnl gstreamer
    PKG_CHECK_MODULES(GSTREAMER, gstreamer-$GST_MAJORMINOR >= $GST_REQUIRED)
    AC_SUBST(GSTREAMER_CFLAGS)
    AC_SUBST(GSTREAMER_LIBS)

    dnl gstreamer-base
    PKG_CHECK_MODULES(GSTREAMER_BASE,
                      gstreamer-base-$GST_MAJORMINOR >= $GST_REQUIRED)
    AC_SUBST(GSTREAMER_BASE_CFLAGS)
    AC_SUBST(GSTREAMER_BASE_LIBS)

    dnl gstreamer-plugin-base
    PKG_CHECK_MODULES(GSTREAMER_PLUGINS_BASE,
                      gstreamer-plugins-base-$GST_MAJORMINOR >= $GST_REQUIRED)
    AC_SUBST(GSTREAMER_PLUGINS_BASE_CFLAGS)
    AC_SUBST(GSTREAMER_PLUGINS_BASE_LIBS)

	GST_PLUGIN_LDFLAGS='-module -avoid-version -Wno-unused-parameter -no-undefined'
	AC_SUBST(GST_PLUGIN_LDFLAGS)

    PKG_CHECK_MODULES(GDK, gdk-2.0 >= 2.8)
    AC_SUBST(GDK_CFLAGS)
    AC_SUBST(GDK_LIBS)
	
	dnl set the gstreamer based file reader, writer and mixer
	GSTREAMER_FILE_READER=gnu.javax.sound.sampled.gstreamer.io.GstAudioFileReader
	GSTREAMER_MIXER_PROVIDER=gnu.javax.sound.sampled.gstreamer.GStreamerMixerProvider
  fi
  dnl add the gstreamer resources 
  AC_SUBST(GSTREAMER_FILE_READER)
  AC_SUBST(GSTREAMER_MIXER_PROVIDER)

  dnl Check for AWT related Qt4
  if test "x${COMPILE_QT_PEER}" = xyes; then
    PKG_CHECK_MODULES(QT, QtCore QtGui >= 4.1.0, HAVE_QT4="yes", HAVE_QT4="no")
    if test "x$HAVE_QT4" = "xyes"; then
      dnl Check needed because in some cases the QtGui includedir
      dnl doesn't contain the subsystem dir.
      QT_INCLUDE_DIR=`$PKG_CONFIG --variable=includedir QtGui`
      EXTRA_QT_INCLUDE_DIR="$QT_INCLUDE_DIR/Qt"
      AC_CHECK_FILE([$QT_INCLUDE_DIR/QWidget],
      AC_MSG_NOTICE([No extra QT_INCLUDE_DIR needed]),
      AC_CHECK_FILE([$EXTRA_QT_INCLUDE_DIR/QWidget],
	QT_CFLAGS="$QT_CFLAGS -I$EXTRA_QT_INCLUDE_DIR",
	AC_MSG_WARN([QWidget not found])))
	AC_CHECK_PROG(MOC, [moc], [moc])
	AC_CHECK_PROG(MOC4, [moc-qt4], [moc-qt4])
	if test x"$MOC4" != x ; then
	  AC_MSG_NOTICE([Using $MOC4 as moc])
	  MOC=$MOC4;
	fi
    fi
    if test "x$HAVE_QT4" = "xno"; then
      AC_MSG_NOTICE([Looking for QT_CFLAGS and QT_LIBS without pkg-config])
      case "$host_os" in
  	darwin*)
  	  AC_ARG_WITH([qt4dir],
  	    [AS_HELP_STRING([--with-qt4dir=DIR],
  	    [Qt4 installation directory used for OS-X.
  	    For other systems use pkg-config.])],
  	    [QT4DIR=$withval]
  	    )
  	  if test x"$QT4DIR" = x ; then
  	    AC_MSG_ERROR([*** No path for Qt4 --with-qt4dir option given])
  	  fi
  	  AC_MSG_RESULT([QT4DIR... $QT4DIR])
  	  AC_CHECK_PROG(MOC, [moc], [$QT4DIR/bin/moc], [], $QT4DIR/bin)
  	  if test x"$MOC" = x; then
  	    AC_MSG_ERROR([*** This is not the right Qt installation])
  	  fi
  	  QT_CFLAGS="-F$QT4DIR/lib -I$QT4DIR/lib/QtCore.framework/Headers"
  	  QT_CFLAGS="$QT_CFLAGS -I$QT4DIR/lib/QtGui.framework/Headers"
  	  QT_LIBS="-Xlinker -F$QT4DIR/lib -Xlinker -framework -Xlinker QtCore"
  	  QT_LIBS="$QT_LIBS -Xlinker -framework -Xlinker QtGui"
  	  ;;
  	*)
  	  AC_MSG_ERROR([*** Please check PKG_CONFIG_PATH or the version
  	  of your installed Qt4 installation.])
  	  ;;
      esac
    fi
    AC_MSG_NOTICE([Set QT_CFLAGS... $QT_CFLAGS])
    AC_SUBST(QT_CFLAGS)
    AC_SUBST(QT_LIBS)
  fi
  dnl **********************************************************************
  dnl Check for MSG_NOSIGNAL
  dnl **********************************************************************
  AC_MSG_CHECKING(for MSG_NOSIGNAL)
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[ int f = MSG_NOSIGNAL; ]])],[ AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_MSG_NOSIGNAL, 1,
     	       [Define this symbol if you have MSG_NOSIGNAL]) ],[ AC_MSG_RESULT(no)
  ])
 dnl **********************************************************************
 dnl Check for SO_NOSIGPIPE (Darwin equivalent for MSG_NOSIGNAL)
 dnl **********************************************************************
 AC_MSG_CHECKING(for SO_NOSIGPIPE )
 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[ int f = SO_NOSIGPIPE; ]])],[ AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_SO_NOSIGPIPE, 1,
    	      [Define this symbol if you have SO_NOSIGPIPE]) ],[ AC_MSG_RESULT(no)
 ])
  dnl **********************************************************************
  dnl Check for MSG_WAITALL
  dnl **********************************************************************
  AC_MSG_CHECKING(for MSG_WAITALL)
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/socket.h>]], [[ int f = MSG_WAITALL; ]])],[ AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_MSG_WAITALL, 1,
     	       [Define this symbol if you have MSG_WAITALL]) ],[ AC_MSG_RESULT(no)
  ])

  dnl Check for plugin support headers and libraries.
  if test "x${COMPILE_PLUGIN}" = xyes; then
    PKG_CHECK_MODULES(MOZILLA, mozilla-plugin libxul-unstable, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, mozilla-plugin, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, firefox-plugin firefox-xpcom, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, xulrunner-plugin xulrunner-xpcom, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, mozilla-firefox-plugin mozilla-firefox-xpcom, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, seamonkey-plugin seamonkey-xpcom, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      PKG_CHECK_MODULES(MOZILLA, iceape-plugin iceape-xpcom, [MOZILLA_FOUND=yes], [MOZILLA_FOUND=no])
    fi
    if test "x${MOZILLA_FOUND}" = xno; then
      AC_MSG_ERROR([Couldn't find plugin support headers and libraries, try --disable-plugin])
    fi

    PKG_CHECK_MODULES(GLIB, glib-2.0)
    PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.8 gthread-2.0 >= 2.2 gdk-pixbuf-2.0)

    AC_SUBST(MOZILLA_CFLAGS)
    AC_SUBST(MOZILLA_LIBS)
    AC_SUBST(GLIB_CFLAGS)
    AC_SUBST(GLIB_LIBS)
    AC_SUBST(GTK_CFLAGS)
    AC_SUBST(GTK_LIBS)

    AC_SUBST(PLUGIN_DIR, $HOME/.mozilla/plugins/)
  fi

  dnl Check for GNU MP library and header file
  dnl for GNU MP versions >= 4.2 use __gmpz_combit; otherwise look for
  dnl __gmpz_mul_si for earlier versions (>= 3.1).
  dnl IMPORTANT: if you decide to look for __gmpz_combit, don't forget to
  dnl change the name of the corresponding ac_ variable on lines 860...
  AC_ARG_WITH(gmp, [  --with-gmp=PATH         specify prefix directory for the installed GMP package.
                          Equivalent to --with-gmp-include=PATH/include
                          plus --with-gmp-lib=PATH/lib])
  AC_ARG_WITH(gmp_include, [  --with-gmp-include=PATH specify directory for installed GMP include files])
  AC_ARG_WITH(gmp_lib, [  --with-gmp-lib=PATH     specify directory for the installed GMP library])

  if test "x$with_gmp" != x; then
    gmplibs=-lgmp
    gmpinc=
    gmplibs="-L$with_gmp/lib $gmplibs"
    gmpinc="-I$with_gmp/include $gmpinc"
    if test "x$with_gmp_include" != x; then
      gmpinc="-I$with_gmp_include $gmpinc"
    fi
    if test "x$with_gmp_lib" != x; then
      gmplibs="-L$with_gmp_lib $gmplibs"
    fi
  else
    gmplibs="-lgmp"
    gmpinc="-I/usr/include"
  fi

  if test "x${COMPILE_GMP}" = xyes; then
    AC_MSG_CHECKING([for GMP directories])
    if test "x$with_gmp" != x; then
       gmplibs="-L$with_gmp/lib -lgmp"
       gmpinc="-I$with_gmp/include"
       if test "x$with_gmp_include" != x; then
       	  gmpinc="-I$with_gmp_include $gmpinc"
       fi
       if test "x$with_gmp_lib" != x; then
       	  gmplibs="-L$with_gmp_lib $gmplibs"
       fi
    else
        with_gmp="/usr"
	gmplibs="-lgmp"
    	gmpinc="-I/usr/include"
    fi
    AC_MSG_RESULT([prefix=$with_gmp, libs=$gmplibs, inc=$gmpinc])
    AC_CHECK_LIB(gmp, __gmpz_mul_si,
      [GMP_CFLAGS="$gmpinc"
       GMP_LIBS="$gmplibs" ],
      [GMP_CFLAGS=
       GMP_LIBS= ])
    AC_SUBST(GMP_CFLAGS)
    AC_SUBST(GMP_LIBS)

    AC_CHECK_HEADERS([gmp.h])
  fi

else
  COMPILE_GMP=no
fi

if test "x${REGENERATE_JNI_HEADERS}" = xyes; then
  CLASSPATH_WITH_JAVAH
fi

dnl ----------------------------------------------------------- 
dnl Add the include files for the native abstraction layer.
dnl Used by AM_CPPFLAGS in the different modules.
dnl -----------------------------------------------------------
CLASSPATH_INCLUDES="-I\$(top_srcdir)/include -I\$(top_srcdir)/native/jni/classpath -I\$(top_srcdir)/native/jni/native-lib"
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
dnl Specify the jar file containing the Eclipse Java Compiler.  If
dnl this option is not specified then the com.sun.tools.javac
dnl implementation will not be included in tools.zip.
dnl -----------------------------------------------------------
dnl GCJ LOCAL: default to $multi_basedir/ecj.jar
dnl In the default case where $multi_basedir/ecj.jar is found
dnl $(jardir)/ecj.jar will not expand properly in GNU Classpath's
dnl gnu/classpath/Configuration.java.  That is OK though since libjava's
dnl gnu/classpath/Configuration.java overrides GNU Classpath's and
dnl ECJ_JAR_FILE will be properly expanded in libjava's
dnl gnu/classpath/natConfiguration.cc.
AC_ARG_WITH([ecj-jar],
  	    [AS_HELP_STRING([--with-ecj-jar=ABS.PATH],
  	    [specify jar file containing the Eclipse Java Compiler])],
  	    [ECJ_JAR=$withval],
	    [if test -f $multi_basedir/ecj.jar; then
	       ECJ_JAR='$(jardir)/ecj.jar'
	     fi])
AC_SUBST(ECJ_JAR)

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

AX_CREATE_STDINT_H([include/config-int.h])

dnl -----------------------------------------------------------------------
dnl                    Support for using a prebuilt class library
dnl -----------------------------------------------------------------------
AC_ARG_WITH([glibj_zip],
            AS_HELP_STRING([--with-glibj-zip=ABS.PATH],
                           [use prebuilt glibj.zip class library]))

case "$with_glibj_zip" in
"")
        use_glibj_zip=false
        ;;
"no" )
        use_glibj_zip=false
        ;;
"yes")
        AC_MSG_ERROR([Please suply an absolute path to a prebuilt glibj.zip])
        ;;
*)
        use_glibj_zip=true
        PATH_TO_GLIBJ_ZIP=$with_glibj_zip
	;;
esac;

AM_CONDITIONAL(USE_PREBUILT_GLIBJ_ZIP, test x$use_glibj_zip = xtrue)
AC_SUBST(PATH_TO_GLIBJ_ZIP)

if test "x${TOOLSDIR}" != x; then
  dnl GCJ LOCAL
  if test "x${COMPILE_WRAPPERS}" = xno && test "x${enable_java_maintainer_mode}" = xyes; then
    AC_PROG_JAVA_WORKS
  fi
  dnl END GCJ LOCAL
  if test "x${COMPILE_GJDOC}" = xyes; then
    AC_LIB_ANTLR
    dnl GCJ LOCAL
    if test "x${REGENERATE_GJDOC_PARSER}" = xyes && test "x${enable_java_maintainer_mode}" = xyes; then
    dnl END GCJ LOCAL
	AC_PROG_ANTLR(2,7,1)
    fi
  fi
fi


# Check for javac if we need to build either the class library,
# the examples or the tools
if test "x${use_glibj_zip}" = xfalse || \
   test "x${EXAMPLESDIR}" != x || \
   test "x${TOOLSDIR}" != x && \
   test "x${build_class_files}" != xno && \
   test "${enable_java_maintainer_mode}" = yes; then dnl GCJ_LOCAL
	AC_PROG_JAVAC_WORKS
  	CLASSPATH_JAVAC_MEM_CHECK
else
   AM_CONDITIONAL(GCJ_JAVAC, false)
fi

dnl -----------------------------------------------------------
dnl Build with Escher based X peers.
dnl -----------------------------------------------------------
AC_ARG_WITH([escher],
	 AS_HELP_STRING([--with-escher=ABS.PATH],
	                [specify path to escher dir or JAR for X peers]))
case "$with_escher" in
"")
        use_escher=false
        ;;
"no")
        use_escher=false
        ;;
"yes")
        AC_MSG_ERROR([Please supply an absolute path to Escher library])
        ;;
*)
        use_escher=true
        PATH_TO_ESCHER=$with_escher
        ;;
esac

AM_CONDITIONAL(USE_ESCHER, test x$use_escher = xtrue)
AC_SUBST(PATH_TO_ESCHER)

dnl -----------------------------------------------------------
dnl Check if local socket support should be included.
dnl -----------------------------------------------------------
AC_ARG_ENABLE([local-sockets],
              [AS_HELP_STRING(--enable-local-sockets,enables local (AF_LOCAL) socket API [default: no])],
              [case "${enableval}" in
                yes)
                  ENABLE_LOCAL_SOCKETS=yes
                  ;;
                *)
                  ENABLE_LOCAL_SOCKETS=no
                  ;;
               esac],
              [])
if test "x$ENABLE_LOCAL_SOCKETS" = "xyes"
then
  AC_CHECK_HEADER([sys/un.h])
  AC_CHECK_FUNCS([read write bind listen accept shutdown], [],
                 AC_MSG_ERROR([networking support not available]))
  AC_DEFINE(ENABLE_LOCAL_SOCKETS, [1], [Define to enable support for local sockets.])
fi
AM_CONDITIONAL(ENABLE_LOCAL_SOCKETS, test "x$ENABLE_LOCAL_SOCKETS" = "xyes")

dnl -----------------------------------------------------------
dnl Add the default preference peer
dnl -----------------------------------------------------------
AC_SUBST(DEFAULT_PREFS_PEER)

dnl -----------------------------------------------------------
dnl Set GNU MP related params
dnl -----------------------------------------------------------
WANT_NATIVE_BIG_INTEGER=false
if test "x${COMPILE_GMP}" = xyes; then
  if test "x${ac_cv_lib_gmp___gmpz_mul_si}" = xyes; then
    if test "x${ac_cv_header_gmp_h}" = xyes; then
      WANT_NATIVE_BIG_INTEGER=true
      AC_DEFINE(WITH_GNU_MP, 1, [Define to 1 if gmp is usable])
    else
      COMPILE_GMP=no
    fi
  else
    COMPILE_GMP=no
  fi
fi
AC_SUBST(WANT_NATIVE_BIG_INTEGER)
AM_CONDITIONAL(CREATE_GMPBI_LIBRARY, test "x${COMPILE_GMP}" = xyes)

dnl -----------------------------------------------------------
dnl output files
dnl -----------------------------------------------------------
AC_CONFIG_FILES([Makefile
doc/Makefile
doc/api/Makefile
external/Makefile
external/sax/Makefile
external/w3c_dom/Makefile
external/relaxngDatatype/Makefile
external/jsr166/Makefile
gnu/classpath/Configuration.java
gnu/java/security/Configuration.java
include/Makefile
native/Makefile
native/fdlibm/Makefile
native/jawt/Makefile
native/jni/Makefile
native/jni/classpath/Makefile
native/jni/java-io/Makefile
native/jni/java-lang/Makefile
native/jni/java-math/Makefile
native/jni/java-net/Makefile
native/jni/java-nio/Makefile
native/jni/java-util/Makefile
native/jni/gtk-peer/Makefile
native/jni/gconf-peer/Makefile
native/jni/gstreamer-peer/Makefile
native/jni/qt-peer/Makefile
native/jni/xmlj/Makefile
native/jni/midi-alsa/Makefile
native/jni/midi-dssi/Makefile
native/jni/native-lib/Makefile
native/plugin/Makefile
resource/Makefile
resource/META-INF/services/java.util.prefs.PreferencesFactory
resource/META-INF/services/javax.sound.sampled.spi.AudioFileReader
resource/META-INF/services/javax.sound.sampled.spi.MixerProvider
scripts/Makefile
scripts/classpath.spec
lib/Makefile
lib/gen-classlist.sh
lib/copy-vmresources.sh
scripts/check_jni_methods.sh
tools/Makefile
examples/Makefile
examples/Makefile.jawt
examples/Makefile.java2d])

CLASSPATH_COND_IF([CREATE_WRAPPERS], [test "x${COMPILE_WRAPPERS}" = xyes], [],
[AC_CONFIG_FILES([tools/gappletviewer
tools/gjarsigner
tools/gkeytool
tools/gjar
tools/gnative2ascii
tools/gserialver
tools/grmiregistry
tools/gtnameserv
tools/gorbd
tools/grmid
tools/grmic
tools/gjavah])

AC_CONFIG_COMMANDS([gappletviewer],[chmod 755 tools/gappletviewer])
AC_CONFIG_COMMANDS([gjarsigner],[chmod 755 tools/gjarsigner])
AC_CONFIG_COMMANDS([gkeytool],[chmod 755 tools/gkeytool])
AC_CONFIG_COMMANDS([gjar],[chmod 755 tools/gjar])
AC_CONFIG_COMMANDS([gnative2ascii],[chmod 755 tools/gnative2ascii])
AC_CONFIG_COMMANDS([gserialver],[chmod 755 tools/gserialver])
AC_CONFIG_COMMANDS([grmiregistry],[chmod 755 tools/grmiregistry])
AC_CONFIG_COMMANDS([gtnameserv],[chmod 755 tools/gtnameserv])
AC_CONFIG_COMMANDS([gorbd],[chmod 755 tools/gorbd])
AC_CONFIG_COMMANDS([grmid],[chmod 755 tools/grmid])
AC_CONFIG_COMMANDS([grmic],[chmod 755 tools/grmic])
AC_CONFIG_COMMANDS([gjavah], [chmod 755 tools/gjavah])
])

if test "x${COMPILE_GJDOC}" = xyes
then
AC_CONFIG_FILES([tools/gjdoc])
AC_CONFIG_COMMANDS([gjdoc], [chmod 755 tools/gjdoc])
fi

AC_CONFIG_COMMANDS([gen-classlist],[chmod 755 lib/gen-classlist.sh])
AC_CONFIG_COMMANDS([copy-vmresources],[chmod 755 lib/copy-vmresources.sh])
AC_OUTPUT

# Create standard.omit based on decisions we just made.
cat ${srcdir}/lib/standard.omit.in > lib/standard.omit
if test x$use_escher != xtrue; then
   echo gnu/java/awt/peer/x/.*java$ >> lib/standard.omit
fi

if test "x${COMPILE_PLUGIN}" = "xyes" ; then
  AC_MSG_WARN([You have enabled the browser plugin.  Please note that this is no longer maintained as part of GNU Classpath; development has moved to the IcedTea project.])
fi
