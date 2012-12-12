dnl # Process this with autoconf to create configure
AC_PREREQ(2.64)
# Still use "libjava" here to placate dejagnu.
AC_INIT([libjava], [version-unused],, [libjava])

AC_CONFIG_SRCDIR(java/lang/System.java)

GCC_TOPLEV_SUBDIRS

# We use these options to decide which functions to include.
AC_ARG_WITH(target-subdir,
  AS_HELP_STRING([--with-target-subdir=SUBDIR],
                 [configure in a subdirectory]))

# We may get other options which we don't document:
# --with-target-subdir, --with-multisrctop, --with-multisubdir

# Find the rest of the source tree framework.
AM_ENABLE_MULTILIB(, ..)

AC_CANONICAL_SYSTEM
_GCC_TOPLEV_NONCANONICAL_BUILD
_GCC_TOPLEV_NONCANONICAL_TARGET

AC_SUBST(target_noncanonical)

GCC_LIBSTDCXX_RAW_CXX_FLAGS

# This works around the fact that libtool configuration may change LD
# for this particular configuration, but some shells, instead of
# keeping the changes in LD private, export them just because LD is
# exported.
ORIGINAL_LD_FOR_MULTILIBS=$LD

AC_PROG_LN_S

# This works around an automake problem.
mkinstalldirs="`cd $ac_aux_dir && ${PWDCMD-pwd}`/mkinstalldirs"
AC_SUBST(mkinstalldirs)

AC_ARG_WITH(cross-host,
  AS_HELP_STRING([--with-cross-host=HOST],
                 [configure with a cross compiler from HOST]))

AC_ARG_WITH(newlib,
  AS_HELP_STRING([--with-newlib],
                 [configure with newlib]))

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

AC_ARG_ENABLE(browser-plugin,
  AS_HELP_STRING([--enable-browser-plugin],
                 [build gcjwebplugin web browser plugin]),
    [case "$enableval" in
      yes) browser_plugin_enabled=yes ;;
      no)  browser_plugin_enabled=no ;;
      *)   AC_MSG_ERROR([Unknown argument to enable/disable browser plugin]);;
     esac],
    [browser_plugin_enabled=no]
)

AC_ARG_ENABLE(gconf-peer,
  AS_HELP_STRING([--enable-gconf-peer],
                 [compile GConf native peers for util.preferences]),
    [case "$enableval" in
      yes) gconf_enabled=yes ;;
      no)  gconf_enabled=no ;;
      *)   AC_MSG_ERROR([Unknown argument to enable/disable gconf-peer]);;
     esac],
    [gconf_enabled=no]
)

AC_ARG_WITH([antlr-jar],
            [AS_HELP_STRING([--with-antlr-jar=file],[Use ANTLR from the specified jar file])],
            [
             ANTLR_JAR=$withval
            ],
            [
             ANTLR_JAR=
            ])

AC_MSG_CHECKING([for antlr.jar])
if test "x$ANTLR_JAR" = x; then
    for antlr_lib_home in `ls -d /usr/local/share/antlr* 2> /dev/null` \
	/usr/share/antlr/lib /usr/share/java /usr/lib;
    do
	if test -f "$antlr_lib_home/antlr.jar"; then
	    # FIXME: version check for antlr needed without using a VM
	    ANTLR_JAR="$antlr_lib_home/antlr.jar"
	    break
	fi
    done
    if test "x$ANTLR_JAR" = x; then
	AC_MSG_RESULT([not found])
    else
	AC_MSG_RESULT($ANTLR_JAR)
    fi
else
    if test -f "${ANTLR_JAR}"; then
	AC_MSG_RESULT($ANTLR_JAR)
    else
	AC_MSG_ERROR([antlr jar file not found: $ANTLR_JAR])
    fi
fi
AC_SUBST(ANTLR_JAR)

AC_ARG_ENABLE([gjdoc],
              [AS_HELP_STRING(--disable-gjdoc,compile GJDoc (disabled by --disable-gjdoc) [default=yes])],
              [case "${enableval}" in
                yes) BUILD_GJDOC=yes ;;
                no) BUILD_GJDOC=no ;;
                *) BUILD_GJDOC=yes ;;
              esac],
              [BUILD_GJDOC=maybe])

AC_MSG_CHECKING([whether to build gjdoc])
if test "x$BUILD_GJDOC" = xmaybe; then
    if test -f "${ANTLR_JAR}"; then
	BUILD_GJDOC=yes
	AC_MSG_RESULT(yes)
    else
	BUILD_GJDOC=no
	AC_MSG_RESULT([no, antlr.jar not found])
    fi
else
    AC_MSG_RESULT($BUILD_GJDOC)
fi
AM_CONDITIONAL(CREATE_GJDOC, test "x${BUILD_GJDOC}" = xyes)

AC_ARG_ENABLE(java-maintainer-mode,
	AS_HELP_STRING([--enable-java-maintainer-mode],
	[allow rebuilding of .class and .h files]))
AM_CONDITIONAL(JAVA_MAINTAINER_MODE, test "$enable_java_maintainer_mode" = yes)

# It may not be safe to run linking tests in AC_PROG_CC/AC_PROG_CXX.
GCC_NO_EXECUTABLES

# For libstdc++-v3, -fno-builtin must be present here so that a
# non-conflicting form of std::exit can be guessed by AC_PROG_CXX, and
# used in later tests.  This may not be necessary in libjava; I don't know.
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -fno-builtin"
AC_PROG_CC
AC_PROG_CXX
CXXFLAGS="$save_CXXFLAGS"

AM_INIT_AUTOMAKE([no-dist 1.9.0])

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(LD, ld)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(DLLTOOL, dlltool, :)
AC_PROG_AWK
AC_CHECK_PROGS([JAR], [jar fastjar gjar], no)
AC_PATH_PROG([ZIP], [zip], no)
AC_PATH_PROG([UNZIP], [unzip], unzip)

# We need a jar that supports -@.  This is a GNU extension.
if test "$JAR" != no; then
   rm -f config-test.jar
   echo $srcdir/configure.ac | $JAR -cf@ config-test.jar >/dev/null 2>&1
   if test -f config-test.jar; then
     rm -f config-test.jar
   else
     JAR=no
   fi
fi

# Prefer the jar we found, but fall back to our jar script.
if test "$JAR" = no; then
  if test "$ZIP" = no; then
    AC_MSG_ERROR([cannot find neither zip nor jar, cannot continue])
  else
    # InfoZIP available, use the 'guaranteed' Bourne-shell JAR to build libjava
    JAR=`pwd`/scripts/jar
  fi
fi

AC_PROG_INSTALL

AM_MAINTAINER_MODE

AC_EXEEXT

# configure.host sets the following important variables
#	libgcj_cflags    - host specific C compiler flags
#	libgcj_cxxflags  - host specific C++ compiler flags
#	libgcj_javaflags - host specific Java compiler flags
#	libgcj_sublib_ltflags - host specific Libtool flags 
#	libgcj_sublib_core_extra_deps - host specific extra 
#			   dependencies for core sublib
#	(these last two only used when building sublibs)
# and a number of others; see the list at the start of the file.

libgcj_cflags=
libgcj_cxxflags=
libgcj_javaflags=
libgcj_sublib_ltflags=
libgcj_sublib_core_extra_deps=

. ${srcdir}/configure.host

ACX_PROG_LD_GNU_SYMBOLIC
libgcj_ld_symbolic_functions=$SYMBOLIC_LDFLAGS
if test -z "$libgcj_ld_symbolic"; then
  libgcj_ld_symbolic=$SYMBOLIC_LDFLAGS
fi

# for windows native targets enforce export of all symbols for shared libgcj.
libgcj_ld_export_all=
case "$host" in
      *mingw*)
	libgcj_ld_export_all=-Wl,--export-all-symbols
	;;
      *)
        ;;
esac

LIBGCJ_CFLAGS="${libgcj_cflags}"
LIBGCJ_CXXFLAGS="${libgcj_cxxflags}"
LIBGCJ_JAVAFLAGS="${libgcj_javaflags}"
LIBGCJ_SUBLIB_LTFLAGS="${libgcj_sublib_ltflags}"
LIBGCJ_SUBLIB_CORE_EXTRA_DEPS="${libgcj_sublib_core_extra_deps}"
LIBGCJ_LD_SYMBOLIC="${libgcj_ld_symbolic}"
LIBGCJ_LD_SYMBOLIC_FUNCTIONS="${libgcj_ld_symbolic_functions}"
LIBGCJ_LD_EXPORT_ALL="${libgcj_ld_export_all}"
AC_SUBST(LIBGCJ_CFLAGS)
AC_SUBST(LIBGCJ_CXXFLAGS)
AC_SUBST(LIBGCJ_JAVAFLAGS)
AC_SUBST(LIBGCJ_SUBLIB_LTFLAGS)
AC_SUBST(LIBGCJ_SUBLIB_CORE_EXTRA_DEPS)
AC_SUBST(LIBGCJ_LD_SYMBOLIC)
AC_SUBST(LIBGCJ_LD_SYMBOLIC_FUNCTIONS)
AC_SUBST(LIBGCJ_LD_EXPORT_ALL)

# Only use libltdl for non-newlib builds.
if test "x${with_newlib}" = "x" || test "x${with_newlib}" = "xno"; then
   AC_LTDL_SHLIBPATH
fi

AC_CONFIG_HEADERS([include/config.h gcj/libgcj-config.h])

# Possibly build libgcj as many sub-libraries.
AC_ARG_ENABLE(libgcj-sublibs,
  AS_HELP_STRING([--enable-libgcj-sublibs],
                 [build libgcj as many sub-libraries]))

if test -z "$enable_libgcj_sublibs"; then
   enable_libgcj_sublibs=$enable_libgcj_sublibs_default
fi
AM_CONDITIONAL(BUILD_SUBLIBS, test "$enable_libgcj_sublibs" = yes)
if test "$enable_libgcj_sublibs" = yes ; then
  # In theory we could make do with only one override and simply
  # tag "_bc" onto the end of it when we use it to generate the
  # spec, but that's an ugly thing to do when there are multiple
  # words in the string and you're relying on the ordering to
  # append the correct one.
  libgcj_spec_lgcj_override="-lgcj-noncore -lgcj"
  libgcj_spec_lgcj_bc_override="-lgcj-noncore -lgcj_bc"
fi


# See if the user has requested runtime debugging.
LIBGCJDEBUG="disable"
AC_SUBST(LIBGCJDEBUG)
AC_ARG_ENABLE(libgcj-debug,
  AS_HELP_STRING([--enable-libgcj-debug],
                 [enable runtime debugging code]),
  [if test "$enable_libgcj_debug" = yes; then
    AC_DEFINE(__GCJ_DEBUG, 1, [Define this if you want runtime debugging enabled.])
    LIBGCJDEBUG="enable"
  fi])

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
        ;;
esac

# Determine which AWT peer libraries to build
AC_ARG_ENABLE(java-awt,
  AS_HELP_STRING([--enable-java-awt],
                 [list of AWT peer implementations to be built]))

peerlibs="`echo ${enable_java_awt} | tr ',' ' '`"
use_xlib_awt=""
use_gtk_awt=""
use_qt_awt=""
use_x_awt=""
# The default toolkit to use is the first one specified.
TOOLKIT=
AC_SUBST(TOOLKIT)

for peer in $peerlibs ; do
  case $peer in
    xlib)
      if test "$no_x" = yes; then
        echo "*** xlib peers requested but no X library available" 1>&2
        exit 1
      else
        use_xlib_awt="yes"
	if test -z "$TOOLKIT"; then
	   TOOLKIT=gnu.awt.xlib.XToolkit
	fi
      fi
      ;;
    gtk)
      if test "$no_x" = yes; then
	 echo "*** gtk peers requested but no X library available" 1>&2
	 exit 1
      else
	 use_gtk_awt=yes
	 if test -z "$TOOLKIT"; then
	    TOOLKIT=gnu.java.awt.peer.gtk.GtkToolkit
	 fi
      fi
      ;;
    qt)
      if test "$no_x" = yes; then
         # Perhaps we should admit the possibility of embedded Qt.
	 echo "*** Qt peers requested but no X library available" 1>&2
	 exit 1
      else
	 use_qt_awt=yes
	 if test -z "$TOOLKIT"; then
	    TOOLKIT=gnu.java.awt.peer.qt.QtToolkit
	 fi
      fi
      ;;
    x)
      use_x_awt=yes
      if test -z "$TOOLKIT"; then
	 TOOLKIT=gnu.java.awt.peer.x.XToolkit
      fi
      ;;
    no)
      use_xlib_awt=
      use_gtk_awt=
      use_qt_awt=
      use_x_awt=
      break
      ;;
    *)
      echo "*** unrecognised argument \"${peer}\" for --enable-java-awt" 1>&2
      exit 1
  esac
done

AM_CONDITIONAL(XLIB_AWT, test "$use_xlib_awt" = yes)
AM_CONDITIONAL(X_AWT, test "$use_x_awt" = yes)

AC_DEFINE_UNQUOTED(AWT_TOOLKIT, "$TOOLKIT", [Name of default AWT toolkit])

expanded_prefix=$prefix
if test "X$prefix" = XNONE; then
  expanded_prefix=${ac_default_prefix}
fi

AC_DEFINE_UNQUOTED(LIBGCJ_PREFIX, "$expanded_prefix", [Installation prefix])

# Create standard.omit based on decisions we just made.
cp $srcdir/standard.omit.in standard.omit
if test "$use_xlib_awt" != yes; then
   echo gnu/awt/xlib >> standard.omit
   echo gnu/gcj/xlib >> standard.omit
fi
if test "$use_x_awt" != yes; then
   echo gnu/java/awt/peer/x >> standard.omit
fi

# Tools that need to be compiled against classpath's tools classes
: > vm-tools-packages
for package in gnu/gcj/tools/gc_analyze ; do
    echo $package >> standard.omit
    echo $package >> vm-tools-packages
done

if test -z "${with_multisubdir}"; then
   builddotdot=.
else
   builddotdot=`echo ${with_multisubdir} | sed -e 's:[[^/]][[^/]]*:..:g'`
fi

NATIVE=yes

# Which gcj and host gcj (for ecjx) do we use?
which_gcj=default
host_exeext=${ac_exeext}
GCJ_FOR_ECJX='$(GCJ)'
GCC_FOR_ECJX='$(CXX)'
built_gcc_dir="`cd ${builddotdot}/../../${host_subdir}/gcc && ${PWDCMD-pwd}`"
if test -n "${with_cross_host}"; then
  # We are being configured with a cross compiler. We can't
  # use ac_exeext, because that is for the target platform.
  NATIVE=no
  cross_host_exeext=
  GCJ_FOR_ECJX="${with_cross_host}-gcj"
  GCC_FOR_ECJX="${with_cross_host}-gcc"
  case "${with_cross_host}" in
     *mingw* | *cygwin*)
         cross_host_exeext=.exe
     ;;
  esac
  host_exeext=${cross_host_exeext}
  if test -x "${built_gcc_dir}/gcj${cross_host_exeext}"; then
     if test x"$build_noncanonical" = x"$with_cross_host"; then
        # Ordinary cross (host!=target and host=build)
        which_gcj=built
     else
        # Canadian cross (host!=target and host!=build)
        which_gcj=cross
     fi
  else
     which_gcj=cross
  fi
else
  # We are being configured with a native or crossed-native compiler
  if test -x "${built_gcc_dir}/gcj${ac_exeext}"; then
     if test x"$build" = x"$host"; then
        # True native build (host=target and host=build)
        which_gcj=built
     else
        # Crossed-native build (host=target and host!=build)
        which_gcj=cross
     fi
  else
     which_gcj=path
  fi
fi
libgcjdir=`${PWDCMD-pwd}`
case $GCJ in
*" -B"*)
  # Just in case there is a comma in the build dir, quote it for the
  # sed command below.
  case $libgcjdir in
  *[[,\\]]*) qlibgcjdir=`echo "$libgcjdir" | sed 's:[[,\\]]:\\&:g'`;;
  *) qlibgcjdir=$libgcjdir;;
  esac
  GCJ=`echo "$GCJ" | sed "s, -B, -B$qlibgcjdir/&,"`
  ;;
*)
  GCJ=$GCJ" -B$libgcjdir/"
  ;;
esac
case "${which_gcj}" in
   built)
      GCJH='$(top_builddir)/$(MULTIBUILDTOP)../../$(host_subdir)/gcc/gcjh'
   ;;
   cross)
      if test "x${with_newlib}" = "xyes"; then
         # FIXME (comment): Why is this needed?
         LIBGCC_UNWIND_INCLUDE=
      fi
      GCJH='$(target_noncanonical)-gcjh'
   ;;
   path)
      GCJH=gcjh
   ;;
esac
JAVAC="$GCJ -C"
export JAVAC

AC_SUBST(GCJ_FOR_ECJX)
AC_SUBST(GCC_FOR_ECJX)
AC_SUBST(GCJH)
AC_SUBST(host_exeext)

# Create it, so that compile/link tests don't fail
test -f libgcj.spec || touch libgcj.spec



# Set up to configure Classpath.
# FIXME: no supported way to pass args in autoconf.
# Disable tool wrappers to avoid ltdl.h configure check.
ac_configure_args="$ac_configure_args --disable-tool-wrappers"
ac_configure_args="$ac_configure_args --disable-load-library"
ac_configure_args="$ac_configure_args --${LIBGCJDEBUG}-debug"
ac_configure_args="$ac_configure_args --enable-default-toolkit=$TOOLKIT"
dir1=`cd $srcdir && pwd`
dir2=`pwd`
ac_configure_args="$ac_configure_args --with-vm-classes=$dir1:$dir2"
ac_configure_args="$ac_configure_args --disable-core-jni"
dnl FIXME?
ac_configure_args="$ac_configure_args --disable-examples"
ac_configure_args="$ac_configure_args --with-glibj=build"
if test "$browser_plugin_enabled" != yes; then
  ac_configure_args="$ac_configure_args --disable-plugin"
else
  ac_configure_args="$ac_configure_args --enable-plugin"
fi
if test "$gconf_enabled" != yes; then
  ac_configure_args="$ac_configure_args --disable-gconf-peer"
  ac_configure_args="$ac_configure_args --enable-default-preferences-peer=gnu.java.util.prefs.FileBasedFactory"
fi
if test "$use_gtk_awt" != yes; then
  ac_configure_args="$ac_configure_args --disable-gtk-peer --disable-plugin"
fi
if test "$use_qt_awt" != yes; then
  ac_configure_args="$ac_configure_args --disable-qt-peer"
else
  # We need this as qt is disabled by default in classpath.
  ac_configure_args="$ac_configure_args --enable-qt-peer"
fi
if test "$use_x_awt" != yes; then
  ac_configure_args="$ac_configure_args --without-escher"
else
  # We need this as escher is disabled by default in classpath.
  if test "$use_escher" != true; then
    AC_MSG_ERROR([Please supply an absolute path to the Escher library])
  else
    ac_configure_args="$ac_configure_args --with-escher=$with_escher"
  fi
fi
if test "x$BUILD_GJDOC" = xno; then
  ac_configure_args="$ac_configure_args --disable-gjdoc"
fi
# -Werror causes unavoidable problems in code using alsa.
ac_configure_args="$ac_configure_args --disable-regen-headers"
ac_configure_args="$ac_configure_args --disable-Werror"
dnl --with-gcj=$GCJ
dnl --with-javah=$GCJH
dnl gtk-cairo -- just export here...
dnl --enable-regen-headers?

# Only use libltdl for non-newlib builds.
if test "x${with_newlib}" = "x" || test "x${with_newlib}" = "xno"; then
   AC_LIBLTDL_CONVENIENCE
   AC_LIBTOOL_DLOPEN
   DIRLTDL=libltdl
   AC_DEFINE(USE_LTDL, 1, [Define if libltdl is in use.])
   # Sigh.  Libtool's macro doesn't do the right thing.
   INCLTDL="-I\$(top_srcdir)/libltdl $INCLTDL"
   # FIXME: this is a hack.
   sub_auxdir="`cd $ac_aux_dir && ${PWDCMD-pwd}`"
   ac_configure_args="$ac_configure_args --with-auxdir=$sub_auxdir"
fi
AC_SUBST(INCLTDL)
AC_SUBST(LIBLTDL)
AC_SUBST(DIRLTDL)
AC_PROG_LIBTOOL
ACX_LT_HOST_FLAGS
AM_PROG_GCJ
AM_PROG_CC_C_O

AC_CONFIG_SUBDIRS(classpath libltdl)

# The -no-testsuite modules omit the test subdir.
AM_CONDITIONAL(TESTSUBDIR, test -d $srcdir/testsuite)

# Should the runtime set system properties by examining the 
# environment variable GCJ_PROPERTIES?
AC_ARG_ENABLE(getenv-properties,
  AS_HELP_STRING([--disable-getenv-properties],
                 [don't set system properties from GCJ_PROPERTIES]))

# Whether GCJ_PROPERTIES is used depends on the target.
if test -z "$enable_getenv_properties"; then
   enable_getenv_properties=${enable_getenv_properties_default-yes}
fi
if test "$enable_getenv_properties" = no; then
   AC_DEFINE(DISABLE_GETENV_PROPERTIES, 1,
     [Define if system properties shouldn't be read from getenv("GCJ_PROPERTIES").])
fi

# Whether we should use arguments to main()
if test -z "$enable_main_args"; then
   enable_main_args=${enable_main_args_default-yes}
fi
if test "$enable_main_args" = no; then
   AC_DEFINE(DISABLE_MAIN_ARGS, 1, [Define if we should ignore arguments to main().])
fi


# Should we use hashtable-based synchronization?
# Currently works only for Linux X86/ia64
# Typically faster and more space-efficient
AC_ARG_ENABLE(hash-synchronization,
  AS_HELP_STRING([--enable-hash-synchronization],
                 [use global hash table for monitor locks]))

if test -z "$enable_hash_synchronization"; then
   enable_hash_synchronization=$enable_hash_synchronization_default
fi


install_ecj_jar=no
ECJ_BUILD_JAR=
ECJ_JAR=
AC_ARG_WITH(ecj-jar,
	AS_HELP_STRING([--with-ecj-jar=FILE], [use preinstalled ecj jar]),
	[ECJ_JAR=$withval],
	[if test -f $multi_basedir/ecj.jar; then
	   ECJ_BUILD_JAR=$multi_basedir/ecj.jar
	   ECJ_JAR='$(jardir)/ecj.jar'
	   install_ecj_jar=yes
	 fi])
AC_SUBST(ECJ_BUILD_JAR)
AC_SUBST(ECJ_JAR)
AM_CONDITIONAL(BUILD_ECJ1, test "$ECJ_JAR" != "")
AM_CONDITIONAL(INSTALL_ECJ_JAR, test $install_ecj_jar = yes)

AC_ARG_WITH(java-home,
  AS_HELP_STRING([--with-java-home=DIRECTORY],
                 [value of java.home system property]),
		 [JAVA_HOME="${withval}"], [JAVA_HOME=""])
AM_CONDITIONAL(JAVA_HOME_SET, test ! -z "$JAVA_HOME")
AC_SUBST(JAVA_HOME)

suppress_libgcj_bc=no
AC_ARG_ENABLE(libgcj-bc,
  AS_HELP_STRING([--enable-libgcj-bc],
                 [enable(default) or disable BC ABI for portions of libgcj]),
  [if test "$enable_libgcj_bc" = "no"; then
     suppress_libgcj_bc=yes
   fi])
AM_CONDITIONAL(SUPPRESS_LIBGCJ_BC, test "$suppress_libgcj_bc" = "yes")

build_libgcj_reduced_reflection=no
AC_ARG_ENABLE(reduced-reflection,
  AS_HELP_STRING([--enable-reduced-reflection],
                 [enable or disable(default) -freduced-reflection when building portions of libgcj]),
  [if test "$enable_reduced_reflection" = "yes"; then
     build_libgcj_reduced_reflection=yes
   fi])
AM_CONDITIONAL(BUILD_LIBGCJ_REDUCED_REFLECTION, test "$build_libgcj_reduced_reflection" = "yes")

# What is the native OS API for MinGW?
AC_ARG_WITH(win32-nlsapi,
  AS_HELP_STRING([--with-win32-nlsapi=ansi or unicows or unicode],
                 [native MinGW libgcj Win32 OS API (default is ansi)]),
[case "${withval}" in
  ansi) with_win32_nlsapi=ansi ;;
  unicows) with_win32_nlsapi=unicows ;;
  unicode) with_win32_nlsapi=unicode ;;
  *) AC_MSG_ERROR(Bad value ${withval} for --with-win32-nlsapi.) ;;
esac],[with_win32_nlsapi=ansi])

case "${with_win32_nlsapi}" in
  unicows | unicode) 
    AC_DEFINE(MINGW_LIBGCJ_UNICODE, 1,
      [Define if MinGW libgcj uses the Windows UNICODE OS API.])
    ;;
esac

# configure.host sets slow_pthread_self if the synchronization code should 
# try to avoid pthread_self calls by caching thread IDs in a hashtable.
if test "${slow_pthread_self}" = "yes"; then
  AC_DEFINE(SLOW_PTHREAD_SELF, 1,
    [Define if if the synchronization code should try to avoid pthread_self calls by caching thread IDs in a hashtable.])
fi

# Check for gc debugging.  This option is handled both here and in the GC.
AC_ARG_ENABLE(gc-debug,
   AS_HELP_STRING([--enable-gc-debug],
		  [include full support for pointer backtracing etc.]),
[ if test "$enable_gc_debug" = "yes"; then
    AC_DEFINE(LIBGCJ_GC_DEBUG, 1, 
	      [Define if we want to use debug calls into the garbage collector.])
  fi])

# See if the user has the interpreter included.
AC_ARG_ENABLE(interpreter,
  AS_HELP_STRING([--enable-interpreter],
                 [enable interpreter]),
  [if test "$enable_interpreter" = yes; then
    # This can also be set in configure.host.
    libgcj_interpreter=yes
  elif test "$enable_interpreter" = no; then
    libgcj_interpreter=no
  fi])

if test "$libgcj_interpreter" = yes; then
   AC_DEFINE(INTERPRETER, 1, [Define if you want a bytecode interpreter.])
fi
INTERPRETER="$libgcj_interpreter"
AC_SUBST(INTERPRETER)
AM_CONDITIONAL(INTERPRETER, test "$libgcj_interpreter" = yes)

AC_MSG_CHECKING([for exception model to use])
AC_LANG_PUSH(C++)
AC_ARG_ENABLE(sjlj-exceptions,
  AS_HELP_STRING([--enable-sjlj-exceptions],
                 [force use of builtin_setjmp for exceptions]),
[:],
[dnl Botheration.  Now we've got to detect the exception model.
dnl Link tests against libgcc.a are problematic since -- at least
dnl as of this writing -- we've not been given proper -L bits for
dnl single-tree newlib and libgloss.
dnl
dnl This is what AC_TRY_COMPILE would do if it didn't delete the
dnl conftest files before we got a change to grep them first.
cat > conftest.$ac_ext << EOF
[#]line __oline__ "configure"
struct S { ~S(); };
void bar();
void foo()
{
  S s;
  bar();
}
EOF
old_CXXFLAGS="$CXXFLAGS"  
CXXFLAGS=-S
if AC_TRY_EVAL(ac_compile); then
  if grep _Unwind_SjLj_Resume conftest.s >/dev/null 2>&1 ; then
    enable_sjlj_exceptions=yes
  elif grep _Unwind_Resume conftest.s >/dev/null 2>&1 ; then
    enable_sjlj_exceptions=no
  elif grep __cxa_end_cleanup conftest.s >/dev/null 2>&1 ; then
    # ARM EH ABI.
    enable_sjlj_exceptions=no
  fi
fi
CXXFLAGS="$old_CXXFLAGS"
rm -f conftest*])
if test x$enable_sjlj_exceptions = xyes; then
  AC_DEFINE(SJLJ_EXCEPTIONS, 1,
	[Define if the compiler is configured for setjmp/longjmp exceptions.])
  ac_exception_model_name=sjlj
elif test x$enable_sjlj_exceptions = xno; then
  ac_exception_model_name="call frame"
else
  AC_MSG_ERROR([unable to detect exception model])
fi
AC_LANG_POP(C++)
AC_MSG_RESULT($ac_exception_model_name)

# If we are non using SJLJ exceptions, and this host does not have support 
# for unwinding from a signal handler, enable checked dereferences and divides.
if test $can_unwind_signal = no && test $enable_sjlj_exceptions = no; then
  CHECKREFSPEC=-fcheck-references
  DIVIDESPEC=-fuse-divide-subroutine
  EXCEPTIONSPEC=
fi

# See if the user wants to disable java.net.  This is the mildly
# ugly way that we admit that target-side configuration sucks.
AC_ARG_ENABLE(java-net,
  AS_HELP_STRING([--disable-java-net],
                 [disable java.net]))

# Whether java.net is built by default can depend on the target.
if test -z "$enable_java_net"; then
   enable_java_net=${enable_java_net_default-yes}
fi
if test "$enable_java_net" = no; then
   AC_DEFINE(DISABLE_JAVA_NET, 1, [Define if java.net native functions should be stubbed out.])
fi

# See if the user wants to configure without libffi.  Some
# architectures don't support it, and default values are set in 
# configure.host.
AC_ARG_WITH(libffi,
  AS_HELP_STRING([--without-libffi],
                 [don't use libffi]),
  [:],
  [with_libffi=${with_libffi_default-yes}])

LIBFFI=
LIBFFIINCS=
if test "$with_libffi" != no; then
   AC_DEFINE(USE_LIBFFI, 1, [Define if we're to use libffi.])
   LIBFFI=../libffi/libffi_convenience.la
   LIBFFIINCS='-I$(top_srcdir)/../libffi/include -I../libffi/include'
fi
AC_SUBST(LIBFFI)
AC_SUBST(LIBFFIINCS)

# See if the user wants to disable JVMPI support.
AC_ARG_ENABLE(jvmpi,
  AS_HELP_STRING([--disable-jvmpi],
                 [disable JVMPI support]))

if test "$enable_jvmpi" != no; then
    AC_DEFINE(ENABLE_JVMPI, 1, [Define if you are using JVMPI.])
fi

# If the target is an eCos system, use the appropriate eCos
# I/O routines.
# FIXME: this should not be a local option but a global target
# system; at present there is no eCos target.
TARGET_ECOS=${PROCESS-"no"}
AC_ARG_WITH(ecos,
[  --with-ecos             enable runtime eCos target support],
TARGET_ECOS="$with_ecos"
)

case "$TARGET_ECOS" in
   no) case "$host" in
      *mingw*)
            PLATFORM=Win32
	    PLATFORMNET=Win32
	    PLATFORMH=win32.h
        CHECK_FOR_BROKEN_MINGW_LD
      ;;
      *)
            PLATFORM=Posix
	    PLATFORMNET=Posix
	    PLATFORMH=posix.h
      ;;
      esac
      ;;
   *)
      PLATFORM=Ecos
      PLATFORMNET=NoNet
      AC_DEFINE(ECOS, 1, [Define if you're running eCos.])
      PLATFORMH=posix.h
      ;;
esac
AC_SUBST(PLATFORM)
AC_CONFIG_LINKS(include/platform.h:include/$PLATFORMH)

AC_EGREP_HEADER(uint32_t, stdint.h, AC_DEFINE(HAVE_INT32_DEFINED, 1,
                                      [Define if you have int32_t and uint32_t.]))
AC_EGREP_HEADER(uint32_t, inttypes.h, AC_DEFINE(HAVE_INT32_DEFINED, 1,
                                        [Define if you have int32_t and uint32_t.]))
AC_EGREP_HEADER(u_int32_t, sys/types.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1,
                                          [Define if you have u_int32_t]))
AC_EGREP_HEADER(u_int32_t, sys/config.h, AC_DEFINE(HAVE_BSD_INT32_DEFINED, 1,
                                           [Define if you have u_int32_t]))

AM_CONDITIONAL(USING_WIN32_PLATFORM, test "$PLATFORM" = Win32)
AM_CONDITIONAL(USING_POSIX_PLATFORM, test "$PLATFORM" = Posix || test "$PLATFORM" = Ecos)

case "$host" in
  *-darwin*) DARWIN_CRT=true ;;
  *) DARWIN_CRT=false ;;
esac
AM_CONDITIONAL(USING_DARWIN_CRT, $DARWIN_CRT)

# This may not be defined in a non-ANS conformant embedded system.
# FIXME: Should these case a runtime exception in that case?
AC_EGREP_HEADER(localtime, time.h, AC_DEFINE(HAVE_LOCALTIME, 1,
                                  [Define is you have 'localtime' in <time.h>]))

# Create the subdirectory for natFile.cc, or the attempt
# to create the link will fail.
test -d java || mkdir java
test -d java/io || mkdir java/io
test -d gnu || mkdir gnu
AC_CONFIG_LINKS(java/io/natFile.cc:java/io/natFile${FILE-${PLATFORM}}.cc)

# Likewise for natConcreteProcess.cc.
test -d java/lang || mkdir java/lang
AC_CONFIG_LINKS(java/lang/natConcreteProcess.cc:java/lang/nat${PLATFORM}Process.cc)

# Likewise for natVMInetAddress.cc and natVMNetworkInterface.cc.
test -d java/net || mkdir java/net
AC_CONFIG_LINKS(java/net/natVMInetAddress.cc:java/net/natVMInetAddress${PLATFORMNET}.cc)
AC_CONFIG_LINKS(java/net/natVMNetworkInterface.cc:java/net/natVMNetworkInterface${PLATFORMNET}.cc)

# Likewise for natPlainSocketImpl.cc and natPlainDatagramSocketImpl.cc.
test -d gnu/java || mkdir gnu/java
test -d gnu/java/net || mkdir gnu/java/net
AC_CONFIG_LINKS(gnu/java/net/natPlainSocketImpl.cc:gnu/java/net/natPlainSocketImpl${PLATFORMNET}.cc)
AC_CONFIG_LINKS(gnu/java/net/natPlainDatagramSocketImpl.cc:gnu/java/net/natPlainDatagramSocketImpl${PLATFORMNET}.cc)

# Likewise for natVMPipe.cc and natVMSelector.cc.
test -d gnu/java/nio || mkdir gnu/java/nio
AC_CONFIG_LINKS(gnu/java/nio/natVMPipe.cc:gnu/java/nio/natVMPipe${PLATFORM}.cc)
AC_CONFIG_LINKS(gnu/java/nio/natVMSelector.cc:gnu/java/nio/natVMSelector${PLATFORM}.cc)

# Likewise for natFileChannelImpl.cc
test -d gnu/java/nio/channels || mkdir gnu/java/nio/channels
AC_CONFIG_LINKS(gnu/java/nio/channels/natFileChannelImpl.cc:gnu/java/nio/channels/natFileChannel${FILE-${PLATFORM}}.cc)

# Likewise for natVMSecureRandom.cc
test -d gnu/java/security || mkdir gnu/java/security
test -d gnu/java/security/jce || mkdir gnu/java/security/jce
test -d gnu/java/security/jce/prng || mkdir gnu/java/security/jce/prng
AC_CONFIG_LINKS(gnu/java/security/jce/prng/natVMSecureRandom.cc:gnu/java/security/jce/prng/natVMSecureRandom${FILE-${PLATFORM}}.cc)

case "${host}" in
    *mingw*)
      SYSTEMSPEC="-lgdi32 -lws2_32"
      if test "${with_win32_nlsapi}" = "unicows"; then
        SYSTEMSPEC="-lunicows $SYSTEMSPEC"
      fi
    ;;
    *-*-darwin[[912]]*)
      SYSTEMSPEC="%{!Zdynamiclib:%{!Zbundle:-allow_stack_execute}}"
    ;;
    *)
      SYSTEMSPEC=
    ;;
esac
AC_SUBST(SYSTEMSPEC)

AC_ARG_WITH(system-zlib,
  AS_HELP_STRING([--with-system-zlib],
                 [use installed libz]))
ZLIBSPEC=
AC_SUBST(ZLIBSPEC)
ZLIBTESTSPEC=
AC_SUBST(ZLIBTESTSPEC)

AC_PATH_XTRA

# FIXME: this should be _libs on some hosts.
libsubdir=.libs

LIBGCJTESTSPEC="-L`${PWDCMD-pwd}`/.libs -rpath `${PWDCMD-pwd}`/.libs"

LIBSTDCXXSPEC=
# extra LD Flags which are required for targets
case "${host}" in
*-*-darwin[[0-7]].*)
    # For now we have to disable it on darwin[8-9] because it slows down
    # the linking phase. A possible bug in ld?
    # on Darwin -single_module speeds up loading of the dynamic libraries.
    extra_ldflags_libjava=-Wl,-single_module
    ;;
arm*-*-linux-*)
    # Some of the ARM unwinder code is actually in libstdc++.  We
    # could in principle replicate it in libgcj, but it's better to
    # have a dependency on libstdc++.
    extra_ldflags='-L$(here)/../libstdc++-v3/src/.libs -lstdc++'
    LIBSTDCXXSPEC=-lstdc++
    LIBGCJTESTSPEC="-L`${PWDCMD-pwd}`/.libs -L`${PWDCMD-pwd}`/../libstdc++-v3/src/.libs -rpath `${PWDCMD-pwd}`/.libs:`${PWDCMD-pwd}`/../libstdc++-v3/src/.libs -lstdc++"
    ;;
*-*-cygwin)
    extra_ldflags_libjava=-liconv
    ;;
*-*-mingw*)
    extra_ldflags_libjava=-lws2_32
    ;;
esac

LIBMATHSPEC=
# extra LD Flags which are required for targets
case "${host}" in
*-*-darwin*)
     ;;
*)
     LIBMATHSPEC=-lm
     ;;
esac

# Check for --no-merge-exidx-entries, an ARM-specific linker option.
AC_CACHE_CHECK([for --no-merge-exidx-entries], [libgcj_cv_exidx],
  [saved_ldflags="$LDFLAGS"
   LDFLAGS="${LDFLAGS} -Wl,--no-merge-exidx-entries"
   AC_LINK_IFELSE([int main(void){ return 0;} ],
		      [eval "libgcj_cv_exidx=yes"],
		      [eval "libgcj_cv_exidx=no"])
   LDFLAGS="${saved_ldflags}"]
)
if test "${libgcj_cv_exidx}" = "yes"; then
  SYSTEMSPEC="${SYSTEMSPEC} --no-merge-exidx-entries"
  extra_ldflags="${extra_ldflags} -Wl,--no-merge-exidx-entries"
fi   

AC_SUBST(extra_ldflags_libjava)
AC_SUBST(extra_ldflags)
AC_SUBST(LIBSTDCXXSPEC)
AC_SUBST(LIBMATHSPEC)

AC_SUBST(LIBGCJTESTSPEC)

# Allow the GC to be disabled.  Can be useful when debugging.
AC_MSG_CHECKING([for garbage collector to use])
AC_ARG_ENABLE(java-gc,
  AS_HELP_STRING([--enable-java-gc=TYPE],
                 [choose garbage collector (default is boehm)]),
  [GC=$enableval],
  [GC=boehm])
GCLIBS=
GCINCS=
GCDEPS=
GCSPEC=
JC1GCSPEC=
GCTESTSPEC=
case "$GC" in
 boehm)
    AC_MSG_RESULT(boehm)
    GCLIBS=../boehm-gc/libgcjgc_convenience.la
    JC1GCSPEC='-fuse-boehm-gc'
    GCTESTSPEC="-L`${PWDCMD-pwd}`/../boehm-gc/.libs -rpath `${PWDCMD-pwd}`/../boehm-gc/.libs"
    GCINCS='-I$(top_srcdir)/../boehm-gc/include -I../boehm-gc/include'
    GCOBJS=boehm.lo    
    GCHDR=boehm-gc.h
    # The POSIX thread support needs to know this.
    AC_DEFINE(HAVE_BOEHM_GC, 1, [Define if Boehm GC in use.])
    ;;
 no)
    AC_MSG_RESULT(none)
    GCHDR=no-gc.h
    ;;
 *)
    AC_MSG_ERROR([unrecognized collector "$GC"])
    ;;
esac
AC_SUBST(GCLIBS)
AC_SUBST(GCINCS)
AC_SUBST(GCDEPS)
AC_SUBST(GCSPEC)
AC_SUBST(JC1GCSPEC)
AC_SUBST(GCTESTSPEC)
AC_CONFIG_LINKS(include/java-gc.h:include/$GCHDR)
AM_CONDITIONAL(USING_BOEHMGC, test "$GC" = boehm)
AM_CONDITIONAL(USING_NOGC, test "$GC" = no)


AC_MSG_CHECKING([for thread model used by GCC])
THREADS=`$CC -v 2>&1 | sed -n 's/^Thread model: //p'`
AC_MSG_RESULT([$THREADS])

case "$THREADS" in
 no | none | single)
    THREADS=none
    ;;
 aix | posix | pthreads)
    THREADS=posix
    case "$host" in
     *-*-linux*)
	AC_DEFINE(LINUX_THREADS, 1, [Define if using POSIX threads on Linux.])
	;;
    esac
    ;;
 win32)
    ;;
 dce | vxworks)
    AC_MSG_ERROR(thread package $THREADS not yet supported)
    ;;
 *)
    AC_MSG_ERROR($THREADS is an unknown thread package)
    ;;
esac

THREADCXXFLAGS=
THREADLDFLAGS=
THREADLIBS=
THREADINCS=
THREADDEPS=
THREADH=
THREADSPEC=
THREADSTARTFILESPEC=
case "$THREADS" in
 posix)
    case "$host" in
     *-*-cygwin*)
	# Don't set THREADLIBS here.  Cygwin doesn't have -lpthread.
	;;
     *-*-freebsd[[34]].*)
	# Before FreeBSD 5, it didn't have -lpthread (or any library which
	# merely adds pthread_* functions) but it does have a -pthread switch
	# which is required at link-time to select -lc_r *instead* of -lc.
	THREADLDFLAGS=-pthread
	# Don't set THREADSPEC here as might be expected since -pthread is
	# not processed when found within a spec file, it must come from
	# the command line.  For now, the user must provide the -pthread
	# switch to link code compiled with gcj.  In future, consider adding
	# support for weak references to pthread_* functions ala gthr.h API.
	THREADSPEC='%{!pthread: %{!shared: %eUnder this configuration, the user must provide -pthread when linking.}}'
	;;
     *-*-freebsd*)
	# FreeBSD >=5.3 implements a model much closer to other modern UNIX
	# systems which support threads and -lpthread.
	THREADLDFLAGS=-pthread
	THREADSPEC=-lpthread
	;;
     hppa*-hp-hpux*)
	THREADCXXFLAGS=-pthread
	# boehm-gc needs some functions from librt, so link that too.
	THREADLIBS='-lpthread -lrt'
	THREADSPEC='-lpthread -lrt'
	;;
     *-*-solaris2.9)
	# Need libthread for TLS support.
	THREADLIBS='-lpthread -lthread'
	THREADSPEC='-lpthread -lthread'
	;;
     *-*-darwin*)
	# Don't set THREADLIBS or THREADSPEC as Darwin already
	# provides pthread via libSystem.
	;;
     *)
	THREADLIBS=-lpthread
	THREADSPEC=-lpthread
	;;
    esac
    THREADH=posix-threads.h
    # MIT pthreads doesn't seem to have the mutexattr functions.
    # But for now we don't check for it.  We just assume you aren't
    # using MIT pthreads.
    AC_DEFINE(HAVE_PTHREAD_MUTEXATTR_INIT, 1, [Define if using POSIX threads that have the mutexattr functions.])

    # If we're using the Boehm GC, then we happen to know that it
    # defines _REENTRANT, so we don't bother.  Eww.
    if test "$GC" != boehm; then
       AC_DEFINE(_REENTRANT, 1, [Required define if using POSIX threads])
    fi
    AC_DEFINE(_POSIX_PTHREAD_SEMANTICS, 1, [Required define if using POSIX threads])
    ;;

 win32)
    THREADH=win32-threads.h
    THREADCXXFLAGS=-mthreads
    # We need thread-safe exception handling so _CRT_MT should be set to 1.
    # But we do not want the executables created to be dependent on
    # mingwm10.dll which provides a __mingwthr_key_dtor() that cleans up
    # exception handling contexts.  The following kludge achieves this effect
    # and causes a dummy __mingwthr_key_dtor() to be linked in from
    # libmingw32.a.  This causes a memory leak of about 24 bytes per thread.
    # A workaround is to explicitly use -mthreads while linking Java programs.
    # See PR libgcj/28263.
    #
    # FIXME: In Java we are able to detect thread death at the end of
    # Thread.run() so we should be able to clean up the exception handling
    # contexts ourselves.
    case "$host" in
    *-w64-mingw*)
      ;;
    *)
      THREADSTARTFILESPEC='crtmt%O%s'
      ;;
    esac
    ;;
 none)
    THREADH=no-threads.h
    ;;
esac
AC_CONFIG_LINKS(include/java-threads.h:include/$THREADH)
AC_SUBST(THREADLIBS)
AC_SUBST(THREADINCS)
AC_SUBST(THREADDEPS)
AC_SUBST(THREADSPEC)
AC_SUBST(THREADSTARTFILESPEC)
AC_SUBST(THREADLDFLAGS)
AC_SUBST(THREADCXXFLAGS)
AM_CONDITIONAL(USING_POSIX_THREADS, test "$THREADS" = posix)
AM_CONDITIONAL(USING_WIN32_THREADS, test "$THREADS" = win32)
AM_CONDITIONAL(USING_NO_THREADS, test "$THREADS" = none)
if test "$enable_shared" != yes; then
  use_libgcj_bc=no
fi
AM_CONDITIONAL(USE_LIBGCJ_BC, test "$use_libgcj_bc" = yes)

if test -d sysdep; then true; else mkdir sysdep; fi
AC_CONFIG_LINKS(sysdep/locks.h:sysdep/$sysdeps_dir/locks.h)
AC_CONFIG_LINKS(sysdep/backtrace.h:$fallback_backtrace_h)
AC_CONFIG_LINKS(sysdep/descriptor.h:$descriptor_h)

LIBGCJ_SPEC_LGCJ=-lgcj
LIBGCJ_SPEC_LGCJ_BC=-lgcj_bc
if test x"$libgcj_spec_lgcj_override" != x ; then
  LIBGCJ_SPEC_LGCJ=$libgcj_spec_lgcj_override
fi
if test x"$libgcj_spec_lgcj_bc_override" != x ; then
  LIBGCJ_SPEC_LGCJ_BC=$libgcj_spec_lgcj_bc_override
fi
LIBGCJ_SPEC="%{s-bc-abi:} $LIBGCJ_SPEC_LGCJ"
if test "$use_libgcj_bc" = yes; then
  LIBGCJ_SPEC="%{static|static-libgcj|!s-bc-abi:$LIBGCJ_SPEC_LGCJ;:$LIBGCJ_SPEC_LGCJ_BC}"
  LIBGCJ_BC_SPEC="%{findirect-dispatch:-fPIC}"
fi
AC_SUBST(LIBGCJ_SPEC)
AC_SUBST(LIBGCJ_BC_SPEC)

HASH_SYNC_SPEC=
# Hash synchronization is only useful with posix threads right now.
if test "$enable_hash_synchronization" = yes && test "$THREADS" != "none"; then
   HASH_SYNC_SPEC=-fhash-synchronization
   AC_DEFINE(JV_HASH_SYNCHRONIZATION, 1, [Define if hash synchronization is in use])
fi
AC_SUBST(HASH_SYNC_SPEC)

AM_CONDITIONAL(USING_GCC, test "$GCC" = yes)

# We're in the tree with libgcc, and need to include some of its headers.
LIBGCC_UNWIND_INCLUDE='-I$(multi_basedir)/./libjava/../libgcc'

if test "x${with_newlib}" = "xyes"; then
   # We are being configured with a cross compiler.  AC_REPLACE_FUNCS
   # may not work correctly, because the compiler may not be able to
   # link executables.

   # We assume newlib.  This lets us hard-code the functions we know
   # we'll have.
   AC_DEFINE(HAVE_MEMMOVE, 1, [Define if you have memmove.])
   AC_DEFINE(HAVE_MEMCPY, 1, [Define if you have memcpy.])
   AC_DEFINE(HAVE_TIME, 1, [Define if you have time.])
   AC_DEFINE(HAVE_GMTIME_R, 1, [Define if you have the 'gmtime_r' function])
   AC_DEFINE(HAVE_LOCALTIME_R, 1, [Define if you have the 'localtime_r' function.])
   AC_DEFINE(HAVE_USLEEP_DECL, 1, [Define if usleep is declared in <unistd.h>.])
   # This is only for POSIX threads.
   AC_DEFINE(HAVE_PTHREAD_MUTEXATTR_INIT, 1, [Define if using POSIX threads that have the mutexattr functions.])

   # Assume we do not have getuid and friends.
   AC_DEFINE(NO_GETUID, 1, [Define if getuid() and friends are missing.])
   PLATFORMNET=NoNet
else
   AC_CHECK_FUNCS([strerror_r select fstat open fsync sleep opendir \
                   localtime_r getpwuid_r getcwd \
		   access stat lstat mkdir rename rmdir unlink utime chmod readlink \
		   nl_langinfo setlocale \
		   inet_pton uname inet_ntoa \
		   getrlimit sigaction ftruncate mmap \
		   getifaddrs])
   AC_CHECK_FUNCS(inet_aton inet_addr, break)
   AC_CHECK_HEADERS(unistd.h dlfcn.h sys/resource.h)
   # Do an additional check on dld, HP-UX for example has dladdr in libdld.sl
   AC_CHECK_LIB(dl, dladdr, [
       AC_DEFINE(HAVE_DLADDR, 1, [Define if you have dladdr()])], [
       AC_CHECK_LIB(dld, dladdr, [
       AC_DEFINE(HAVE_DLADDR, 1, [Define if you have dladdr()])])])

   if test x"$cross_compiling" = x"no"; then
     AC_CHECK_FILES(/proc/self/exe, [
       AC_DEFINE(HAVE_PROC_SELF_EXE, 1, [Define if you have /proc/self/exe])])
     AC_CHECK_FILES(/proc/self/maps, [
       AC_DEFINE(HAVE_PROC_SELF_MAPS, 1,
         [Define if you have /proc/self/maps])])
   else
     case $host in
     *-linux*)
       AC_DEFINE(HAVE_PROC_SELF_EXE, 1, [Define if you have /proc/self/exe])
       AC_DEFINE(HAVE_PROC_SELF_MAPS, 1, [Define if you have /proc/self/maps])
       ;;
     esac
   fi

   AM_ICONV
   AM_LC_MESSAGES
   AC_STRUCT_TIMEZONE
   LDLIBICONV=`echo " $LIBICONV " | sed "s/${acl_cv_wl--Wl,}/ /g; s/,/ /g"`
   AC_SUBST([LDLIBICONV])

   AC_CHECK_FUNCS(gethostbyname_r, [
     AC_DEFINE(HAVE_GETHOSTBYNAME_R, 1,
       [Define if you have the 'gethostbyname_r' function.])
     # There are two different kinds of gethostbyname_r.
     # We look for the one that returns `int'.
     # Hopefully this check is robust enough.
     AC_EGREP_HEADER(int.*gethostbyname_r, netdb.h, [
       AC_DEFINE(GETHOSTBYNAME_R_RETURNS_INT, 1, [Define if gethostbyname_r returns 'int'.])])

     case " $GCINCS " in
     *" -D_REENTRANT "*) ;;
     *)
	dnl On DU4.0, gethostbyname_r is only declared with -D_REENTRANT
	AC_CACHE_CHECK([whether gethostbyname_r declaration requires -D_REENTRANT],
	[libjava_cv_gethostbyname_r_needs_reentrant],
	[ AC_LANG_PUSH(C++)
	  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netdb.h>]],
	    [[gethostbyname_r("", 0, 0);]])],
	    [libjava_cv_gethostbyname_r_needs_reentrant=no], [dnl
		CPPFLAGS_SAVE="$CPPFLAGS"
		CPPFLAGS="$CPPFLAGS -D_REENTRANT"
		AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netdb.h>]], [[gethostbyname_r("", 0, 0);]])],
		    [libjava_cv_gethostbyname_r_needs_reentrant=yes],
		    [libjava_cv_gethostbyname_r_needs_reentrant=fail])
		CPPFLAGS="$CPPFLAGS_SAVE"
	  ])
	  AC_LANG_POP(C++)
	])
	if test "x$libjava_cv_gethostbyname_r_needs_reentrant" = xyes; then
	  AC_DEFINE(_REENTRANT, 1, [Required define if using POSIX threads])
	fi
     ;;
     esac

     AC_CACHE_CHECK([for struct hostent_data],
	[libjava_cv_struct_hostent_data], [dnl
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#if GETHOSTBYNAME_R_NEEDS_REENTRANT && !defined(_REENTRANT)
# define _REENTRANT 1
#endif
#include <netdb.h>]], [[struct hostent_data data;]])],
	  [libjava_cv_struct_hostent_data=yes],
	  [libjava_cv_struct_hostent_data=no])])
     if test "x$libjava_cv_struct_hostent_data" = xyes; then
       AC_DEFINE(HAVE_STRUCT_HOSTENT_DATA, 1,
         [Define if struct hostent_data is defined in netdb.h])
     fi
   ])

   # FIXME: libjava source code expects to find a prototype for
   # gethostbyaddr_r in netdb.h.  The outer check ensures that
   # HAVE_GETHOSTBYADDR_R will not be defined if the prototype fails
   # to exist where expected.  (The root issue: AC_CHECK_FUNCS assumes C
   # linkage check is enough, yet C++ code requires proper prototypes.)
   AC_EGREP_HEADER(gethostbyaddr_r, netdb.h, [
   AC_CHECK_FUNCS(gethostbyaddr_r, [
     AC_DEFINE(HAVE_GETHOSTBYADDR_R, 1,
       [Define if you have the 'gethostbyaddr_r' function.])
     # There are two different kinds of gethostbyaddr_r.
     # We look for the one that returns `int'.
     # Hopefully this check is robust enough.
     AC_EGREP_HEADER(int.*gethostbyaddr_r, netdb.h, [
       AC_DEFINE(GETHOSTBYADDR_R_RETURNS_INT, 1,
         [Define if gethostbyaddr_r returns 'int'.])])])])

   AC_CHECK_FUNCS(gethostname, [
     AC_DEFINE(HAVE_GETHOSTNAME, 1,
       [Define if you have the 'gethostname' function.])
     AC_EGREP_HEADER(gethostname, unistd.h, [
       AC_DEFINE(HAVE_GETHOSTNAME_DECL, 1,
         [Define if gethostname is declared in <unistd.h>.])])])

   AC_CHECK_FUNCS(usleep, [
     AC_EGREP_HEADER(usleep, unistd.h, [
       AC_DEFINE(HAVE_USLEEP_DECL, 1,
          [Define if usleep is declared in <unistd.h>.])])])

   # Look for these functions in the thread library, but only bother
   # if using POSIX threads.
   if test "$THREADS" = posix; then
      save_LIBS="$LIBS"
      LIBS="$LIBS $THREADLIBS"

      # Look for sched_yield.  Up to Solaris 2.6, it is in libposix4, since
      # Solaris 7 the name librt is preferred.
      AC_CHECK_FUNCS(sched_yield, , [
	AC_CHECK_LIB(rt, sched_yield, [
	  AC_DEFINE(HAVE_SCHED_YIELD)
	  THREADLIBS="$THREADLIBS -lrt"
	  THREADSPEC="$THREADSPEC -lrt"], [
	  AC_CHECK_LIB(posix4, sched_yield, [
	    AC_DEFINE(HAVE_SCHED_YIELD)
	    THREADLIBS="$THREADLIBS -lposix4"
	    THREADSPEC="$THREADSPEC -lposix4"])])])

      AC_CHECK_LIB(rt, clock_gettime, [
         AC_DEFINE(HAVE_CLOCK_GETTIME, 1, [Define if you have clock_gettime()])
	 case "$THREADSPEC" in
	   *-lrt*) ;;
	   *)
 	     THREADSPEC="$THREADSPEC -lrt"
	     THREADLIBS="$THREADLIBS -lrt"
	     ;;
	 esac])

      LIBS="$save_LIBS"

      # We can save a little space at runtime if the mutex has m_count
      # or __m_count.  This is a nice hack for Linux.
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <pthread.h>]], [[
          extern pthread_mutex_t *mutex; int q = mutex->m_count;
        ]])], AC_DEFINE(PTHREAD_MUTEX_HAVE_M_COUNT, 1,
             [Define if pthread_mutex_t has m_count member.]), [
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <pthread.h>]], [[
	    extern pthread_mutex_t *mutex; int q = mutex->__m_count;
	  ]])], AC_DEFINE(PTHREAD_MUTEX_HAVE___M_COUNT, 1,
            [Define if pthread_mutex_t has __m_count member.]))])
   fi

   # We require a way to get the time.
   time_found=no
   AC_CHECK_FUNCS(gettimeofday time ftime, time_found=yes)
   if test "$time_found" = no; then
      AC_MSG_ERROR([no function found to get the time])
   fi

   AC_CHECK_FUNCS(memmove)

   # We require memcpy.
   memcpy_found=no
   AC_CHECK_FUNCS(memcpy, memcpy_found=yes)
   if test "$memcpy_found" = no; then
      AC_MSG_ERROR([memcpy is required])
   fi
   # Do an additional check on dld, HP-UX for example has dlopen in libdld.sl
   AC_CHECK_LIB(dl, dlopen, [
       AC_DEFINE(HAVE_DLOPEN, 1, [Define if dlopen is available])], [
       AC_CHECK_LIB(dld, dlopen, [
       AC_DEFINE(HAVE_DLOPEN, 1, [Define if dlopen is available])])])

   # Some library-finding code we stole from Tcl.
   #--------------------------------------------------------------------
   #	Check for the existence of the -lsocket and -lnsl libraries.
   #	The order here is important, so that they end up in the right
   #	order in the command line generated by make.  Here are some
   #	special considerations:
   #	1. Use "connect" and "accept" to check for -lsocket, and
   #	   "gethostbyname" to check for -lnsl.
   #	2. Use each function name only once:  can't redo a check because
   #	   autoconf caches the results of the last check and won't redo it.
   #	3. Use -lnsl and -lsocket only if they supply procedures that
   #	   aren't already present in the normal libraries.  This is because
   #	   IRIX 5.2 has libraries, but they aren't needed and they're
   #	   bogus:  they goof up name resolution if used.
   #	4. On some SVR4 systems, can't use -lsocket without -lnsl too.
   #	   To get around this problem, check for both libraries together
   #	   if -lsocket doesn't work by itself.
   #--------------------------------------------------------------------

   AC_CACHE_CHECK([for socket libraries], gcj_cv_lib_sockets,
    [gcj_cv_lib_sockets=
     gcj_checkBoth=0
     unset ac_cv_func_connect
     AC_CHECK_FUNC(connect, gcj_checkSocket=0, gcj_checkSocket=1)
     if test "$gcj_checkSocket" = 1; then
	 unset ac_cv_func_connect
	 AC_CHECK_LIB(socket, main, gcj_cv_lib_sockets="-lsocket",
		      gcj_checkBoth=1)
     fi
     if test "$gcj_checkBoth" = 1; then
	 gcj_oldLibs=$LIBS
	 LIBS="$LIBS -lsocket -lnsl"
	 unset ac_cv_func_accept
	 AC_CHECK_FUNC(accept,
		       [gcj_checkNsl=0
			gcj_cv_lib_sockets="-lsocket -lnsl"])
	 unset ac_cv_func_accept
	 LIBS=$gcj_oldLibs
     fi
     unset ac_cv_func_gethostbyname
     gcj_oldLibs=$LIBS
     LIBS="$LIBS $gcj_cv_lib_sockets"
     AC_CHECK_FUNC(gethostbyname, ,
		   [AC_CHECK_LIB(nsl, main,
				 [gcj_cv_lib_sockets="$gcj_cv_lib_sockets -lnsl"])])
     unset ac_cv_func_gethostbyname
     LIBS=$gcj_oldLIBS
   ])
   SYSTEMSPEC="$SYSTEMSPEC $gcj_cv_lib_sockets"

   if test "$with_system_zlib" = yes; then
      AC_CHECK_LIB(z, deflate, ZLIBSPEC=-lz, ZLIBSPEC=)
   fi

   # Test for Gtk stuff, if asked for.
   if test "$use_gtk_awt" = yes; then
      PKG_CHECK_MODULES(GTK, gtk+-2.0 >= 2.4)
      AC_SUBST(GTK_CFLAGS)
      AC_SUBST(GTK_LIBS)

      PKG_CHECK_MODULES(GLIB, glib-2.0 >= 2.4 gthread-2.0 >= 2.4)
      AC_SUBST(GLIB_CFLAGS)
      AC_SUBST(GLIB_LIBS)

      PKG_CHECK_MODULES(LIBART, libart-2.0 >= 2.1)
      AC_SUBST(LIBART_CFLAGS)
      AC_SUBST(LIBART_LIBS)

      # We require the XTest Extension to support java.awt.Robot.
      AC_CHECK_LIB([Xtst], [XTestQueryExtension], [true],
		   [AC_MSG_ERROR([libXtst not found, required by java.awt.Robot])],
		   [${X_LIBS}])
   fi

   # On Solaris, and maybe other architectures, the Boehm collector
   # requires -ldl.
   if test "$GC" = boehm; then
      case "${host}" in
	  *-*-darwin*)
	    ;;
	  *)
            AC_CHECK_LIB(dl, main, SYSTEMSPEC="$SYSTEMSPEC -ldl")
	    ;;
      esac
   fi
fi

# Use a semicolon as CLASSPATH separator for MinGW, otherwise a colon.
case $build in
    *-mingw32) CLASSPATH_SEPARATOR=';' ;;
    *)         CLASSPATH_SEPARATOR=':' ;;
esac
AC_SUBST(CLASSPATH_SEPARATOR)

# We must search the source tree for java.lang, since we still don't
# have libgcj.jar nor java/lang/*.class
GCJ_SAVE_CPPFLAGS=$CPPFLAGS
CPPFLAGS="$CPPFLAGS -I`${PWDCMD-pwd}` -I`cd $srcdir && ${PWDCMD-pwd}`"

# Since some classes depend on this one, we need its source available
# before we can do any GCJ compilation test :-(
if test ! -f gnu/classpath/Configuration.java; then
  test -d gnu || mkdir gnu
  test -d gnu/classpath || mkdir gnu/classpath
  # Note that it is not crucial that all the values here be correct.
  sed -e "s,@prefix@,$expanded_prefix," \
      -e "s,@VERSION@,$VERSION," \
      -e "s,@LIBDEBUG@,false," \
      -e "s,@INIT_LOAD_LIBRARY@,false," \
      -e "s,@@,$LIBGCJDEBUG," \
      -e "s,@default_toolkit@,$TOOLKIT," \
      -e "s,@JAVA_LANG_SYSTEM_EXPLICIT_INITIALIZATION@,false," \
      -e "s,@GTK_CAIRO_ENABLED@,false," \
      -e "s,@ECJ_JAR@,," \
      -e "s,@WANT_NATIVE_BIG_INTEGER@,false," \
	< $srcdir/classpath/gnu/classpath/Configuration.java.in \
	> gnu/classpath/Configuration.java
  # We do not want to redirect the output of the grep below to /dev/null,
  # but we add /dev/null to the input list so that grep will print the
  # filename of Configuration.java in case it finds any matches.
  if grep '@.*@' gnu/classpath/Configuration.java /dev/null; then
    AC_MSG_ERROR([configure.ac is missing the substitutions above])
  fi
fi

LT_AC_PROG_GCJ

# Now remove it.
rm -f gnu/classpath/Configuration.java

CPPFLAGS=$GCJ_SAVE_CPPFLAGS

AC_CHECK_SIZEOF(void *)

AC_C_BIGENDIAN

ZLIBS=
SYS_ZLIBS=
ZINCS=

if test -z "$ZLIBSPEC"; then
   # Use zlib from the GCC tree.
   ZINCS='-I$(top_srcdir)/../zlib'
   ZLIBS=../zlib/libzgcj_convenience.la
else
   # System's zlib.
   SYS_ZLIBS="$ZLIBSPEC"
fi
AC_SUBST(ZLIBS)
AC_SUBST(SYS_ZLIBS)
AC_SUBST(ZINCS)
AC_SUBST(DIVIDESPEC)
AC_SUBST(CHECKREFSPEC)
AC_SUBST(EXCEPTIONSPEC)
AC_SUBST(BACKTRACESPEC)
AC_SUBST(IEEESPEC)
AC_SUBST(ATOMICSPEC)

AM_CONDITIONAL(NATIVE, test "$NATIVE" = yes)
AM_CONDITIONAL(ENABLE_SHARED, test "$enable_shared" = yes)
AM_CONDITIONAL(NEEDS_DATA_START, test "$NEEDS_DATA_START" = yes && test "$NATIVE" = yes)
AM_CONDITIONAL(INSTALL_BINARIES, test -z "${with_multisubdir}")
AC_SUBST(LIBGCC_UNWIND_INCLUDE)

# Process the option "--enable-version-specific-runtime-libs"
# Calculate toolexeclibdir
case ${version_specific_libs} in
  yes)
    # Need the gcc compiler version to know where to install libraries
    # and header files if --enable-version-specific-runtime-libs option
    # is selected.
    includedir='$(libdir)/gcc/$(target_noncanonical)/$(gcc_version)/include/'
    toolexecdir='$(libdir)/gcc/$(target_noncanonical)'
    toolexecmainlibdir='$(toolexecdir)/$(gcc_version)$(MULTISUBDIR)'
    toolexeclibdir=$toolexecmainlibdir
    ;;
  no)
    if test -n "$with_cross_host" &&
       test x"$with_cross_host" != x"no"; then
      # Install a library built with a cross compiler in tooldir, not libdir.
      toolexecdir='$(exec_prefix)/$(target_noncanonical)'
      toolexecmainlibdir='$(toolexecdir)/lib'
    else
      toolexecdir='$(libdir)/gcc-lib/$(target_noncanonical)'
      toolexecmainlibdir='$(libdir)'
    fi
    multi_os_directory=`$CC -print-multi-os-directory`
    case $multi_os_directory in
      .) toolexeclibdir=$toolexecmainlibdir ;; # Avoid trailing /.
      *) toolexeclibdir=$toolexecmainlibdir/$multi_os_directory ;;
    esac
    ;;
esac
AC_SUBST(toolexecdir)
AC_SUBST(toolexecmainlibdir)
AC_SUBST(toolexeclibdir)

# Determine gcj and libgcj version number.
gcjversion=`cat "$srcdir/../gcc/BASE-VER"`
libgcj_soversion=`awk -F: '/^[[^#]].*:/ { print $1 }' $srcdir/libtool-version`
GCJVERSION=$gcjversion
AC_SUBST(GCJVERSION)
AC_DEFINE_UNQUOTED(GCJVERSION, "$GCJVERSION", [Short GCJ version ID])

# Determine where the standard .db file and GNU Classpath JNI
# libraries are found.
gcjsubdir=gcj-$gcjversion-$libgcj_soversion
multi_os_directory=`$CC -print-multi-os-directory`
case $multi_os_directory in
  .)
   dbexecdir='$(libdir)/'$gcjsubdir # Avoid /.
   ;;
  *)
   dbexecdir='$(libdir)/'$multi_os_directory/$gcjsubdir
   ;;
esac
AC_SUBST(dbexecdir)
AC_SUBST(gcjsubdir)

AC_DEFINE(JV_VERSION, "1.5.0", [Compatibility version string])
AC_DEFINE(JV_API_VERSION, "1.5", [API compatibility version string])

TL_AC_GXX_INCLUDE_DIR

# We check for sys/filio.h because Solaris 2.5 defines FIONREAD there.
# On that system, sys/ioctl.h will not include sys/filio.h unless
# BSD_COMP is defined; just including sys/filio.h is simpler.
# We avoid AC_HEADER_DIRENT since we really only care about dirent.h
# for now.  If you change this, you also must update natFile.cc.
AC_CHECK_HEADERS([unistd.h sys/time.h sys/types.h fcntl.h \
		  sys/ioctl.h sys/filio.h sys/stat.h sys/select.h \
		  sys/socket.h netinet/in.h arpa/inet.h netdb.h \
		  pwd.h sys/config.h stdint.h langinfo.h locale.h \
		  dirent.h sys/rw_lock.h magic.h ifaddrs.h])

# sys/socket.h is a prerequisite for net/if.h on Solaris.
AC_CHECK_HEADERS(net/if.h, [], [], [
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
])

AC_CHECK_HEADERS(inttypes.h, [
    AC_DEFINE(HAVE_INTTYPES_H, 1, [Define if <inttypes.h> is available])
    AC_DEFINE(JV_HAVE_INTTYPES_H, 1, [Define if <inttypes.h> is available])
])

AC_CHECK_TYPE([ssize_t], [int])
AC_CHECK_TYPE([magic_t], [
    AC_DEFINE(HAVE_MAGIC_T, 1, [Define if magic.h declares magic_t])], [], [
#ifdef HAVE_MAGIC_H
#include <magic.h>
#endif])

AC_MSG_CHECKING([for in_addr_t])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <sys/types.h>
#if STDC_HEADERS
#include <stdlib.h>
#include <stddef.h>
#endif
#if HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif]], [[in_addr_t foo;]])],
  [AC_DEFINE(HAVE_IN_ADDR_T, 1,
     [Define to 1 if 'in_addr_t' is defined in sys/types.h or netinet/in.h.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)])

AC_MSG_CHECKING([whether struct ip_mreq is in netinet/in.h])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>]], [[struct ip_mreq mreq;]])],
  [AC_DEFINE(HAVE_STRUCT_IP_MREQ, 1,
     [Define if struct ip_mreq is defined in netinet/in.h.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)])

AC_MSG_CHECKING([whether struct ipv6_mreq is in netinet/in.h])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>]], [[struct ipv6_mreq mreq6;]])],
  [AC_DEFINE(HAVE_STRUCT_IPV6_MREQ, 1,
     [Define if struct ipv6_mreq is defined in netinet/in.h.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)])

AC_MSG_CHECKING([whether struct sockaddr_in6 is in netinet/in.h])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <netinet/in.h>]], [[struct sockaddr_in6 addr6;]])],
  [AC_DEFINE(HAVE_INET6, 1,
     [Define if inet6 structures are defined in netinet/in.h.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)])

AC_MSG_CHECKING([for socklen_t in sys/socket.h])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#define _POSIX_PII_SOCKET
#include <sys/types.h>
#include <sys/socket.h>]], [[socklen_t x = 5;]])],
  [AC_DEFINE(HAVE_SOCKLEN_T, 1, [Define it socklen_t typedef is in sys/socket.h.])
   AC_MSG_RESULT(yes)],
  [AC_MSG_RESULT(no)])

AC_MSG_CHECKING([for tm_gmtoff in struct tm])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#include <time.h>]], [[struct tm tim; tim.tm_gmtoff = 0;]])],
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

AC_CHECK_PROGS(PERL, perl, false)

SYSDEP_SOURCES=
SIGNAL_HANDLER_AUX=

case "${host}" in
 *-*-solaris2*)
    SIGNAL_HANDLER=include/posix-signal.h
    ;;
 i?86-*-linux*)
    SIGNAL_HANDLER=include/i386-signal.h
    SIGNAL_HANDLER_AUX=include/x86_64-signal.h
    ;;
# ia64-*)
#    SYSDEP_SOURCES=sysdep/ia64.c
#    test -d sysdep || mkdir sysdep
#    ;;
 hppa*-*-linux*)
    SIGNAL_HANDLER=include/pa-signal.h
    ;;
 hppa*-hp-hpux*)
    SIGNAL_HANDLER=include/hppa-signal.h
    ;;
 ia64-*-linux*)
    SIGNAL_HANDLER=include/dwarf2-signal.h
    ;;
 powerpc*-*-linux*)
    SIGNAL_HANDLER=include/powerpc-signal.h
    ;;
 alpha*-*-linux*)
    SIGNAL_HANDLER=include/dwarf2-signal.h
    ;;
 s390*-*-linux*)
    SIGNAL_HANDLER=include/s390-signal.h
    ;;
 x86_64*-*-linux*)
    SIGNAL_HANDLER=include/x86_64-signal.h
    SIGNAL_HANDLER_AUX=include/i386-signal.h
    ;;
 sparc*-*-linux*)
    SIGNAL_HANDLER=include/dwarf2-signal.h
    ;;
 sh*-*-linux*)
    SIGNAL_HANDLER=include/sh-signal.h
    ;;
 *mingw*)
    SIGNAL_HANDLER=include/win32-signal.h
    ;;
 mips*-*-linux*)
    SIGNAL_HANDLER=include/mips-signal.h
    ;;
 m68*-*-linux*)
    SIGNAL_HANDLER=include/dwarf2-signal.h
    ;;
 powerpc*-*-darwin* | i?86-*-darwin[[912]]* | x86_64-*-darwin[[912]]*)
    SIGNAL_HANDLER=include/darwin-signal.h
    ;;
 powerpc*-*-aix*)
    SIGNAL_HANDLER=include/aix-signal.h
    ;;
 *)
    SIGNAL_HANDLER=include/default-signal.h
    ;;
esac

# If we're using sjlj exceptions, forget what we just learned.
if test "$enable_sjlj_exceptions" = yes; then
   SIGNAL_HANDLER=include/default-signal.h
   SIGNAL_HANDLER_AUX=
fi

AC_SUBST(SYSDEP_SOURCES)

if test -z "$SIGNAL_HANDLER_AUX"; then
  SIGNAL_HANDLER_AUX=$SIGNAL_HANDLER
fi

AC_CONFIG_LINKS(include/java-signal.h:$SIGNAL_HANDLER
		include/java-signal-aux.h:$SIGNAL_HANDLER_AUX)

if test "${multilib}" = "yes"; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

# See if we support thread-local storage.
GCC_CHECK_TLS

# For _Unwind_GetIPInfo.
GCC_CHECK_UNWIND_GETIPINFO

# See if linker supports anonymous version scripts.
AC_CACHE_CHECK([whether ld supports anonymous version scripts],
  [libjava_cv_anon_version_script],
  [AC_ARG_ENABLE(symvers,
     AS_HELP_STRING([--disable-symvers],
		    [disable symbol versioning for libjava]),
       [case "$enableval" in
         yes|gnu*) libjava_cv_anon_version_script=yes ;;
         no)  libjava_cv_anon_version_script=no ;;
         *)   AC_MSG_ERROR([Unknown argument to enable/disable symvers]);;
        esac],
       [libjava_cv_anon_version_script=yes]
   )
   if test x$libjava_cv_anon_version_script = xyes; then
     save_CFLAGS="$CFLAGS"; save_LDFLAGS="$LDFLAGS"
     CFLAGS="$CFLAGS -fPIC";
     LDFLAGS="$LDFLAGS -shared -Wl,--version-script,conftest.map"
     echo '{ global: globalsymb*; local: *; };' > conftest.map
     AC_TRY_LINK(void globalsymbol (void) {} void localsymbol (void) {},,
	         [libjava_cv_anon_version_script=gnu],
		 [libjava_cv_anon_version_script=no])
     if test x$libjava_cv_anon_version_script = xno; then
       case "$target_os" in
         solaris2*)
           LDFLAGS="$save_LDFLAGS"
           LDFLAGS="$LDFLAGS -shared -Wl,-M,conftest.map"
           # Sun ld doesn't understand wildcards here.
           echo '{ global: globalsymbol; local: *; };' > conftest.map
           AC_TRY_LINK(void globalsymbol (void) {} void localsymbol (void) {},,
	 	       [libjava_cv_anon_version_script=sun], [])
           ;;
       esac
     fi
     CFLAGS="$save_CFLAGS"; LDFLAGS="$save_LDFLAGS"
   fi
  ])
AM_CONDITIONAL(ANONVERSCRIPT, test "$libjava_cv_anon_version_script" != no)
AM_CONDITIONAL(ANONVERSCRIPT_GNU, test "$libjava_cv_anon_version_script" = gnu)
AM_CONDITIONAL(ANONVERSCRIPT_SUN, test "$libjava_cv_anon_version_script" = sun)

# Check if linker supports static linking on a per library basis
LD_START_STATIC_SPEC=
LD_FINISH_STATIC_SPEC=
if $LD --help 2>&1 | grep -- -call_shared >/dev/null 2>&1; then
  if $LD --help 2>&1 | grep -- -non_shared >/dev/null 2>&1; then
    LD_START_STATIC_SPEC='%{static-libgcj:-non_shared}'
    LD_FINISH_STATIC_SPEC='%{static-libgcj:-call_shared}'
  fi
fi
AC_SUBST(LD_START_STATIC_SPEC)
AC_SUBST(LD_FINISH_STATIC_SPEC)

here=`${PWDCMD-pwd}`
AC_SUBST(here)

# We get this from the environment.
AC_SUBST(GCJFLAGS)

default_python_dir="/share/gcc-`cat ${srcdir}/../gcc/BASE-VER`/python/libjava"
AC_ARG_WITH([python-dir],
            AS_HELP_STRING([--with-python-dir],
                           [the location to install Python modules. This path should NOT include the prefix.]),
            [with_python_dir=$withval], [with_python_dir="${default_python_dir}"])

# Needed for installing Python modules during make install.
python_mod_dir="\${prefix}${with_python_dir}"

# Needed for substituting into aot-compile*
python_mod_dir_expanded="${expanded_prefix}${with_python_dir}"

AC_MSG_RESULT(Python modules dir: ${python_mod_dir_expanded});
AC_SUBST(python_mod_dir)
AC_SUBST(python_mod_dir_expanded)

# needed for aot-compile-rpm
MAKE=`which make`
AC_SUBST(MAKE)

AC_ARG_ENABLE([aot-compile-rpm],
  [AS_HELP_STRING([--enable-aot-compile-rpm],
                 [enable installation of aot-compile-rpm [default=no]])],
  [case "${enableval}" in
    yes) AOT_RPM_ENABLED=yes ;;
    no)  AOT_RPM_ENABLED=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable aot-compile-rpm]) ;;
  esac],
  [AOT_RPM_ENABLED=no]
)
AM_CONDITIONAL(INSTALL_AOT_RPM, test "x${AOT_RPM_ENABLED}" = xyes)

AC_ARG_ENABLE([java-home],
  [AS_HELP_STRING([--enable-java-home],
                 [create a standard JDK-style directory layout in the install tree [default=no]])],
  [case "${enableval}" in
    yes) JAVA_HOME_ENABLED=yes ;;
    no)  JAVA_HOME_ENABLED=no ;;
    *)   AC_MSG_ERROR([Unknown argument to enable/disable java-home]) ;;
  esac],
  [JAVA_HOME_ENABLED=no]
)
AM_CONDITIONAL(CREATE_JAVA_HOME, test "x${JAVA_HOME_ENABLED}" = xyes)

# Only do these checks if java-home above is enabled.
if test "x${JAVA_HOME_ENABLED}" = xyes
then

  AC_ARG_WITH([gcc-suffix],
              AS_HELP_STRING([--with-gcc-suffix],
                             [the GCC tool suffix (defaults to empty string)]),
              [gcc_suffix=$withval], [gcc_suffix=""])
  AC_MSG_RESULT(GCC suffix: ${gcc_suffix})
  AC_SUBST(gcc_suffix)

  AC_ARG_WITH([arch-directory],
              AS_HELP_STRING([--with-arch-directory],
                             [the arch directory under jre/lib (defaults to auto-detect)]),
              [host_cpu=$withval], [host_cpu=""])

  AC_ARG_WITH([os-directory],
              AS_HELP_STRING([--with-os-directory],
                             [the os directory under include (defaults to auto-detect)]),
              [host_os=$withval], [host_os=""])

  AC_ARG_WITH([origin-name],
              AS_HELP_STRING([--with-origin-name],
                             [the JPackage origin name of this package (default is gcj${gcc_suffix}]),
              [origin_name=$withval], [origin_name=gcj${gcc_suffix}])
  AC_MSG_RESULT(JPackage origin name: ${origin_name})

  AC_ARG_WITH([arch-suffix],
              AS_HELP_STRING([--with-arch-suffix],
                             [the arch directory suffix (default is the empty string]),
              [arch_suffix=$withval], [arch_suffix=""])
  AC_MSG_RESULT(arch suffix: ${arch_suffix})

  AC_ARG_WITH([jvm-root-dir],
              AS_HELP_STRING([--with-jvm-root-dir],
                             [where to install SDK (default is ${prefix}/lib/jvm)]),
              [jvm_root_dir=$withval], [jvm_root_dir="\${prefix}/lib/jvm"])
  AC_MSG_RESULT(JVM root installation directory: ${jvm_root_dir})

  AC_ARG_WITH([jvm-jar-dir],
              AS_HELP_STRING([--with-jvm-jar-dir],
			                 [where to install jars (default is ${prefix}/lib/jvm-exports)]),
              [jvm_jar_dir=$withval], [jvm_jar_dir=\${prefix}/lib/jvm-exports])
  AC_MSG_RESULT(JAR root installation directory: ${jvm_jar_dir})

  JAVA_VERSION=1.5.0
  BUILD_VERSION=0
  AC_SUBST(JAVA_VERSION)
  AC_SUBST(BUILD_VERSION)
  AC_MSG_RESULT(Java version: ${JAVA_VERSION})

  jre_dir=jre
  jre_lnk=jre-${JAVA_VERSION}-${origin_name}
  sdk_lnk=java-${JAVA_VERSION}-${origin_name}

  JVM_ROOT_DIR=${jvm_root_dir}
  AC_SUBST(JVM_ROOT_DIR)
  AC_MSG_RESULT(JVM ROOT directory: ${JVM_ROOT_DIR})

  JVM_JAR_ROOT_DIR=${jvm_jar_dir}
  AC_SUBST(JVM_JAR_ROOT_DIR)
  AC_MSG_RESULT(JVM JAR ROOT directory: ${JVM_JAR_ROOT_DIR})

  JVM_JAR_DIR=${jvm_jar_dir}
  AC_SUBST(JVM_JAR_DIR)
  AC_MSG_RESULT(JVM JAR directory: ${JVM_JAR_DIR})

  JRE_DIR=${jre_dir}
  AC_SUBST(JRE_DIR)

  JRE_LNK=${jre_lnk}
  AC_SUBST(JRE_LNK)

  SDK_LNK=${sdk_lnk}
  AC_SUBST(SDK_LNK)

  SDK_BIN_DIR=${jvm_root_dir}/bin
  AC_SUBST(SDK_BIN_DIR)
  AC_MSG_RESULT(SDK tools directory: ${SDK_BIN_DIR})

  SDK_LIB_DIR=${jvm_root_dir}/lib
  AC_SUBST(SDK_LIB_DIR)
  AC_MSG_RESULT(SDK jar directory: ${SDK_LIB_DIR})

  SDK_INCLUDE_DIR=${jvm_root_dir}/include
  AC_SUBST(SDK_INCLUDE_DIR)
  AC_MSG_RESULT(SDK include directory: ${SDK_INCLUDE_DIR})

  JRE_BIN_DIR=${jvm_root_dir}/${jre_dir}/bin
  AC_SUBST(JRE_BIN_DIR)
  AC_MSG_RESULT(JRE tools directory: ${JRE_BIN_DIR})

  JRE_LIB_DIR=${jvm_root_dir}/${jre_dir}/lib
  AC_SUBST(JRE_LIB_DIR)
  AC_MSG_RESULT(JRE lib directory: ${JRE_LIB_DIR})

  # Find gcj prefix using gcj found in PATH.
  gcj_prefix=`which gcj${gcc_suffix} | sed "s%/bin/gcj${gcc_suffix}%%"`

  # Where do the gcj binaries live?
  # For jhbuild based builds, they all live in a sibling of bin called
  # gcj-bin.  Check for gcj-bin first, and use bin otherwise.
  GCJ_BIN_DIR=`if test -d ${gcj_prefix}/gcj-bin; then echo ${gcj_prefix}/gcj-bin; else echo ${gcj_prefix}/bin; fi`
  AC_SUBST(GCJ_BIN_DIR)
  AC_MSG_RESULT(GCJ tools directory: ${GCJ_BIN_DIR})

  AC_MSG_RESULT(host is ${host})
  if test "x${host_cpu}" = "x"
  then
    case ${host} in
      *-mingw* | *-cygwin*)
        host_cpu=x86;;
      i486-* | i586-* | i686-*)
        host_cpu=i386;;
      x86_64-*)
        host_cpu=amd64;;
      *)
        host_cpu=${host_cpu};;
    esac
  fi
  AC_MSG_RESULT(arch directory: ${host_cpu})
  CPU=${host_cpu}
  AC_SUBST(CPU)

  if test "x${host_os}" = "x"
  then
    case ${host} in
      *-mingw* | *-cygwin*)
        host_os=win32;;
      *-linux*)
        host_os=linux;;
      *)
        host_os=${host_os};;
    esac
  fi
  echo os directory: ${host_os}
  OS=${host_os}
  AC_SUBST(OS)

  # make an expanded $libdir, for substituting into
  # scripts (and other non-Makefile things).
  LIBDIR=$libdir
  if test "x${exec_prefix}" = "xNONE"
  then
      lib_exec_prefix=$expanded_prefix
  else
      lib_exec_prefix=$exec_prefix
  fi
  LIBDIR=`echo $libdir | sed "s:\\\${exec_prefix}:$lib_exec_prefix:g"`
  AC_SUBST(LIBDIR)
fi

AC_CONFIG_FILES([
Makefile
libgcj.pc
libgcj.spec
libgcj-test.spec
gcj/Makefile
include/Makefile
testsuite/Makefile
contrib/aotcompile.py
contrib/aot-compile
contrib/aot-compile-rpm
contrib/generate-cacerts.pl
contrib/rebuild-gcj-db
])

if test ${multilib} = yes; then
  multilib_arg="--enable-multilib"
else
  multilib_arg=
fi

AC_CONFIG_FILES([scripts/jar], [chmod +x scripts/jar])
AC_OUTPUT
