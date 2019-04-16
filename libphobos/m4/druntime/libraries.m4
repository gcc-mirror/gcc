#
# Contains macros to handle library dependencies.
#


# DRUNTIME_LIBRARIES_THREAD
# -------------------------
# Allow specifying the thread library to link with or autodetect
# Add thread library to LIBS if necessary.
AC_DEFUN([DRUNTIME_LIBRARIES_THREAD],
[
  enable_thread_lib=yes
  AC_ARG_ENABLE(thread-lib,
    AC_HELP_STRING([--enable-thread-lib=<arg>],
                   [specify linker option for the system thread library (default: autodetect)]))

  AS_IF([test "x$enable_thread_lib" = "xyes"], [
    AC_SEARCH_LIBS([pthread_create], [pthread])
  ], [
    AS_IF([test "x$enable_thread_lib" = "xno"], [
      AC_MSG_CHECKING([for thread library])
      AC_MSG_RESULT([disabled])
    ], [
      AC_CHECK_LIB([$enable_thread_lib], [pthread_create], [], [
        AC_MSG_ERROR([Thread library not found])
      ])
    ])
  ])
])


# DRUNTIME_LIBRARIES_DLOPEN
# -----------------------
# Autodetect and add dl library to LIBS if necessary.
AC_DEFUN([DRUNTIME_LIBRARIES_DLOPEN],
[
  # Libtool has already checked this, so re-use the inferred dlopen lib.
  AS_IF([test "x$enable_dlopen" = "xyes" && test -n "$lt_cv_dlopen_libs"], [
    LIBS="$LIBS $lt_cv_dlopen_libs"
  ], [
  ])
])


# DRUNTIME_LIBRARIES_NET
# -----------------------
# Autodetect and add networking library to LIBS if necessary.
AC_DEFUN([DRUNTIME_LIBRARIES_NET],
[
  dnl Test for -lsocket and -lnsl.  Copied from libjava/configure.ac.
  AC_CACHE_CHECK([for socket libraries], druntime_cv_lib_sockets,
    [druntime_cv_lib_sockets=
     druntime_check_both=no
     AC_CHECK_FUNC(connect, druntime_check_socket=no, druntime_check_socket=yes)
     if test "$druntime_check_socket" = "yes"; then
       unset ac_cv_func_connect
       AC_CHECK_LIB(socket, main, druntime_cv_lib_sockets="-lsocket",
		    druntime_check_both=yes)
     fi
     if test "$druntime_check_both" = "yes"; then
       druntime_old_libs=$LIBS
       LIBS="$LIBS -lsocket -lnsl"
       unset ac_cv_func_accept
       AC_CHECK_FUNC(accept,
		     [druntime_check_nsl=no
		      druntime_cv_lib_sockets="-lsocket -lnsl"])
       unset ac_cv_func_accept
       LIBS=$druntime_old_libs
     fi
     unset ac_cv_func_gethostbyname
     druntime_old_libs="$LIBS"
     AC_CHECK_FUNC(gethostbyname, ,
		   [AC_CHECK_LIB(nsl, main,
		    [druntime_cv_lib_sockets="$druntime_cv_lib_sockets -lnsl"])])
  ])
  LIBS="$LIBS $druntime_cv_lib_sockets"
])

# DRUNTIME_LIBRARIES_ZLIB
# -----------------------
# Allow specifying whether to use the system zlib or
# compiling the zlib included in GCC.  Adds substitute
# for LIBZ or adds zlib to LIBS if necessary.
AC_DEFUN([DRUNTIME_LIBRARIES_ZLIB],
[
  AC_LANG_PUSH([C])
  LIBZ=""

  AC_ARG_WITH(target-system-zlib,
    AS_HELP_STRING([--with-target-system-zlib={yes,no,auto}],
                   [use installed libz (default: no)]),,
              [with_target_system_zlib=no])

  case "$with_target_system_zlib" in
    yes|no|auto) ;;
    *) AC_MSG_ERROR([Invalid argument for --with-target-system-zlib]) ;;
  esac

  AC_MSG_CHECKING([for system zlib])
  save_LIBS=$LIBS
  LIBS="$LIBS -lz"
  dnl the link test is not good enough for ARM32 multilib detection,
  dnl first check to link, then to run
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM([#include <zlib.h>],[gzopen("none", "rb")])],
    [
      AC_RUN_IFELSE([AC_LANG_SOURCE([[
        #include <zlib.h>
        int main() {
          gzFile file = gzopen("none", "rb");
          return 0;
        }
        ]])],
        [system_zlib_found=yes],
        [system_zlib_found=no],
        dnl no system zlib for cross builds ...
        [system_zlib_found=no]
      )
    ],
    [system_zlib_found=no])
  LIBS=$save_LIBS

  if test x$system_zlib_found = xyes && test x$with_target_system_zlib != xno; then
    AC_MSG_RESULT([found])
    LIBS="$LIBS -lz"
  elif test x$system_zlib_found = xno && test x$with_target_system_zlib = xyes; then
    AC_MSG_ERROR([system zlib required but not found])
  else
    AC_MSG_RESULT([just compiled])
    LIBZ=../../zlib/libz_convenience.la
  fi

  AC_SUBST(LIBZ)
  AC_LANG_POP([C])
])

# DRUNTIME_LIBRARIES_ATOMIC
# -------------------------
# Allow specifying whether to use libatomic for atomic support.
AC_DEFUN([DRUNTIME_LIBRARIES_ATOMIC],
[
  AC_ARG_WITH(libatomic,
    AS_HELP_STRING([--without-libatomic],
                   [Do not use libatomic in core.atomic (default: auto)]))

  DCFG_HAVE_LIBATOMIC=false
  LIBATOMIC=
  AS_IF([test "x$with_libatomic" != "xno"], [
    DCFG_HAVE_LIBATOMIC=true
    LIBATOMIC=../../libatomic/libatomic_convenience.la
  ], [
    AC_MSG_CHECKING([for libatomic])
    AC_MSG_RESULT([disabled])
  ])

  AC_SUBST(DCFG_HAVE_LIBATOMIC)
  AC_SUBST(LIBATOMIC)
])

# DRUNTIME_LIBRARIES_BACKTRACE
# ---------------------------
# Allow specifying whether to use libbacktrace for backtrace support.
# Adds subsitute for BACKTRACE_SUPPORTED, BACKTRACE_USES_MALLOC,
# and BACKTRACE_SUPPORTS_THREADS.
AC_DEFUN([DRUNTIME_LIBRARIES_BACKTRACE],
[
  AC_LANG_PUSH([C])
  BACKTRACE_SUPPORTED=false
  BACKTRACE_USES_MALLOC=false
  BACKTRACE_SUPPORTS_THREADS=false
  LIBBACKTRACE=""

  AC_ARG_WITH(libbacktrace,
    AS_HELP_STRING([--without-libbacktrace],
                   [Do not use libbacktrace in core.runtime (default: auto)]))

  AS_IF([test "x$with_libbacktrace" != "xno"], [
    LIBBACKTRACE=../../libbacktrace/libbacktrace.la

    gdc_save_CPPFLAGS=$CPPFLAGS
    CPPFLAGS="$CPPFLAGS -I../libbacktrace "

    AC_CHECK_HEADER(backtrace-supported.h, have_libbacktrace_h=true,
      have_libbacktrace_h=false)

    if $have_libbacktrace_h; then
      AC_MSG_CHECKING([libbacktrace: BACKTRACE_SUPPORTED])
      AC_EGREP_CPP(FOUND_LIBBACKTRACE_RESULT_GDC,
      [
      #include <backtrace-supported.h>
      #if BACKTRACE_SUPPORTED
        FOUND_LIBBACKTRACE_RESULT_GDC
      #endif
      ], BACKTRACE_SUPPORTED=true, BACKTRACE_SUPPORTED=false)
      AC_MSG_RESULT($BACKTRACE_SUPPORTED)

      AC_MSG_CHECKING([libbacktrace: BACKTRACE_USES_MALLOC])
      AC_EGREP_CPP(FOUND_LIBBACKTRACE_RESULT_GDC,
      [
      #include <backtrace-supported.h>
      #if BACKTRACE_USES_MALLOC
        FOUND_LIBBACKTRACE_RESULT_GDC
      #endif
      ], BACKTRACE_USES_MALLOC=true, BACKTRACE_USES_MALLOC=false)
      AC_MSG_RESULT($BACKTRACE_USES_MALLOC)

      AC_MSG_CHECKING([libbacktrace: BACKTRACE_SUPPORTS_THREADS])
      AC_EGREP_CPP(FOUND_LIBBACKTRACE_RESULT_GDC,
      [
      #include <backtrace-supported.h>
      #if BACKTRACE_SUPPORTS_THREADS
        FOUND_LIBBACKTRACE_RESULT_GDC
      #endif
      ], BACKTRACE_SUPPORTS_THREADS=true, BACKTRACE_SUPPORTS_THREADS=false)
      AC_MSG_RESULT($BACKTRACE_SUPPORTS_THREADS)
    fi
    CPPFLAGS=$gdc_save_CPPFLAGS
  ], [
    AC_MSG_CHECKING([for libbacktrace])
    AC_MSG_RESULT([disabled])
  ])

  AC_SUBST(LIBBACKTRACE)
  AC_SUBST(BACKTRACE_SUPPORTED)
  AC_SUBST(BACKTRACE_USES_MALLOC)
  AC_SUBST(BACKTRACE_SUPPORTS_THREADS)
  AC_LANG_POP([C])
])

# DRUNTIME_LIBRARIES_CLIB
# -----------------------
# Perform various feature checks on the C library.
AC_DEFUN([DRUNTIME_LIBRARIES_CLIB],
[
  AC_LANG_PUSH([C])
  DCFG_HAVE_QSORT_R=false
  AC_CHECK_FUNC(qsort_r, [DCFG_HAVE_QSORT_R=true])
  AC_SUBST(DCFG_HAVE_QSORT_R)
  AC_LANG_POP([C])
])
