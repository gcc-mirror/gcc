#
# Contains macros to detect OS features.
#


# DRUNTIME_OS_THREAD_MODEL
# ------------------------
# Detect thread model and substitute DCFG_THREAD_MODEL
AC_DEFUN([DRUNTIME_OS_THREAD_MODEL],
[
  AC_REQUIRE([AC_PROG_GDC])
  AC_MSG_CHECKING([for thread model used by GDC])
  d_thread_model=`$GDC -v 2>&1 | sed -n 's/^Thread model: //p'`
  AC_MSG_RESULT([$d_thread_model])

  # Map from thread model to thread interface.
  DRUNTIME_CONFIGURE_THREADS([$d_thread_model])
])


# DRUNTIME_CONFIGURE_THREADS(thread_model)
# ----------------------------------------
# Map target os to D version identifier
AC_DEFUN([DRUNTIME_CONFIGURE_THREADS],
[
case $1 in
    aix)    DCFG_THREAD_MODEL="Posix" ;;
    lynx)   DCFG_THREAD_MODEL="Posix" ;;
    posix)  DCFG_THREAD_MODEL="Posix" ;;
    single) DCFG_THREAD_MODEL="Single" ;;
    win32)  DCFG_THREAD_MODEL="Win32" ;;
    # TODO: These targets need porting.
    dce|mipssde|rtems|tpf|vxworks)
	    DCFG_THREAD_MODEL="Single" ;;
    *)	    as_fn_error "Thread implementation '$1' not recognised" "$LINENO" 5 ;;
esac
AC_SUBST(DCFG_THREAD_MODEL)
])


# DRUNTIME_OS_DETECT
# ------------------
# Set the druntime_cv_target_os variable
AC_DEFUN([DRUNTIME_OS_DETECT],
[
  AC_CACHE_CHECK([[for target OS]],
    [[druntime_cv_target_os]],
    [[druntime_cv_target_os=`echo $target_os | sed 's/^\([A-Za-z_]+\)/\1/'`]])
    AS_IF([[test -z "$druntime_cv_target_os"]],
      [AC_MSG_ERROR([[can't detect target OS]])],
      [])
])


# DRUNTIME_OS_SOURCES
# -------------------
# Detect target OS and add DRUNTIME_OS_AIX DRUNTIME_OS_DARWIN
# DRUNTIME_OS_FREEBSD DRUNTIME_OS_LINUX DRUNTIME_OS_MINGW
# DRUNTIME_OS_SOLARIS DRUNTIME_OS_OPENBSD conditionals.
# If the system is posix, add DRUNTIME_OS_POSIX conditional.
AC_DEFUN([DRUNTIME_OS_SOURCES],
[
  AC_REQUIRE([DRUNTIME_OS_DETECT])

  druntime_target_os_parsed=""
  case "$druntime_cv_target_os" in
      aix*)    druntime_target_os_parsed="aix"
               ;;
      *android*)
               druntime_target_os_parsed="android"
               ;;
      darwin*) druntime_target_os_parsed="darwin"
               ;;
      dragonfly*)
               druntime_target_os_parsed="dragonflybsd"
               ;;
      freebsd*|k*bsd*-gnu)
               druntime_target_os_parsed="freebsd"
               ;;
      openbsd*)
               druntime_target_os_parsed="openbsd"
               ;;
      netbsd*)
               druntime_target_os_parsed="netbsd"
               ;;
      linux*)  druntime_target_os_parsed="linux"
               ;;
      mingw*)  druntime_target_os_parsed="mingw"
             ;;
      *solaris*) druntime_target_os_parsed="solaris"
  esac
  AM_CONDITIONAL([DRUNTIME_OS_AIX],
                 [test "$druntime_target_os_parsed" = "aix"])
  AM_CONDITIONAL([DRUNTIME_OS_ANDROID],
                 [test "$druntime_target_os_parsed" = "android"])
  AM_CONDITIONAL([DRUNTIME_OS_DARWIN],
                 [test "$druntime_target_os_parsed" = "darwin"])
  AM_CONDITIONAL([DRUNTIME_OS_DRAGONFLYBSD],
                 [test "$druntime_target_os_parsed" = "dragonflybsd"])
  AM_CONDITIONAL([DRUNTIME_OS_FREEBSD],
                 [test "$druntime_target_os_parsed" = "freebsd"])
  AM_CONDITIONAL([DRUNTIME_OS_NETBSD],
                 [test "$druntime_target_os_parsed" = "netbsd"])
  AM_CONDITIONAL([DRUNTIME_OS_OPENBSD],
                 [test "$druntime_target_os_parsed" = "openbsd"])
  AM_CONDITIONAL([DRUNTIME_OS_LINUX],
                 [test "$druntime_target_os_parsed" = "linux"])
  AM_CONDITIONAL([DRUNTIME_OS_MINGW],
                 [test "$druntime_target_os_parsed" = "mingw"])
  AM_CONDITIONAL([DRUNTIME_OS_SOLARIS],
                 [test "$druntime_target_os_parsed" = "solaris"])

  druntime_target_posix="no"
  case "$druntime_cv_target_os" in
    aix*|*bsd*|cygwin*|darwin*|dragonfly*|gnu*|linux*|skyos*|*solaris*|sysv*)
      druntime_target_posix="yes"
      ;;
  esac
  AM_CONDITIONAL([DRUNTIME_OS_POSIX], [test "$druntime_target_posix" = "yes"])
])


# DRUNTIME_OS_ARM_EABI_UNWINDER
# ------------------------
# Check if using ARM unwinder and substitute DCFG_ARM_EABI_UNWINDER
# and set DRUNTIME_OS_ARM_EABI_UNWINDER conditional.
AC_DEFUN([DRUNTIME_OS_ARM_EABI_UNWINDER],
[
  AC_LANG_PUSH([C])
  AC_MSG_CHECKING([for ARM unwinder])
  AC_TRY_COMPILE([#include <unwind.h>],[
  #if __ARM_EABI_UNWINDER__
  #error Yes, it is.
  #endif
  ],
    [AC_MSG_RESULT([no])
     DCFG_ARM_EABI_UNWINDER=false],
    [AC_MSG_RESULT([yes])
     DCFG_ARM_EABI_UNWINDER=true])
  AC_SUBST(DCFG_ARM_EABI_UNWINDER)
  AM_CONDITIONAL([DRUNTIME_OS_ARM_EABI_UNWINDER], [test "$DCFG_ARM_EABI_UNWINDER" = "true"])
  AC_LANG_POP([C])
])


# DRUNTIME_OS_MINFO_BRACKETING
# ----------------------------
# Check if the linker provides __start_minfo and __stop_minfo symbols and
# substitute DCFG_MINFO_BRACKETING.
AC_DEFUN([DRUNTIME_OS_MINFO_BRACKETING],
[
  AC_REQUIRE([DRUNTIME_OS_DETECT])

  AC_LANG_PUSH([C])
  AC_MSG_CHECKING([for minfo section bracketing])
  case "$druntime_cv_target_os" in
      darwin*)
	section="__DATA,__minfodata"
	start="section\$start\$__DATA\$__minfodata"
	stop="section\$end\$__DATA\$__minfodata"
	;;
      *)
	section="minfo"
	start="__start_minfo"
	stop="__stop_minfo"
	;;
  esac
  AC_LINK_IFELSE([AC_LANG_SOURCE([
    void* module_info_ptr __attribute__((section ("$section")));
    extern void* start_minfo __asm__("$start") __attribute__((visibility ("hidden")));
    extern void* stop_minfo __asm__("$stop") __attribute__((visibility ("hidden")));

    int main()
    {
        // Never run, just to prevent compiler from optimizing access
        return (int)(&stop_minfo - &start_minfo);
    }
  ])],
    [AC_MSG_RESULT([yes])
     DCFG_MINFO_BRACKETING=true],
    [AC_MSG_RESULT([no])
     DCFG_MINFO_BRACKETING=false])
  AC_SUBST(DCFG_MINFO_BRACKETING)
  AM_CONDITIONAL([DRUNTIME_OS_MINFO_BRACKETING], [test "$DCFG_MINFO_BRACKETING" = "true"])
  AC_LANG_POP([C])
])

# DRUNTIME_OS_DLPI_TLS_MODID
# ----------------------------
# Check if struct dl_phdr_info includes the dlpi_tls_modid member and  
# substitute DCFG_DLPI_TLS_MODID.
AC_DEFUN([DRUNTIME_OS_DLPI_TLS_MODID],
[
  AC_LANG_PUSH([C])
  AC_CHECK_MEMBER([struct dl_phdr_info.dlpi_tls_modid],
		  [DCFG_DLPI_TLS_MODID=true], [DCFG_DLPI_TLS_MODID=false],
		  [[#include <link.h>]])
  AC_SUBST(DCFG_DLPI_TLS_MODID)
  AC_LANG_POP([C])
])

# DRUNTIME_OS_LINK_SPEC
# ---------------------
# Add target-specific link options to link_spec.
AC_DEFUN([DRUNTIME_OS_LINK_SPEC],
[
  case $target in
    i?86-*-solaris2.* | x86_64-*-solaris2.*)
      # 64-bit Solaris/x86 ld breaks calls to __tls_get_addr with non-TLS
      # relocs.  Work around by disabling TLS transitions.  Not necessary
      # on 32-bit x86, but cannot be distinguished reliably in specs.
      druntime_ld_prog=`$CC -print-prog-name=ld`
      druntime_ld_gld=no
      druntime_ld_relax_transtls=no
      if test -n "$druntime_ld_prog" \
         && $druntime_ld_prog -v 2>&1 | grep GNU > /dev/null 2>&1; then
        druntime_ld_gld=yes
      else
        echo 'int main (void) { return 0; }' > conftest.c
        save_LDFLAGS="$LDFLAGS"
        LDFLAGS="$LDFLAGS -Wl,-z,relax=transtls"
        if $CC $CFLAGS $LDFLAGS -o conftest conftest.c > /dev/null 2>&1; then
          druntime_ld_relax_transtls=yes
        fi
        LDFLAGS="$save_LDFLAGS"
        rm -f conftest.c conftest
      fi
      if test "$druntime_ld_relax_transtls" = "yes"; then
        OS_LINK_SPEC='-z relax=transtls'
      fi
      ;;
  esac
  AC_SUBST(OS_LINK_SPEC)
])
