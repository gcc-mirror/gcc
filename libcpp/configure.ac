#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT(cpplib, [ ], gcc-bugs@gcc.gnu.org, cpplib)
AC_CONFIG_SRCDIR(ucnid.h)
AC_CONFIG_MACRO_DIR(../config)
AC_CANONICAL_SYSTEM

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_CC
AC_PROG_CXX
AC_PROG_RANLIB
AC_CHECK_TOOL(AR, ar)

AC_USE_SYSTEM_EXTENSIONS
AC_SYS_LARGEFILE

MISSING=`cd $ac_aux_dir && ${PWDCMD-pwd}`/missing
AC_CHECK_PROGS([ACLOCAL], [aclocal], [$MISSING aclocal])
AC_CHECK_PROGS([AUTOCONF], [autoconf], [$MISSING autoconf])
AC_CHECK_PROGS([AUTOHEADER], [autoheader], [$MISSING autoheader])

if test -z "$ETAGS"; then
  ETAGS=etags
fi
AC_SUBST([ETAGS])

# Figure out what compiler warnings we can enable.
# See config/warnings.m4 for details.

ACX_PROG_CC_WARNING_OPTS([-W -Wall -Wno-narrowing -Wwrite-strings \
			  -Wmissing-format-attribute], [warn])
ACX_PROG_CC_WARNING_OPTS([-Wstrict-prototypes -Wmissing-prototypes \
			  -Wold-style-definition -Wc++-compat], [c_warn])
ACX_PROG_CC_WARNING_ALMOST_PEDANTIC([-Wno-long-long])

# Disable exceptions and RTTI if building with g++
ACX_PROG_CC_WARNING_OPTS(
       m4_quote(m4_do([-fno-exceptions -fno-rtti])), [noexception_flags])

# Only enable with --enable-werror-always until existing warnings are
# corrected.
ACX_PROG_CC_WARNINGS_ARE_ERRORS([manual])

# Dependency checking.
ZW_CREATE_DEPDIR
AC_LANG_PUSH([C++])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([int i;])], [],
		  [AC_MSG_ERROR([C++ compiler missing or inoperational])])
AC_LANG_POP([C++])
ZW_PROG_COMPILER_DEPENDENCIES([CXX])

# Checks for header files.
AC_HEADER_TIME
ACX_HEADER_STRING

AC_CHECK_HEADERS(locale.h fcntl.h limits.h stddef.h \
	stdlib.h strings.h string.h sys/file.h unistd.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_BIGENDIAN
AC_C_CONST
AC_C_INLINE
AC_FUNC_OBSTACK
AC_TYPE_OFF_T
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINTPTR_T
AC_CHECK_TYPE(ptrdiff_t, int)
AC_TYPE_UINT64_T
if test x"$ac_cv_c_uint64_t" = x"no"; then
  AC_MSG_ERROR([uint64_t not found])
fi
AC_STRUCT_TM
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
define(libcpp_UNLOCKED_FUNCS, clearerr_unlocked feof_unlocked dnl
  ferror_unlocked fflush_unlocked fgetc_unlocked fgets_unlocked dnl
  fileno_unlocked fprintf_unlocked fputc_unlocked fputs_unlocked dnl
  fread_unlocked fwrite_unlocked getchar_unlocked getc_unlocked dnl
  putchar_unlocked putc_unlocked)
AC_CHECK_FUNCS(libcpp_UNLOCKED_FUNCS)
AC_CHECK_DECLS([abort, asprintf, basename(char *), errno, getopt, vasprintf])
AC_CHECK_DECLS(m4_split(m4_normalize(libcpp_UNLOCKED_FUNCS)))

# Checks for library functions.
AC_FUNC_ALLOCA
AC_HEADER_STDC
AM_LANGINFO_CODESET
ZW_GNU_GETTEXT_SISTER_DIR

AC_CACHE_CHECK(for uchar, gcc_cv_type_uchar,
[AC_TRY_COMPILE([
#include <sys/types.h>
],
[if ((uchar *)0) return 0;
 if (sizeof(uchar)) return 0;],
ac_cv_type_uchar=yes, ac_cv_type_uchar=no)])
if test $ac_cv_type_uchar = yes; then
  AC_DEFINE(HAVE_UCHAR, 1,
  [Define if <sys/types.h> defines \`uchar'.])
fi

AC_CHECK_SIZEOF(ino_t)
AC_CHECK_SIZEOF(dev_t)

# g++ on Solaris 10+ defines _XOPEN_SOURCE=600, which exposes a different
# iconv() prototype.
AC_LANG_PUSH([C++])
AM_ICONV
AC_LANG_POP([C++])

# More defines and substitutions.
PACKAGE="$PACKAGE_TARNAME"
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Define to the name of this package.])
AC_SUBST(PACKAGE)

if test "x$enable_nls" != xno; then
  USED_CATALOGS='$(CATALOGS)'
else
  USED_CATALOGS=
fi
AC_SUBST(USED_CATALOGS)

AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable rules only needed by maintainers],,
enable_maintainer_mode=no)

if test "x$enable_maintainer_mode" = xno; then
  MAINT='#'
else
  MAINT=
fi
AC_SUBST(MAINT)

# Enable expensive internal checks
is_release=
if test -f $srcdir/../gcc/DEV-PHASE \
   && test x"`cat $srcdir/../gcc/DEV-PHASE`" != xexperimental; then
  is_release=yes
fi

AC_ARG_ENABLE(checking,
[AS_HELP_STRING([[--enable-checking[=LIST]]],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: yes,no,all,none,release.
		 Flags are: misc,valgrind or other strings])],
[ac_checking_flags="${enableval}"],[
# Determine the default checks.
if test x$is_release = x ; then
  ac_checking_flags=yes
else
  ac_checking_flags=release
fi])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes|all) ac_checking=1 ; ac_assert_checking=1 ; ac_valgrind_checking= ;;
	no|none) ac_checking= ; ac_assert_checking= ; ac_valgrind_checking= ;;
	release) ac_checking= ; ac_assert_checking=1 ; ac_valgrind_checking= ;;
	# these enable particular checks
	assert) ac_assert_checking=1 ;;
	misc) ac_checking=1 ;;
	valgrind) ac_valgrind_checking=1 ;;
	# accept
	*) ;;
	esac
done
IFS="$ac_save_IFS"
                
if test x$ac_checking != x ; then
  AC_DEFINE(CHECKING_P, 1,
[Define to 1 if you want more run-time sanity checks.])
else
  AC_DEFINE(CHECKING_P, 0)
fi

if test x$ac_assert_checking != x ; then
  AC_DEFINE(ENABLE_ASSERT_CHECKING, 1,
[Define if you want assertions enabled.  This is a cheap check.])
fi

AC_ARG_ENABLE(canonical-system-headers,
[  --enable-canonical-system-headers
                          enable or disable system headers canonicalization],
[],
enable_canonical_system_headers=yes)
if test $enable_canonical_system_headers != no; then
  AC_DEFINE(ENABLE_CANONICAL_SYSTEM_HEADERS,
            1, [Define to enable system headers canonicalization.])
fi

case $target in
  i?86-* | x86_64-*)
    AC_TRY_COMPILE([], [asm ("pcmpestri %0, %%xmm0, %%xmm1" : : "i"(0))],
      [AC_DEFINE([HAVE_SSE4], [1],
		 [Define to 1 if you can assemble SSE4 insns.])])
esac

# Enable --enable-host-shared.
AC_ARG_ENABLE(host-shared,
[AS_HELP_STRING([--enable-host-shared],
		[build host code as shared libraries])])
AC_SUBST(enable_host_shared)

# Enable --enable-host-pie.
AC_ARG_ENABLE(host-pie,
[AS_HELP_STRING([--enable-host-pie],
		[build host code as PIE])])
AC_SUBST(enable_host_pie)

if test x$enable_host_shared = xyes; then
  PICFLAG=-fPIC
elif test x$enable_host_pie = xyes; then
  PICFLAG=-fPIE
else
  PICFLAG=
fi

AC_SUBST(PICFLAG)

# Enable Intel CET on Intel CET enabled host if jit is enabled.
GCC_CET_HOST_FLAGS(CET_HOST_FLAGS)
case x$enable_languages in
*jit*)
  ;;
*)
  CET_HOST_FLAGS=
  ;;
esac
AC_SUBST(CET_HOST_FLAGS)

AC_ARG_ENABLE(valgrind-annotations,
[AS_HELP_STRING([--enable-valgrind-annotations],
		[enable valgrind runtime interaction])], [],
[enable_valgrind_annotations=no])
if test x$enable_valgrind_annotations != xno \
    || test x$ac_valgrind_checking != x; then
  AC_DEFINE(ENABLE_VALGRIND_WORKAROUNDS, 1,
[Define if you want to workaround Valgrind warnings about
 possible memory leaks because of libcpp use of interior pointers.])
fi

# Output.

AC_CONFIG_HEADERS(config.h:config.in, [echo timestamp > stamp-h1])
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
