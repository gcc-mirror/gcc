dnl See whether strncmp reads past the end of its string parameters.
dnl On some versions of SunOS4 at least, strncmp reads a word at a time
dnl but erroneously reads past the end of strings.  This can cause
dnl a SEGV in some cases.
AC_DEFUN(libiberty_AC_FUNC_STRNCMP,
[AC_REQUIRE([AC_FUNC_MMAP])
AC_CACHE_CHECK([for working strncmp], ac_cv_func_strncmp_works,
[AC_TRY_RUN([
/* Test by Jim Wilson and Kaveh Ghazi.
   Check whether strncmp reads past the end of its string parameters. */
#include <sys/types.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifndef MAP_ANON
#ifdef MAP_ANONYMOUS
#define MAP_ANON MAP_ANONYMOUS
#else
#define MAP_ANON MAP_FILE
#endif
#endif

#ifndef MAP_FILE
#define MAP_FILE 0
#endif
#ifndef O_RDONLY
#define O_RDONLY 0
#endif

#define MAP_LEN 0x10000

main ()
{
#if defined(HAVE_MMAP) || defined(HAVE_MMAP_ANYWHERE)
  char *p;
  int dev_zero;

  dev_zero = open ("/dev/zero", O_RDONLY);
  if (dev_zero < 0)
    exit (1);
  
  p = (char *) mmap (0, MAP_LEN, PROT_READ|PROT_WRITE,
		     MAP_ANON|MAP_PRIVATE, dev_zero, 0);
  if (p == (char *)-1)
    p = (char *) mmap (0, MAP_LEN, PROT_READ|PROT_WRITE,
		       MAP_ANON|MAP_PRIVATE, -1, 0);
  if (p == (char *)-1)
    exit (2);
  else
    {
      char *string = "__si_type_info";
      char *q = (char *) p + MAP_LEN - strlen (string) - 2;
      char *r = (char *) p + 0xe;

      strcpy (q, string);
      strcpy (r, string);
      strncmp (r, q, 14);
    }
#endif /* HAVE_MMAP || HAVE_MMAP_ANYWHERE */
  exit (0);
}
], ac_cv_func_strncmp_works=yes, ac_cv_func_strncmp_works=no,
  ac_cv_func_strncmp_works=no)
rm -f core core.* *.core])
if test $ac_cv_func_strncmp_works = no ; then
  LIBOBJS="$LIBOBJS strncmp.o"
fi
])

dnl See if errno must be declared even when <errno.h> is included.
AC_DEFUN(libiberty_AC_DECLARE_ERRNO,
[AC_CACHE_CHECK(whether errno must be declared, libiberty_cv_declare_errno,
[AC_TRY_COMPILE(
[#include <errno.h>],
[int x = errno;],
libiberty_cv_declare_errno=no,
libiberty_cv_declare_errno=yes)])
if test $libiberty_cv_declare_errno = yes
then AC_DEFINE(NEED_DECLARATION_ERRNO, 1,
  [Define if errno must be declared even when <errno.h> is included.])
fi
])

# FIXME: We temporarily define our own version of AC_PROG_CC.  This is
# copied from autoconf 2.12, but does not call AC_PROG_CC_WORKS.  We
# are probably using a cross compiler, which will not be able to fully
# link an executable.  This should really be fixed in autoconf
# itself.

AC_DEFUN(LIB_AC_PROG_CC,
[AC_BEFORE([$0], [AC_PROG_CPP])dnl
AC_PROVIDE([AC_PROG_CC])
AC_CHECK_PROG(CC, gcc, gcc)
if test -z "$CC"; then
  AC_CHECK_PROG(CC, cc, cc, , , /usr/ucb/cc)
  test -z "$CC" && AC_MSG_ERROR([no acceptable cc found in \$PATH])
fi

AC_PROG_CC_GNU

if test $ac_cv_prog_gcc = yes; then
  GCC=yes
  ac_libiberty_warn_cflags='-W -Wall -Wtraditional -pedantic'
dnl Check whether -g works, even if CFLAGS is set, in case the package
dnl plays around with CFLAGS (such as to build both debugging and
dnl normal versions of a library), tasteless as that idea is.
  ac_test_CFLAGS="${CFLAGS+set}"
  ac_save_CFLAGS="$CFLAGS"
  CFLAGS=
  AC_PROG_CC_G
  if test "$ac_test_CFLAGS" = set; then
    CFLAGS="$ac_save_CFLAGS"
  elif test $ac_cv_prog_cc_g = yes; then
    CFLAGS="-g -O2"
  else
    CFLAGS="-O2"
  fi
else
  GCC=
  ac_libiberty_warn_cflags=
  test "${CFLAGS+set}" = set || CFLAGS="-g"
fi
AC_SUBST(ac_libiberty_warn_cflags)
])

# Work around a bug in autoheader.  This can go away when we switch to
# autoconf >2.50.  The use of define instead of AC_DEFUN is
# deliberate.
define(AC_DEFINE_NOAUTOHEADER,
[cat >> confdefs.h <<\EOF
[#define] $1 ifelse($#, 2, [$2], $#, 3, [$2], 1)
EOF
])

# We always want a C version of alloca() compiled into libiberty,
# because native-compiler support for the real alloca is so !@#$%
# unreliable that GCC has decided to use it only when being compiled
# by GCC.  This is the part of AC_FUNC_ALLOCA that calculates the
# information alloca.c needs.
AC_DEFUN(libiberty_AC_FUNC_C_ALLOCA,
[AC_CACHE_CHECK(whether alloca needs Cray hooks, ac_cv_os_cray,
[AC_EGREP_CPP(webecray,
[#if defined(CRAY) && ! defined(CRAY2)
webecray
#else
wenotbecray
#endif
], ac_cv_os_cray=yes, ac_cv_os_cray=no)])
if test $ac_cv_os_cray = yes; then
  for ac_func in _getb67 GETB67 getb67; do
    AC_CHECK_FUNC($ac_func, 
      [AC_DEFINE_UNQUOTED(CRAY_STACKSEG_END, $ac_func, 
  [Define to one of _getb67, GETB67, getb67 for Cray-2 and Cray-YMP
   systems. This function is required for alloca.c support on those
   systems.])  break])
  done
fi

AC_CACHE_CHECK(stack direction for C alloca, ac_cv_c_stack_direction,
[AC_TRY_RUN([find_stack_direction ()
{
  static char *addr = 0;
  auto char dummy;
  if (addr == 0)
    {
      addr = &dummy;
      return find_stack_direction ();
    }
  else
    return (&dummy > addr) ? 1 : -1;
}
main ()
{
  exit (find_stack_direction() < 0);
}], 
  ac_cv_c_stack_direction=1,
  ac_cv_c_stack_direction=-1,
  ac_cv_c_stack_direction=0)])
AC_DEFINE_UNQUOTED(STACK_DIRECTION, $ac_cv_c_stack_direction,
  [Define if you know the direction of stack growth for your system;
   otherwise it will be automatically deduced at run-time.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown])
])
