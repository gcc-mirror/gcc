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
