#include <bits/c++config.h>

#ifdef _GLIBCPP_HAVE_UNISTD_H
#include <unistd.h>
#define writestr(str)	write(2, str, sizeof(str) - 1)
#ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library).  */
#define write(fd, buf, n)	__write((fd), (buf), (n))
#endif
#else
#include <stdio.h>
#define writestr(str)	fputs(str, stderr)
#endif

extern "C" {

extern void __terminate(void) __attribute__ ((__noreturn__));

void
__pure_virtual (void)
{
  writestr ("pure virtual method called\n");
  __terminate ();
}

}
