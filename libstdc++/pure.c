#include <stdio.h>

#ifdef __GNU_LIBRARY__
  /* Avoid forcing the library's meaning of `write' on the user program
     by using the "internal" name (for use within the library)  */
#define write(fd, buf, n)	__write((fd), (buf), (n))
#endif

#define MESSAGE "pure virtual method called\n"

void
__pure_virtual (void)
{
  write (2, MESSAGE, sizeof (MESSAGE) - 1);
  __terminate ();
}
