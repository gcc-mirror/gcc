#include "libioP.h"
#include "stdio.h"

#undef getchar

int
getchar ()
{
  return _IO_getc (stdin);
}
