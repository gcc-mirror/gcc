#include "libioP.h"
#include "stdio.h"
#undef putchar

int
putchar(c)
     int c;
{
  return _IO_putc(c, stdout);
}
