#include "libioP.h"
#include "stdio.h"

#undef putc

int
putc(c, stream)
     int c;
     FILE *stream;
{
  return _IO_putc(c, stream);
}
