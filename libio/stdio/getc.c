#include "libioP.h"
#include "stdio.h"

#undef getc

int
getc(stream)
  FILE *stream;
{
  return _IO_getc (stream);
}
