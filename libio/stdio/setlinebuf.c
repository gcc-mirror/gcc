#include "libioP.h"
#include "stdio.h"

#undef setlinebuf

void
setlinebuf (stream)
  FILE *stream;
{
  _IO_setvbuf(stream, NULL, 1, 0);
}
