#include "libioP.h"
#include "stdio.h"

int
fputc(c, fp)
     int c;
     FILE *fp;
{
  CHECK_FILE(fp, EOF);
  return _IO_putc(c, fp);
}
