#include "libioP.h"
#include "stdio.h"

int
fgetc(fp)
     FILE *fp;
{
  CHECK_FILE(fp, EOF);
  return _IO_getc(fp);
}
