#include "libioP.h"
#include "stdio.h"

int
ferror(fp)
     FILE* fp;
{
  CHECK_FILE(fp, EOF);
  return _IO_ferror(fp);
}
