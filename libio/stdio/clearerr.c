#include "libioP.h"
#include "stdio.h"

void
clearerr(fp)
     FILE* fp;
{
  CHECK_FILE(fp, /*nothing*/);
  _IO_clearerr(fp);
}
