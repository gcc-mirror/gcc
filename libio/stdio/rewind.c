#include "stdio.h"
#include "libioP.h"

void
rewind(fp)
     _IO_FILE* fp;
{
  CHECK_FILE(fp, );
  _IO_rewind(fp);
}
