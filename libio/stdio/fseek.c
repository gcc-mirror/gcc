#include "stdio.h"
#include "libioP.h"

int
fseek(fp, offset, whence)
     _IO_FILE* fp;
     long int offset;
     int whence;
{
  CHECK_FILE(fp, -1);
  return _IO_fseek(fp, offset, whence);
}
