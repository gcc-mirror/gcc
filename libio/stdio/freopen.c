#include "libioP.h"
#include "stdio.h"

FILE*
freopen(filename, mode, fp)
     const char* filename;
     const char* mode;
     FILE* fp;
{
  CHECK_FILE(fp, NULL);
  if (!(fp->_flags & _IO_IS_FILEBUF))
    return NULL;
  return _IO_freopen(filename, mode, fp);
}
