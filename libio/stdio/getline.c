#include "libioP.h"
#include "stdio.h"

/* NOTE:  This geline function is different from _IO_getline.  */

_IO_ssize_t
getline (lineptr, linelen, fp)
     char** lineptr;
     size_t* linelen;
     FILE* fp;
{
  return _IO_getdelim (lineptr, linelen, '\n', fp);
}
