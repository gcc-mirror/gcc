#include "libioP.h"
#include "stdio.h"

void
setbuf (fp, buf)
     FILE *fp;  char *buf;
{
  _IO_setbuffer(fp, buf, _IO_BUFSIZ);
}
