/* Some known programs (xterm, pdksh?) non-portably change the _file
   field of s struct _iobuf.  This kludge allows the same "functionality".
   This code is an undocumented feature for iostream/stdio. Use it at
   your own risk. */

#include "libioP.h"
#include "stdio.h"

void
setfileno(fp, fd)
     _IO_FILE* fp;
     int fd;
{
  CHECK_FILE(fp, );
  if ((fp->_flags & _IO_IS_FILEBUF) != 0)
    fp->_fileno = fd;
}
