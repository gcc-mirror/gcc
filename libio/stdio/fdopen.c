#include "libioP.h"

_IO_FILE *
fdopen (fd, mode)
     int fd;
     const char *mode;
{
  return _IO_fdopen (fd, mode);
}
