#include "libioP.h"
#include <errno.h>
#include <string.h>
#ifndef errno
extern int errno;
#endif

#ifndef _IO_strerror
extern char* _IO_strerror __P((int));
#endif

void
_IO_perror (s)
     const char *s;
{
  char *error = _IO_strerror (errno);

  if (s != NULL && *s != '\0')
    _IO_fprintf (_IO_stderr, "%s:", s);

  _IO_fprintf (_IO_stderr, "%s\n", error ? error : "");
}
