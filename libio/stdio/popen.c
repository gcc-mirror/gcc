#include "libioP.h"
#include "stdio.h"
#include <errno.h>

FILE *
popen(command, mode)
     const char *command; const char *mode;
{
  return _IO_popen(command, mode);
}

int
pclose(fp)
     FILE *fp;
{
#if 0
  /* Does not actually test that stream was created by popen(). Instead,
     it depends on the filebuf::sys_close() virtual to Do The Right Thing. */
  if (fp is not a proc_file)
    return -1;
#endif
  return _IO_fclose(fp);
}
