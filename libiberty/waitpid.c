#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

int
waitpid (pid, stat_loc, options)
	int pid, *stat_loc, options;
{
  for (;;)
    {
      int wpid = wait(stat_loc);
      if (wpid == pid || wpid == -1)
	return wpid;
    }
}
