/* rename -- rename a file
   This function is in the public domain. */

/* Rename a file.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

int
rename (zfrom, zto)
     char *zfrom;
     char *zto;
{
  if (link (zfrom, zto) < 0)
    {
      if (errno != EEXIST)
	return -1;
      if (unlink (zto) < 0
	  || link (zfrom, zto) < 0)
	return -1;
    }
  return unlink (zfrom);
}
