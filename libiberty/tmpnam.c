#include <stdio.h>

#ifndef L_tmpnam
#define L_tmpname 100
#endif
#ifndef P_tmpdir
#define P_tmpdir "/usr/tmp"
#endif

static char tmpnam_buffer[L_tmpnam];
static int tmpnam_counter;

extern int getpid ();

char *
tmpnam (s)
     char *s;
{
  int pid = getpid ();

  if (s == NULL)
    s = tmpnam_buffer;

  /*  Generate the filename and make sure that there isn't one called
      it already.  */

  while (1)
    {
      FILE *f;
      sprintf (s, "%s/%s%x.%x", P_tmpdir, "t", pid, tmpnam_counter);
      f = fopen (s, "r");
      if (f == NULL)
	break;
      tmpnam_counter++;
      fclose (f);
    }

  return s;
}
