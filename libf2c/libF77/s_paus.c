#include <stdio.h>
#include "f2c.h"
#define PAUSESIG 15

#include "signal1.h"
#undef abs
#undef min
#undef max
#include <stdlib.h>
extern int getpid (void), isatty (int), pause (void);

extern void f_exit (void);

static void
waitpause (Sigarg)
{
  Use_Sigarg;
  return;
}

static void
s_1paus (FILE * fin)
{
  fprintf (stderr,
	   "To resume execution, type go.  Other input will terminate the job.\n");
  fflush (stderr);
  if (getc (fin) != 'g' || getc (fin) != 'o' || getc (fin) != '\n')
    {
      fprintf (stderr, "STOP\n");
#ifdef NO_ONEXIT
      f_exit ();
#endif
      exit (0);
    }
}

int
s_paus (char *s, ftnlen n)
{
  fprintf (stderr, "PAUSE ");
  if (n > 0)
    fprintf (stderr, " %.*s", (int) n, s);
  fprintf (stderr, " statement executed\n");
  if (isatty (fileno (stdin)))
    s_1paus (stdin);
  else
    {
#if (defined (MSDOS) && !defined (GO32)) || defined (_WIN32)
      FILE *fin;
      fin = fopen ("con", "r");
      if (!fin)
	{
	  fprintf (stderr, "s_paus: can't open con!\n");
	  fflush (stderr);
	  exit (1);
	}
      s_1paus (fin);
      fclose (fin);
#else
      fprintf (stderr,
	       "To resume execution, execute a   kill -%d %d   command\n",
	       PAUSESIG, getpid ());
      signal1 (PAUSESIG, waitpause);
      fflush (stderr);
      pause ();
#endif
    }
  fprintf (stderr, "Execution resumes after PAUSE.\n");
  fflush (stderr);
  return 0;			/* NOT REACHED */
}
