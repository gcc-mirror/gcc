#include <stdio.h>
#include "f2c.h"

#undef abs
#undef min
#undef max
#include <stdlib.h>
void f_exit (void);

int
s_stop (char *s, ftnlen n)
{
  int i;

  if (n > 0)
    {
      fprintf (stderr, "STOP ");
      for (i = 0; i < n; ++i)
	putc (*s++, stderr);
      fprintf (stderr, " statement executed\n");
    }
#ifdef NO_ONEXIT
  f_exit ();
#endif
  exit (0);

/* We cannot avoid (useless) compiler diagnostics here:		*/
/* some compilers complain if there is no return statement,	*/
/* and others complain that this one cannot be reached.		*/

  return 0;			/* NOT REACHED */
}
