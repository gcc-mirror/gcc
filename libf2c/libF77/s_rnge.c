#include <stdio.h>
#include "f2c.h"

/* called when a subscript is out of range */

extern void sig_die (char *, int);
integer
s_rnge (char *varn, ftnint offset, char *procn, ftnint line)
{
  register int i;

  fprintf (stderr, "Subscript out of range on file line %ld, procedure ",
	   (long) line);
  while ((i = *procn) && i != '_' && i != ' ')
    putc (*procn++, stderr);
  fprintf (stderr, ".\nAttempt to access the %ld-th element of variable ",
	   (long) offset + 1);
  while ((i = *varn) && i != ' ')
    putc (*varn++, stderr);
  sig_die (".", 1);
  return 0;			/* not reached */
}
