#include "config.h"
#include "f2c.h"
#include "fio.h"

#undef abs
#undef min
#undef max
#include <stdlib.h>
#ifdef NON_UNIX_STDIO
#ifndef unlink
#define unlink remove
#endif
#else
#if defined (MSDOS) && !defined (GO32)
#include "io.h"
#else
extern int unlink (const char *);
#endif
#endif

integer
f_clos (cllist * a)
{
  unit *b;

  if (f__init & 2)
    f__fatal (131, "I/O recursion");
  if (a->cunit >= MXUNIT)
    return (0);
  b = &f__units[a->cunit];
  if (b->ufd == NULL)
    goto done;
  if (b->uscrtch == 1)
    goto Delete;
  if (!a->csta)
    goto Keep;
  switch (*a->csta)
    {
    default:
    Keep:
    case 'k':
    case 'K':
      if (b->uwrt == 1)
	t_runc ((alist *) a);
      if (b->ufnm)
	{
	  fclose (b->ufd);
	  free (b->ufnm);
	}
      break;
    case 'd':
    case 'D':
    Delete:
      fclose (b->ufd);
      if (b->ufnm)
	{
	  unlink (b->ufnm);
	  /*SYSDEP*/ free (b->ufnm);
	}
    }
  b->ufd = NULL;
done:
  b->uend = 0;
  b->ufnm = NULL;
  return (0);
}

void
f_exit (void)
{
  int i;
  static cllist xx;
  if (!(f__init & 1))
    return;			/* Not initialized, so no open units. */
  /* I/O no longer in progress.  If, during an I/O operation (such
     as waiting for the user to enter a line), there is an
     interrupt (such as ^C to stop the program on a UNIX system),
     f_exit() is called, but there is no longer any I/O in
     progress.  Without turning off this flag, f_clos() would
     think that there is an I/O recursion in this circumstance. */
  f__init &= ~2;
  if (!xx.cerr)
    {
      xx.cerr = 1;
      xx.csta = NULL;
      for (i = 0; i < MXUNIT; i++)
	{
	  xx.cunit = i;
	  (void) f_clos (&xx);
	}
    }
}
int
G77_flush_0 (void)
{
  int i;
  for (i = 0; i < MXUNIT; i++)
    if (f__units[i].ufd != NULL && f__units[i].uwrt)
      fflush (f__units[i].ufd);
  return 0;
}
