/* { dg-do run } */
/* { dg-additional-options "-foffload-memory=pinned" } */

/* { dg-xfail-run-if "Pinning not implemented on this host" { ! *-*-linux-gnu } } */

/* Test that pinned memory works.  */

#ifdef __linux__
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/mman.h>

int
get_pinned_mem ()
{
  int pid = getpid ();
  char buf[100];
  sprintf (buf, "/proc/%d/status", pid);

  FILE *proc = fopen (buf, "r");
  if (!proc)
    abort ();
  while (fgets (buf, 100, proc))
    {
      int val;
      if (sscanf (buf, "VmLck: %d", &val))
	{
	  printf ("lock %d\n", val);
	  fclose (proc);
	  return val;
	}
    }
  abort ();
}
#else
int
get_pinned_mem ()
{
  return 0;
}

#define mlockall(...) 0
#endif

#include <omp.h>

/* Allocate more than a page each time, but stay within the ulimit.  */
#define SIZE 10*1024

int
main ()
{
  // Sanity check
  if (get_pinned_mem () == 0)
    {
      /* -foffload-memory=pinned has failed, but maybe that's because
	 isufficient pinned memory was available.  */
      if (mlockall (MCL_CURRENT | MCL_FUTURE) == 0)
	abort ();
    }

  return 0;
}
