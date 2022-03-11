/* { dg-do run } */

/* { dg-xfail-run-if "Pinning not implemented on this host" { ! *-*-linux-gnu } } */

/* Test that ompx_pinned_mem_alloc works.  */

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
#endif

#include <omp.h>

/* Allocate more than a page each time, but stay within the ulimit.  */
#define SIZE 10*1024

int
main ()
{
  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, ompx_pinned_mem_alloc);
  if (!p)
    abort ();

  int amount = get_pinned_mem ();
  if (amount == 0)
    abort ();

  p = omp_realloc (p, SIZE*2, ompx_pinned_mem_alloc, ompx_pinned_mem_alloc);

  int amount2 = get_pinned_mem ();
  if (amount2 <= amount)
    abort ();

  p = omp_calloc (1, SIZE, ompx_pinned_mem_alloc);

  if (get_pinned_mem () <= amount2)
    abort ();

  return 0;
}
