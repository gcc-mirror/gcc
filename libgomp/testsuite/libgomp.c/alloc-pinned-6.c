/* { dg-do run } */

/* Test that ompx_gnu_pinned_mem_alloc fails correctly.  */

#include <stdio.h>
#include <stdlib.h>

#ifdef __linux__
#include <sys/types.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/resource.h>

#define PAGE_SIZE sysconf(_SC_PAGESIZE)

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

void
set_pin_limit (int size)
{
  struct rlimit limit;
  if (getrlimit (RLIMIT_MEMLOCK, &limit))
    abort ();
  limit.rlim_cur = (limit.rlim_max < size ? limit.rlim_max : size);
  if (setrlimit (RLIMIT_MEMLOCK, &limit))
    abort ();
}
#else
#define PAGE_SIZE 10000 * 1024 /* unknown */

int
get_pinned_mem ()
{
  return 0;
}

void
set_pin_limit (int size)
{
}
#endif

#include <omp.h>

int
main ()
{
  /* Allocate at least a page each time, but stay within the ulimit.  */
  const int SIZE = PAGE_SIZE * 4;

  /* Ensure that the limit is smaller than the allocation.  */
  set_pin_limit (SIZE / 2);

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  // Should fail
  void *p = omp_alloc (SIZE, ompx_gnu_pinned_mem_alloc);
  if (p)
    abort ();

  // Should fail
  p = omp_calloc (1, SIZE, ompx_gnu_pinned_mem_alloc);
  if (p)
    abort ();

  // Should fail to realloc
  void *notpinned = omp_alloc (SIZE, omp_default_mem_alloc);
  p = omp_realloc (notpinned, SIZE, ompx_gnu_pinned_mem_alloc,
		   omp_default_mem_alloc);
  if (!notpinned || p)
    abort ();

  // No memory should have been pinned
  int amount = get_pinned_mem ();
  if (amount != 0)
    abort ();

  return 0;
}
