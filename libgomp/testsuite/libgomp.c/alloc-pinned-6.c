/* { dg-do run } */

/* { dg-additional-options -DOFFLOAD_DEVICE_NVPTX { target offload_device_nvptx } } */

/* Test that ompx_pinned_mem_alloc fails correctly.  */

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
#define PAGE_SIZE 10000*1024 /* unknown */

int
get_pinned_mem ()
{
  return 0;
}

void
set_pin_limit ()
{
}
#endif

#include <omp.h>

int
main ()
{
#ifdef OFFLOAD_DEVICE_NVPTX
  /* Go big or go home.  */
  const int SIZE = 40 * 1024 * 1024;
#else
  /* Allocate at least a page each time, but stay within the ulimit.  */
  const int SIZE = PAGE_SIZE*4;
#endif
  const int PIN_LIMIT = PAGE_SIZE*2;

  /* Ensure that the limit is smaller than the allocation.  */
  set_pin_limit (PIN_LIMIT);

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, ompx_pinned_mem_alloc);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'.
  if (!p)
    abort ();
#else
  // Should fail
  if (p)
    abort ();
#endif

  p = omp_calloc (1, SIZE, ompx_pinned_mem_alloc);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'.
  if (!p)
    abort ();
#else
  // Should fail
  if (p)
    abort ();
#endif

  void *notpinned = omp_alloc (SIZE, omp_default_mem_alloc);
  p = omp_realloc (notpinned, SIZE, ompx_pinned_mem_alloc, omp_default_mem_alloc);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'; does reallocate.
  if (!notpinned || !p || p == notpinned)
    abort ();
#else
  // Should fail to realloc
  if (!notpinned || p)
    abort ();
#endif

  // No memory should have been pinned
  int amount = get_pinned_mem ();
  if (amount != 0)
    abort ();

  return 0;
}
