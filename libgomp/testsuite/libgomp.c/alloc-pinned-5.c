/* { dg-do run } */

/* { dg-skip-if "Pinning not implemented on this host" { ! *-*-linux-gnu* } } */

/* { dg-additional-options -DOFFLOAD_DEVICE_NVPTX { target offload_device_nvptx } } */

/* Test that ompx_gnu_pinned_mem_alloc works.  */

#include <stdio.h>
#include <stdlib.h>

#ifdef __linux__
#include <sys/types.h>
#include <unistd.h>

#include <sys/mman.h>
#include <sys/resource.h>

#define PAGE_SIZE sysconf(_SC_PAGESIZE)
#define CHECK_SIZE(SIZE) { \
  struct rlimit limit; \
  if (getrlimit (RLIMIT_MEMLOCK, &limit) \
      || limit.rlim_cur <= SIZE) \
    { \
      fprintf (stderr, "insufficient lockable memory; please increase ulimit\n"); \
      abort (); \
    } \
  }

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
#error "OS unsupported"
#endif

static void
verify0 (char *p, size_t s)
{
  for (size_t i = 0; i < s; ++i)
    if (p[i] != 0)
      abort ();
}

#include <omp.h>

int
main ()
{
#ifdef OFFLOAD_DEVICE_NVPTX
  /* Go big or go home.
     The OS ulimit does not affect memory locked via CUDA for NVPTX devices. */
  const int SIZE = 40 * 1024 * 1024;
#else
  /* Allocate at least a page each time, allowing space for overhead,
     but stay within the ulimit.  */
  const int SIZE = PAGE_SIZE - 128;
  CHECK_SIZE (SIZE * 5);
#endif

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, ompx_gnu_pinned_mem_alloc);
  if (!p)
    abort ();

  int amount = get_pinned_mem ();
#ifdef OFFLOAD_DEVICE_NVPTX
  /* This doesn't show up as process 'VmLck'ed memory.  */
  if (amount != 0)
    abort ();
#else
  if (amount == 0)
    abort ();
#endif

  p = omp_realloc (p, SIZE * 2, ompx_gnu_pinned_mem_alloc,
		   ompx_gnu_pinned_mem_alloc);

  int amount2 = get_pinned_mem ();
#ifdef OFFLOAD_DEVICE_NVPTX
  /* This doesn't show up as process 'VmLck'ed memory.  */
  if (amount2 != 0)
    abort ();
#else
  if (amount2 <= amount)
    abort ();
#endif

  /* SIZE*2 ensures that it doesn't slot into the space possibly
     vacated by realloc.  */
  p = omp_calloc (1, SIZE * 2, ompx_gnu_pinned_mem_alloc);

#ifdef OFFLOAD_DEVICE_NVPTX
  /* This doesn't show up as process 'VmLck'ed memory.  */
  if (get_pinned_mem () != 0)
    abort ();
#else
  if (get_pinned_mem () <= amount2)
    abort ();
#endif

  verify0 (p, SIZE * 2);

  return 0;
}
