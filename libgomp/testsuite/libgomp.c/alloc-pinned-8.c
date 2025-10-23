/* { dg-do run } */

/* { dg-skip-if "Pinning not implemented on this host" { ! *-*-linux-gnu* } } */

/* { dg-additional-options -DOFFLOAD_DEVICE_NVPTX { target offload_device_nvptx } } */

/* Test that pinned memory works for small allocations.  */

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
    fprintf (stderr, "insufficient lockable memory; please increase ulimit\n"); \
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
  /* Choose a small size where all our allocations fit on one page.  */
  const int SIZE = 10;
#ifndef OFFLOAD_DEVICE_NVPTX
  CHECK_SIZE (SIZE*4);
#endif

  const omp_alloctrait_t traits[] = {
      { omp_atk_pinned, 1 }
  };
  omp_allocator_handle_t allocator = omp_init_allocator (omp_default_mem_space, 1, traits);

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, allocator);
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

  p = omp_realloc (p, SIZE * 2, allocator, allocator);

  int amount2 = get_pinned_mem ();
#ifdef OFFLOAD_DEVICE_NVPTX
  /* This doesn't show up as process 'VmLck'ed memory.  */
  if (amount2 != 0)
    abort ();
#else
  /* A small allocation should not allocate another page.  */
  if (amount2 != amount)
    abort ();
#endif

  p = omp_calloc (1, SIZE, allocator);

#ifdef OFFLOAD_DEVICE_NVPTX
  /* This doesn't show up as process 'VmLck'ed memory.  */
  if (get_pinned_mem () != 0)
    abort ();
#else
  /* A small allocation should not allocate another page.  */
  if (get_pinned_mem () != amount2)
    abort ();
#endif

  verify0 (p, SIZE);

  return 0;
}
