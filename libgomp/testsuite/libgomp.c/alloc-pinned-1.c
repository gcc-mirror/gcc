/* { dg-do run } */

/* { dg-skip-if "Pinning not implemented on this host" { ! *-*-linux-gnu* } } */

/* Test that pinned memory works.  */

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
  /* Allocate at least a page each time, allowing space for overhead,
     but stay within the ulimit.  */
  const int SIZE = PAGE_SIZE - 128;
  CHECK_SIZE (SIZE * 5);  // This is intended to help diagnose failures

  const omp_alloctrait_t traits[] = {
      { omp_atk_pinned, 1 }
  };
  omp_allocator_handle_t allocator = omp_init_allocator (omp_default_mem_space,
							 1, traits);

#ifdef EXPECT_OMP_NULL_ALLOCATOR
  if (allocator == omp_null_allocator)
    return 0;
#endif

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, allocator);
  if (!p)
    abort ();

  int amount = get_pinned_mem ();
  if (amount == 0)
    abort ();

  p = omp_realloc (p, SIZE * 2, allocator, allocator);

  int amount2 = get_pinned_mem ();
  if (amount2 <= amount)
    abort ();

  /* SIZE*2 ensures that it doesn't slot into the space possibly
     vacated by realloc.  */
  p = omp_calloc (1, SIZE * 2, allocator);

  if (get_pinned_mem () <= amount2)
    abort ();

  verify0 (p, SIZE * 2);

  return 0;
}
