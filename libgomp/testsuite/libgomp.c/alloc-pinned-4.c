/* { dg-do run } */

/* Test that pinned memory fails correctly, pool_size code path.  */

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
#define EXPECT_OMP_NULL_ALLOCATOR

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
  /* This needs to be large enough to cover multiple pages.  */
  const int SIZE = PAGE_SIZE * 4;

  /* Pinned memory, no fallback.  */
  const omp_alloctrait_t traits1[] = {
      { omp_atk_pinned, 1 },
      { omp_atk_fallback, omp_atv_null_fb },
      { omp_atk_pool_size, SIZE * 8 }
  };
  omp_allocator_handle_t allocator1 = omp_init_allocator (omp_default_mem_space,
							  3, traits1);

  /* Pinned memory, plain memory fallback.  */
  const omp_alloctrait_t traits2[] = {
      { omp_atk_pinned, 1 },
      { omp_atk_fallback, omp_atv_default_mem_fb },
      { omp_atk_pool_size, SIZE * 8 }
  };
  omp_allocator_handle_t allocator2 = omp_init_allocator (omp_default_mem_space,
							  3, traits2);

#ifdef EXPECT_OMP_NULL_ALLOCATOR
  if (allocator1 == omp_null_allocator
      && allocator2 == omp_null_allocator)
    return 0;
#endif

  /* Ensure that the limit is smaller than the allocation.  */
  set_pin_limit (SIZE / 2);

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  // Should fail
  void *p = omp_alloc (SIZE, allocator1);
  if (p)
    abort ();

  // Should fail
  p = omp_calloc (1, SIZE, allocator1);
  if (p)
    abort ();

  // Should fall back
  p = omp_alloc (SIZE, allocator2);
  if (!p)
    abort ();

  // Should fall back
  p = omp_calloc (1, SIZE, allocator2);
  if (!p)
    abort ();
  verify0 (p, SIZE);

  // Should fail to realloc
  void *notpinned = omp_alloc (SIZE, omp_default_mem_alloc);
  p = omp_realloc (notpinned, SIZE, allocator1, omp_default_mem_alloc);
  if (!notpinned || p)
    abort ();

  // Should fall back to no realloc needed
  p = omp_realloc (notpinned, SIZE, allocator2, omp_default_mem_alloc);
  if (p != notpinned)
    abort ();

  // No memory should have been pinned
  int amount = get_pinned_mem ();
  if (amount != 0)
    abort ();

  return 0;
}
