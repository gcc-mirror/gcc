/* { dg-do run } */

/* Test that pinned memory fails correctly.  */

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
set_pin_limit (int size)
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
      { omp_atk_fallback, omp_atv_null_fb }
  };
  omp_allocator_handle_t allocator1 = omp_init_allocator (omp_default_mem_space,
							  2, traits1);

  /* Pinned memory, plain memory fallback.  */
  const omp_alloctrait_t traits2[] = {
      { omp_atk_pinned, 1 },
      { omp_atk_fallback, omp_atv_default_mem_fb }
  };
  omp_allocator_handle_t allocator2 = omp_init_allocator (omp_default_mem_space,
							  2, traits2);

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
  void *p1 = omp_alloc (SIZE, allocator1);
  if (p1)
    abort ();

  // Should fail
  void *p2 = omp_calloc (1, SIZE, allocator1);
  if (p2)
    abort ();

  // Should fall back
  void *p3 = omp_alloc (SIZE, allocator2);
  if (!p3)
    abort ();

  // Should fall back
  void *p4 = omp_calloc (1, SIZE, allocator2);
  if (!p4)
    abort ();
  verify0 (p4, SIZE);

  // Should fail to realloc
  void *notpinned = omp_alloc (SIZE, omp_default_mem_alloc);
  void *p5 = omp_realloc (notpinned, SIZE, allocator1, omp_default_mem_alloc);
  if (!notpinned || p5)
    abort ();

  // Should fall back to no realloc needed
  void *p6 = omp_realloc (notpinned, SIZE, allocator2, omp_default_mem_alloc);
  if (p6 != notpinned)
    abort ();

  // No memory should have been pinned
  int amount = get_pinned_mem ();
  if (amount != 0)
    abort ();

  // Ensure free works correctly
  if (p1) omp_free (p1, allocator1);
  if (p2) omp_free (p2, allocator1);
  if (p3) omp_free (p3, allocator2);
  if (p4) omp_free (p4, allocator2);
  // p5 and notpinned have been reallocated
  if (p6) omp_free (p6, omp_default_mem_alloc);

  return 0;
}
