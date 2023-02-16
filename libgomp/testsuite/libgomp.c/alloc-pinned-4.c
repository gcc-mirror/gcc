/* { dg-do run } */

/* { dg-additional-options -DOFFLOAD_DEVICE_NVPTX { target offload_device_nvptx } } */

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
int
#define PAGE_SIZE 10000*1024 /* unknown */

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
#ifdef OFFLOAD_DEVICE_NVPTX
  /* Go big or go home.  */
  const int SIZE = 40 * 1024 * 1024;
#else
  /* This needs to be large enough to cover multiple pages.  */
  const int SIZE = PAGE_SIZE*4;
#endif
  const int PIN_LIMIT = PAGE_SIZE*2;

  /* Pinned memory, no fallback.  */
  const omp_alloctrait_t traits1[] = {
      { omp_atk_pinned, 1 },
      { omp_atk_fallback, omp_atv_null_fb },
      { omp_atk_pool_size, SIZE*8 }
  };
  omp_allocator_handle_t allocator1 = omp_init_allocator (omp_default_mem_space, 3, traits1);

  /* Pinned memory, plain memory fallback.  */
  const omp_alloctrait_t traits2[] = {
      { omp_atk_pinned, 1 },
      { omp_atk_fallback, omp_atv_default_mem_fb },
      { omp_atk_pool_size, SIZE*8 }
  };
  omp_allocator_handle_t allocator2 = omp_init_allocator (omp_default_mem_space, 3, traits2);

  /* Ensure that the limit is smaller than the allocation.  */
  set_pin_limit (PIN_LIMIT);

  // Sanity check
  if (get_pinned_mem () != 0)
    abort ();

  void *p = omp_alloc (SIZE, allocator1);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'.
  if (!p)
    abort ();
#else
  // Should fail
  if (p)
    abort ();
#endif

  p = omp_calloc (1, SIZE, allocator1);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'.
  if (!p)
    abort ();
#else
  // Should fail
  if (p)
    abort ();
#endif

  // Should fall back
  p = omp_alloc (SIZE, allocator2);
  if (!p)
    abort ();

  // Should fall back
  p = omp_calloc (1, SIZE, allocator2);
  if (!p)
    abort ();
  verify0 (p, SIZE);

  void *notpinned = omp_alloc (SIZE, omp_default_mem_alloc);
  p = omp_realloc (notpinned, SIZE, allocator1, omp_default_mem_alloc);
#ifdef OFFLOAD_DEVICE_NVPTX
  // Doesn't care about 'set_pin_limit'; does reallocate.
  if (!notpinned || !p || p == notpinned)
    abort ();
#else
  // Should fail to realloc
  if (!notpinned || p)
    abort ();
#endif

#ifdef OFFLOAD_DEVICE_NVPTX
  void *p_ = omp_realloc (p, SIZE, allocator2, allocator1);
  // Does reallocate.
  if (p_ == p)
    abort ();
#else
  p = omp_realloc (notpinned, SIZE, allocator2, omp_default_mem_alloc);
  // Should fall back to no realloc needed
  if (p != notpinned)
    abort ();
#endif

  // No memory should have been pinned
  int amount = get_pinned_mem ();
  if (amount != 0)
    abort ();

  return 0;
}
