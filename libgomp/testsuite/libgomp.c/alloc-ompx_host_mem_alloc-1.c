/* Verify that on the host we can but on a device we cannot allocate 'ompx_host_mem_alloc' memory.  */

/* { dg-additional-options -DOFFLOAD_DEVICE { target offload_device } } */

#include <omp.h>

#pragma omp requires dynamic_allocators

int main()
{
#pragma omp target
  {
    char *c, *c_;

    c = omp_alloc(1, ompx_host_mem_alloc);
#ifdef OFFLOAD_DEVICE
    if (c)
      __builtin_abort ();
#else
    if (!c)
      __builtin_abort ();
#endif
    omp_free(c, ompx_host_mem_alloc);

    c = omp_aligned_alloc(128, 256, ompx_host_mem_alloc);
#ifdef OFFLOAD_DEVICE
    if (c)
      __builtin_abort ();
#else
    if (!c)
      __builtin_abort ();
#endif
    omp_free(c, omp_null_allocator);

    c = omp_calloc(1, 1, ompx_host_mem_alloc);
#ifdef OFFLOAD_DEVICE
    if (c)
      __builtin_abort ();
#else
    if (!c)
      __builtin_abort ();
#endif
    c_ = omp_realloc(c, 2, ompx_host_mem_alloc, ompx_host_mem_alloc);
#ifdef OFFLOAD_DEVICE
    if (c_)
      __builtin_abort ();
#else
    if (!c_)
      __builtin_abort ();
#endif
    c = omp_realloc(c_, 0, ompx_host_mem_alloc, ompx_host_mem_alloc);
    if (c)
      __builtin_abort ();

    c = omp_aligned_calloc(64, 1, 512, ompx_host_mem_alloc);
#ifdef OFFLOAD_DEVICE
    if (c)
      __builtin_abort ();
#else
    if (!c)
      __builtin_abort ();
#endif
    c_ = omp_realloc(c, 2, c ? omp_null_allocator : ompx_host_mem_alloc, omp_null_allocator);
#ifdef OFFLOAD_DEVICE
    if (c_)
      __builtin_abort ();
#else
    if (!c_)
      __builtin_abort ();
#endif
    c = omp_realloc(c_, 0, omp_null_allocator, omp_null_allocator);
    if (c)
      __builtin_abort ();
  }

  return 0;
}
