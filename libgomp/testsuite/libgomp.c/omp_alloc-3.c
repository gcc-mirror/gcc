/* { dg-do run } */

/* Stress-test omp_alloc/omp_malloc under concurrency.  */

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#pragma omp requires dynamic_allocators

#define N 1000

void
test (omp_allocator_handle_t allocator)
{
  #pragma omp target map(to:allocator)
  {
    #pragma omp parallel for
    for (int i = 0; i < N; i++)
      for (int j = 0; j < N; j++)
	{
	  int *p = omp_alloc (sizeof (int), allocator);
	  omp_free (p, allocator);
	}
  }
}

int
main ()
{
  /* omp_low_lat_mem_alloc doesn't actually get low-latency memory on GPU.  */
  omp_alloctrait_t traits[1] = { { omp_atk_access, omp_atv_cgroup } };
  omp_allocator_handle_t gpu_lowlat;
  #pragma omp target map(from:gpu_lowlat)
  gpu_lowlat = omp_init_allocator (omp_low_lat_mem_space, 1, traits);

  // Smaller than low-latency memory limit
  test (omp_default_mem_alloc);
  test (omp_large_cap_mem_alloc);
  test (omp_const_mem_alloc);
  test (omp_high_bw_mem_alloc);
  test (omp_low_lat_mem_alloc);
  test (gpu_lowlat);
  test (omp_cgroup_mem_alloc);
  test (omp_pteam_mem_alloc);
  test (omp_thread_mem_alloc);

  return 0;
}
