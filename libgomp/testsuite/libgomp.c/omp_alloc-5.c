/* { dg-do run } */

/* Test calloc with omp_alloc.  */

#include <omp.h>

#pragma omp requires dynamic_allocators

void
test (int n, omp_allocator_handle_t allocator)
{
  #pragma omp target map(to:n) map(to:allocator)
  {
    int *a;
    a = (int *) omp_calloc (n, sizeof (int), allocator);

    for (int i = 0; i < n; i++)
      if (a[i] != 0)
	{
	  __builtin_printf ("memory not zeroed at %i\n", i);
	  __builtin_abort ();
	}

    #pragma omp parallel
    for (int i = 0; i < n; i++)
      a[i] = i;

    for (int i = 0; i < n; i++)
      if (a[i] != i)
	{
	  __builtin_printf ("data mismatch at %i\n", i);
	  __builtin_abort ();
	}

    omp_free (a, allocator);
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
  test (10, omp_default_mem_alloc);
  test (10, omp_large_cap_mem_alloc);
  test (10, omp_const_mem_alloc);
  test (10, omp_high_bw_mem_alloc);
  test (10, omp_low_lat_mem_alloc);
  test (10, gpu_lowlat);
  test (10, omp_cgroup_mem_alloc);
  test (10, omp_pteam_mem_alloc);
  test (10, omp_thread_mem_alloc);

  // Larger than low-latency memory limit
  test (100000, omp_default_mem_alloc);
  test (100000, omp_large_cap_mem_alloc);
  test (100000, omp_const_mem_alloc);
  test (100000, omp_high_bw_mem_alloc);
  test (100000, omp_low_lat_mem_alloc);
  test (100000, gpu_lowlat);
  test (100000, omp_cgroup_mem_alloc);
  test (100000, omp_pteam_mem_alloc);
  test (100000, omp_thread_mem_alloc);

  return 0;
}
