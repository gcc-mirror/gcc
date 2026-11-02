/* Check thread_limit on an AMD GCN device in "simt" offload mode, where each
   lane of a wavefront is a thread, so a limit counts lanes, up to 1024, and
   need not be a multiple of the wavefront size.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

static const int limits[] = { 1, 2, 63, 64, 65, 100, 128, 1000, 1024, 2000 };

int
main (void)
{
  for (int k = 0; k < (int) (sizeof (limits) / sizeof (limits[0])); k++)
    {
      int tl = limits[k];
      int expect = tl < 1024 ? tl : 1024;
      int n1 = 0, l1 = 0, n2 = 0, l2 = 0, n3 = 0, count = 0;

      /* On the target construct, without teams.  */
      #pragma omp target thread_limit(tl) map(tofrom: n1, l1, count)
      #pragma omp parallel
      {
	#pragma omp atomic
	count++;
	if (omp_get_thread_num () == 0)
	  {
	    n1 = omp_get_num_threads ();
	    l1 = omp_get_thread_limit ();
	  }
      }
      if (n1 != expect || l1 != expect || count != expect)
	abort ();

      /* On the teams construct, also with a larger num_threads.  */
      #pragma omp target teams num_teams(1) thread_limit(tl) \
		  map(tofrom: n2, l2, n3)
      {
	#pragma omp parallel
	if (omp_get_thread_num () == 0)
	  {
	    n2 = omp_get_num_threads ();
	    l2 = omp_get_thread_limit ();
	  }
	#pragma omp parallel num_threads(2048)
	if (omp_get_thread_num () == 0)
	  n3 = omp_get_num_threads ();
      }
      if (n2 != expect || l2 != expect || n3 != expect)
	abort ();
    }

  return 0;
}
