/* Check that the wavefronts of an AMD GCN device in "simt" offload mode that
   are not part of the current team survive many wake-ups with no work.  A
   GCN barrier stops every wavefront of the work-group, so the barriers of a
   team smaller than the launch also release the idle wavefronts, which used
   to give up with "team master not responding" after 100 of them.  */

/* { dg-do run } */
/* { dg-require-effective-target offload_device_gcn } */
/* { dg-additional-options "--param=offload-gpu-mode=simt" { target offload_target_amdgcn } } */

#include <stdlib.h>
#include <omp.h>

#define N 300

int
main (void)
{
  int count = 0, nthr = 0;

  /* A team of two wavefronts; the others stay idle.  */
  #pragma omp target teams num_teams(1) map(tofrom: count, nthr)
  {
    #pragma omp parallel num_threads(100)
    {
      for (int i = 0; i < N; i++)
	{
	  #pragma omp barrier
	}
      if (omp_get_thread_num () == 0)
	nthr = omp_get_num_threads ();
    }
    for (int i = 0; i < N; i++)
      {
	#pragma omp parallel num_threads(100)
	if (omp_get_thread_num () == 0)
	  count++;
      }
  }
  if (count != N || nthr != 100)
    abort ();

  return 0;
}
