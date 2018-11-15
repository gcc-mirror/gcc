/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <omp.h>

int
main ()
{
  int a[64];
  #pragma omp parallel
  {
    #pragma omp barrier
    if (omp_get_thread_num () == 0)
      {
	#pragma omp cancel parallel
      }
    #pragma omp for
    for (int i = 0; i < 64; i++)
      a[i] = i;
    if (omp_get_cancellation ())
      abort ();
  }
  #pragma omp parallel
  {
    #pragma omp barrier
    if (omp_get_thread_num () == 0)
      {
	#pragma omp cancel parallel
      }
    #pragma omp taskgroup
    {
      #pragma omp for
      for (int i = 0; i < 64; i++)
	#pragma omp task
	a[i] += i;
      if (omp_get_cancellation ())
	abort ();
    }
  }
  return 0;
}
