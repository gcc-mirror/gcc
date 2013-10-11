/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <omp.h>

int
main ()
{
  #pragma omp parallel num_threads (32)
  {
    int i;
    #pragma omp for
    for (i = 0; i < 1000; ++i)
      {
	#pragma omp cancel for
	if (omp_get_cancellation ())
	  abort ();
      }
  }
  return 0;
}
