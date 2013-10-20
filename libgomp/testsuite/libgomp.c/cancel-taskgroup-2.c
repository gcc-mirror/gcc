/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <unistd.h>
#include <omp.h>

int
main ()
{
  #pragma omp parallel
  #pragma omp taskgroup
  #pragma omp task
  {
    #pragma omp cancel taskgroup
    if (omp_get_cancellation ())
      abort ();
  }
  #pragma omp parallel
  {
    #pragma omp barrier
    #pragma omp single
    #pragma omp taskgroup
    {
      int i;
      for (i = 0; i < 50; i++)
	#pragma omp task
	{
	  #pragma omp cancellation point taskgroup
	  usleep (30);
	  #pragma omp cancel taskgroup if (i > 5)
	}
    }
    usleep (10);
  }
  return 0;
}
