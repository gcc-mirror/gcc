/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <unistd.h>
#include <omp.h>

int
main ()
{
  int i;
  #pragma omp parallel
  {
    int c = 0;
    #pragma omp barrier
    #pragma omp master taskloop num_tasks (25) firstprivate (c)
    for (i = 0; i < 50; i++)
      {
        if (c && omp_get_cancellation ())
	  abort ();
	#pragma omp cancellation point taskgroup
	usleep (30);
	if (i > 10)
	  c = 1;
	#pragma omp cancel taskgroup if (i > 10)
	if (i > 10 && omp_get_cancellation ())
	  abort ();
      }
    usleep (10);
  }
  return 0;
}
