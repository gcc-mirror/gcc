/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <omp.h>

int
main ()
{
  if (!omp_get_cancellation ())
    return 0;
  #pragma omp parallel num_threads (32)
  {
    #pragma omp sections
      {
	{
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  #pragma omp cancel sections
	  abort ();
	}
      }
  }
  return 0;
}
