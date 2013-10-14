// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <omp.h>
#include "cancel-test.h"

int
main ()
{
  if (!omp_get_cancellation ())
    return 0;
  #pragma omp parallel num_threads (32)
  {
    S a;
    #pragma omp sections
      {
	{
	  S b;
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  S c;
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  S d;
	  #pragma omp cancel sections
	  abort ();
	}
      #pragma omp section
	{
	  S e;
	  #pragma omp cancel sections
	  abort ();
	}
      }
  }
  S::verify ();
}
