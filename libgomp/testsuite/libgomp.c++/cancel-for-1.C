// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <omp.h>
#include "cancel-test.h"

int
main ()
{
  {
    S c;
    #pragma omp parallel num_threads (32)
    {
      S a, b;
      int i;
      #pragma omp for private (b) firstprivate (c)
      for (i = 0; i < 1000; ++i)
	{
	  S d;
	  #pragma omp cancel for
	  if (omp_get_cancellation ())
	    abort ();
	  b.bump ();
	  c.bump ();
	}
    }
  }
  S::verify ();
}
