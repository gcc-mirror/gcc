// { dg-do run }
// { dg-set-target-env-var OMP_CANCELLATION "true" }

#include <omp.h>
#include "cancel-test.h"

int
main ()
{
  #pragma omp parallel num_threads (32)
  {
    S a;
    #pragma omp cancel parallel
    if (omp_get_cancellation ())
      abort ();
  }
  S::verify ();
}
