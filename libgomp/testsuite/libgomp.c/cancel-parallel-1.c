/* { dg-do run } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */

#include <stdlib.h>
#include <omp.h>

int
main ()
{
  #pragma omp parallel num_threads (32)
  {
    #pragma omp cancel parallel
    if (omp_get_cancellation ())
      abort ();
  }
  return 0;
}
