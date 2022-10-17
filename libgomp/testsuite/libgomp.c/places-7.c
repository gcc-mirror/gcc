/* { dg-set-target-env-var OMP_PLACES "cores(1)" } */

#include <omp.h>

int
main ()
{
  omp_display_env (0);
  return 0;
}
