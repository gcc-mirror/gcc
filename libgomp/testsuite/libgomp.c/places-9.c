/* { dg-set-target-env-var OMP_PLACES "ll_caches(1)" } */

#include <omp.h>

int
main ()
{
  omp_display_env (0);
  return 0;
}
