/* { dg-set-target-env-var OMP_PLACES "numa_domains" } */

#include <omp.h>

int
main ()
{
  omp_display_env (0);
  return 0;
}
