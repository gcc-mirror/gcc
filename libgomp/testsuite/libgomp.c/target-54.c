/* { dg-do run } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "default" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '0'.*OMP_TARGET_OFFLOAD = 'DEFAULT'.*" } */

#include <omp.h>

int
main ()
{
  int x;
  #pragma omp target map(tofrom:x)
    x = 5 + omp_is_initial_device ();

  if (x != 5 + (omp_get_default_device() == omp_get_initial_device()))
    __builtin_abort ();

  return 0;
}
