/* { dg-do run } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "disabled" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '\[0-9\]+'.*OMP_TARGET_OFFLOAD = 'DISABLED'.*" } */

#include <omp.h>

int
main ()
{
  int x;
  #pragma omp target map(tofrom:x)
    x = 5 + omp_is_initial_device ();

  if (x != 5+1)
    __builtin_abort ();

  if (omp_get_default_device() != omp_get_initial_device())
    __builtin_abort ();
  return 0;
}
