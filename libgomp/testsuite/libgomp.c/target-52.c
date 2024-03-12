/* Only run this with available non-host devices; in that case, GCC sets
   the default-device-var to 0.  */

/* { dg-do run { target { offload_device } } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '0'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" } */

#include <omp.h>

int
main ()
{
  int x;
  #pragma omp target map(tofrom:x)
    x = 5 + omp_is_initial_device ();

  if (x != 5)
    __builtin_abort ();

  if (0 != omp_get_default_device())
    __builtin_abort ();
  return 0;
}
