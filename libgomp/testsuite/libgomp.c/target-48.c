/* Check OMP_TARGET_OFFLOAD on systems with no available non-host devices;
   omp_invalid_device == -4 with GCC.  */

/* { dg-do run { target { ! offload_device } } } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '-4'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" } */

#include <omp.h>

int
main ()
{
  if (omp_get_default_device () != omp_invalid_device)
    __builtin_abort ();

  omp_set_default_device (omp_initial_device);

  /* The spec is a bit unclear whether the line above sets the device number
     (a) to -1 (= omp_initial_device) or
     (b) to omp_get_initial_device() == omp_get_num_devices(). Therefore,
     we accept either value.   */

  if (omp_get_default_device() != omp_get_initial_device()
      && omp_get_default_device() != omp_initial_device)
    __builtin_abort ();

  omp_display_env (0);

  return 0;
}
