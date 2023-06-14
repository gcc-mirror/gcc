/* Check OMP_TARGET_OFFLOAD on systems with non-host devices but no executable
   code due to -foffload=disable.

   Note: While one might expect that -foffload=disable implies no non-host
   devices, libgomp actually detects the devices and only fails when trying to
   run as no executable code is availale for that device.
   (Without MANDATORY it simply uses host fallback, which should usually be fine
   but might have issues in corner cases.)

   We have default-device-var = 0 (default but also explicitly set), which will
   fail at runtime. For -foffload=disable without non-host devices, see
   target-50.c testcase.  */

/* { dg-do run { target offload_device } } */

/* { dg-additional-options "-foffload=disable" } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */
/* { dg-set-target-env-var OMP_DEFAULT_DEVICE "0" } */
/* { dg-set-target-env-var OMP_DISPLAY_ENV "true" } */

/* { dg-output ".*OMP_DEFAULT_DEVICE = '0'.*OMP_TARGET_OFFLOAD = 'MANDATORY'.*" } */

#include <omp.h>

int
main ()
{
  int x;
  /* We know that there are non-host devices. With GCC, we still find them as
     available devices, hence, check for it.  */
  if (omp_get_num_devices() <= 0)
    __builtin_abort ();

  /* But due to -foffload=disable, there are no binary code for (default) device '0'  */

  /* { dg-output ".*libgomp: OMP_TARGET_OFFLOAD is set to MANDATORY, but device cannot be used for offloading.*" } */
  /* { dg-shouldfail "OMP_TARGET_OFFLOAD=mandatory and no binary code for a non-host device" } */
  #pragma omp target map(tofrom:x)
    x = 5;
  if (x != 5)
    __builtin_abort ();
  return 0;
}
