/* { dg-do run { target { ! openacc_nvidia_accel_configured } } } */

#include <stdio.h>
#include <openacc.h>

int
main (void)
{
  fprintf (stderr, "CheCKpOInT\n");
  acc_init (acc_device_nvidia);

  acc_shutdown (acc_device_nvidia);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "device type nvidia not supported" } */
/* { dg-shouldfail "" } */
