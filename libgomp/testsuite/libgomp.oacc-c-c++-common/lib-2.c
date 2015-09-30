/* { dg-do run } */

#include <stdio.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  acc_device_t devtype = acc_device_host;

#if ACC_DEVICE_TYPE_nvidia
  devtype = acc_device_nvidia;

  if (acc_get_num_devices (acc_device_nvidia) == 0)
    return 0;
#endif

  acc_init (devtype);

  acc_shutdown (devtype);

  fprintf (stderr, "CheCKpOInT\n");
  acc_shutdown (devtype);

  return 0;
}

/* { dg-output "CheCKpOInT(\n|\r\n|\r).*" } */
/* { dg-output "no device initialized" } */
/* { dg-shouldfail "" } */
