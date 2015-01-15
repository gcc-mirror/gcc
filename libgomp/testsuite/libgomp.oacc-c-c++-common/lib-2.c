/* { dg-do run } */

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

  acc_shutdown (devtype);

  return 0;
}

/* { dg-shouldfail "libgomp: no device initialized" } */
