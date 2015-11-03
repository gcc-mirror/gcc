/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  int i;
  int num_devices;
  int devnum;
  acc_device_t devtype = acc_device_default;

  num_devices = acc_get_num_devices (devtype);
  if (num_devices == 0)
    abort ();

  acc_init (devtype);

  if (num_devices != acc_get_num_devices (devtype))
    abort ();

  for (i = 0; i < num_devices; i++)
    {
      acc_set_device_num (i, devtype);
      devnum = acc_get_device_num (devtype);
      if (devnum != i)
	abort ();
    }

  acc_shutdown (devtype);

  if (num_devices != acc_get_num_devices (devtype))
    abort ();

  for (i = 0; i < num_devices; i++)
    {
      acc_set_device_num (i, devtype);
      devnum = acc_get_device_num (devtype);
      if (devnum != i)
	abort ();
    }

  acc_shutdown (devtype);

  acc_init (devtype);

  acc_set_device_num (0, devtype);

  devnum = acc_get_device_num (devtype);
  if (devnum != 0)
    abort ();

  if (num_devices > 1)
    {
      acc_set_device_num (1, (acc_device_t) 0);

      devnum = acc_get_device_num (devtype);
      if (devnum != 1)
	abort ();
  }

  acc_shutdown (devtype);

  return 0;
}

/* { dg-output "" } */
