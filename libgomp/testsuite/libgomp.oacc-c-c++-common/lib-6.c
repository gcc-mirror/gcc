/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  int devnum;

  if (acc_get_device_type () == acc_device_default)
    abort ();

  if (acc_get_num_devices (acc_device_nvidia))
    {
      acc_set_device_type (acc_device_nvidia);

      if (acc_get_device_type () != acc_device_nvidia)
	abort ();

      acc_shutdown (acc_device_nvidia);

      acc_set_device_type (acc_device_nvidia);

      if (acc_get_device_type () != acc_device_nvidia)
	abort ();

      devnum = acc_get_num_devices (acc_device_host);
      if (devnum != 1)
	abort ();

      acc_shutdown (acc_device_nvidia);
    }

  if (acc_get_num_devices (acc_device_radeon))
    {
      acc_set_device_type (acc_device_radeon);

      if (acc_get_device_type () != acc_device_radeon)
	abort ();

      acc_shutdown (acc_device_radeon);

      acc_set_device_type (acc_device_radeon);

      if (acc_get_device_type () != acc_device_radeon)
	abort ();

      devnum = acc_get_num_devices (acc_device_host);
      if (devnum != 1)
	abort ();

      acc_shutdown (acc_device_radeon);
    }

  if (acc_get_device_type () == acc_device_default)
    abort ();

  return 0;
}
