/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  if (acc_get_device_type () == acc_device_default)
    abort ();

  acc_init (acc_device_default);

  if (acc_get_device_type () == acc_device_default)
    abort ();

  acc_shutdown (acc_device_default);

  if (acc_get_num_devices (acc_device_nvidia) != 0)
    {
      acc_init (acc_device_nvidia);

      if (acc_get_device_type () != acc_device_nvidia)
        abort ();

      acc_shutdown (acc_device_nvidia);

      acc_init (acc_device_default);

      acc_set_device_type (acc_device_nvidia);

      if (acc_get_device_type () != acc_device_nvidia)
        abort ();

      acc_shutdown (acc_device_nvidia);
    }

  return 0;

}
