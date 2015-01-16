/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  void *d;
  acc_device_t devtype = acc_device_host;

#if ACC_DEVICE_TYPE_nvidia
  devtype = acc_device_nvidia;

  if (acc_get_num_devices (acc_device_nvidia) == 0)
    return 0;
#endif

  acc_init (devtype);

  d = acc_malloc (0);
  if (d != NULL)
    abort ();

  acc_free (0);

  acc_shutdown (devtype);

  acc_set_device_type (devtype);

  d = acc_malloc (0);
  if (d != NULL)
    abort ();

  acc_shutdown (devtype);

  acc_init (devtype);

  d = acc_malloc (1024);
  if (d == NULL)
    abort ();

  acc_free (d);

  acc_shutdown (devtype);

  acc_set_device_type (devtype);

  d = acc_malloc (1024);
  if (d == NULL)
    abort ();

  acc_free (d);

  acc_shutdown (devtype);

  return 0;
}
