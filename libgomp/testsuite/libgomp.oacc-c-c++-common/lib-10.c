/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  void *d;
  acc_device_t devtype = acc_device_default;

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
