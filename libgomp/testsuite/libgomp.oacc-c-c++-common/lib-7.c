/* { dg-do run } */

#include <stdlib.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  if (acc_get_num_devices (acc_device_none) != 0)
    abort ();

  if (acc_get_num_devices (acc_device_host) == 0)
    abort ();

  return 0;
}

/* { dg-output "" } */
