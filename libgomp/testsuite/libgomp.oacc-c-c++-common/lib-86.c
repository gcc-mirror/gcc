/* { dg-do run } */

#include <stdlib.h>
#include <unistd.h>
#include <openacc.h>

int
main (int argc, char **argv)
{
  if (acc_get_num_devices (acc_device_nvidia) == 0)
    return 0;

  if (acc_get_current_cuda_device () != 0)
    abort ();

  acc_init (acc_device_host);

  if (acc_get_current_cuda_device () != 0)
    abort ();

  acc_shutdown (acc_device_host);

  if (acc_get_num_devices (acc_device_nvidia) == 0)
    return 0;

  if (acc_get_current_cuda_device () != 0)
    abort ();

  acc_init (acc_device_nvidia);

  if (acc_get_current_cuda_device () == 0)
    abort ();

  acc_shutdown (acc_device_nvidia);

  if (acc_get_current_cuda_device () != 0)
    abort ();

  return 0;
}

/* { dg-output "" } */
