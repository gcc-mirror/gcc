/* { dg-xfail-run-if TODO { offload_device_intel_mic } } */

#include <omp.h>
#include <stdlib.h>

int main (void)
{

  int host_device_num = omp_get_device_num ();

  if (host_device_num != omp_get_initial_device ())
    abort ();

  int device_num;
  int initial_device;

  #pragma omp target map(from: device_num, initial_device)
  {
    initial_device = omp_is_initial_device ();
    device_num = omp_get_device_num ();
  }

  if (initial_device && host_device_num != device_num)
    abort ();

  if (!initial_device && host_device_num == device_num)
    abort ();

  return 0;
}
