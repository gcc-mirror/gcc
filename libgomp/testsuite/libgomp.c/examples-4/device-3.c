/* { dg-do run } */
/* { dg-require-effective-target offload_device } */

#include <omp.h>
#include <stdlib.h>

int main ()
{
  int res;
  int default_device = omp_get_default_device ();

  #pragma omp target map(from: res)
    res = omp_is_initial_device ();

  if (res)
    abort ();

  omp_set_default_device (omp_get_num_devices ());

  #pragma omp target map(from: res)
    res = omp_is_initial_device ();

  if (!res)
    abort ();

  return 0;
}
