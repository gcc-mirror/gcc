/* { dg-do run } */
/* { dg-require-effective-target offload_device } */

#include <omp.h>
#include <stdlib.h>

#define N 10

int main ()
{
  int i;
  int offload[N];
  int num = omp_get_num_devices();

  #pragma omp parallel for
    for (i = 0; i < N; i++)
      #pragma omp target device(i) map(from: offload[i:1])
	offload[i] = omp_is_initial_device ();

  for (i = 0; i < num; i++)
    if (offload[i])
      abort ();

  for (i = num; i < N; i++)
    if (!offload[i])
      abort ();

  return 0;
}
