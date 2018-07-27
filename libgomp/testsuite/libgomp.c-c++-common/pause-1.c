#include <omp.h>
#include <stdlib.h>

int a[64];

int
main ()
{
  int i;
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    a[i] = i;
  omp_pause_resource (omp_pause_soft, omp_get_initial_device ());
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    a[i] += i;
  omp_pause_resource_all (omp_pause_hard);
  #pragma omp parallel for
  for (i = 0; i < 64; i++)
    if (a[i] != 2 * i)
      abort ();
  return 0;
}
