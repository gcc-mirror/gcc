#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_set_dynamic (0);
  #pragma omp parallel num_threads (4)
  #pragma omp target if (0)
  #pragma omp single
  if (omp_get_num_threads () != 1)
    abort ();
  return 0;
}
