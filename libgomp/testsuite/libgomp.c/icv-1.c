#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  int err = 0;

  omp_set_num_threads (4);
  if (omp_get_max_threads () != 4)
    abort ();
  #pragma omp parallel reduction(|: err) num_threads(1)
  {
    if (omp_get_max_threads () != 4)
      err |= 1;
    omp_set_num_threads (6);
    #pragma omp task if(0) shared(err)
    {
      if (omp_get_max_threads () != 6)
	err |= 2;
      omp_set_num_threads (5);
      if (omp_get_max_threads () != 5)
	err |= 4;
    }
    if (omp_get_max_threads () != 6)
      err |= 8;
  }
  if (err)
    abort ();
  if (omp_get_max_threads () != 4)
    abort ();
  return 0;
}
