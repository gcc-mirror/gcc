/* PR middle-end/35549 */
/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main (void)
{
  int i = 6, n = 0;
  omp_set_dynamic (0);
  omp_set_nested (1);
  #pragma omp parallel shared (i) num_threads (3)
  {
    if (omp_get_num_threads () != 3)
      #pragma omp atomic
	n += 1;
    #pragma omp parallel shared (i) num_threads (4)
    {
      if (omp_get_num_threads () != 4)
	#pragma omp atomic
	  n += 1;
      #pragma omp critical
	i += 1;
    }
  }
  if (n == 0 && i != 6 + 3 * 4)
    abort ();
  return 0;
}
