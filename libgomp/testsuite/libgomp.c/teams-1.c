/* { dg-additional-options "-Wno-deprecated-declarations" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  #pragma omp teams thread_limit (2)
  {
    if (omp_in_parallel ()
	|| omp_get_level () != 0
	|| omp_get_ancestor_thread_num (0) != 0
	|| omp_get_ancestor_thread_num (1) != -1)
      abort ();
    omp_set_dynamic (0);
    omp_set_nested (1);
    #pragma omp parallel num_threads (2)
    {
      if (!omp_in_parallel ()
	  || omp_get_level () != 1
	  || omp_get_ancestor_thread_num (0) != 0
	  || omp_get_ancestor_thread_num (1) != omp_get_thread_num ()
	  || omp_get_ancestor_thread_num (2) != -1)
	abort ();
    }
  }
  return 0;
}
