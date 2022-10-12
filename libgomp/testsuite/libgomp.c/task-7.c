/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  #pragma omp task final (1)
  {
    if (!omp_in_final ())
      abort ();
    #pragma omp task
    {
      if (!omp_in_final ())
	abort ();
      #pragma omp target nowait
      if (omp_in_final ())
	abort ();
      if (!omp_in_final ())
	abort ();
      #pragma omp taskwait
    }
  }
  return 0;
}
