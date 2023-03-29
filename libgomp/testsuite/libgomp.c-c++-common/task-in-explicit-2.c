/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  #pragma omp task
  {
    if (!omp_in_explicit_task ())
      abort ();
    #pragma omp task
    {
      if (!omp_in_explicit_task ())
	abort ();
      #pragma omp target nowait
      if (omp_in_explicit_task ())
	abort ();
      if (!omp_in_explicit_task ())
	abort ();
      #pragma omp taskwait
    }
  }
  return 0;
}
