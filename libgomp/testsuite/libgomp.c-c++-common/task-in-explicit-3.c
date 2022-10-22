/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int a;

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
      #pragma omp taskgroup task_reduction (+: a)
      {
	if (!omp_in_explicit_task ())
	  abort ();
	#pragma omp task in_reduction (+: a)
	{
	  ++a;
	  if (!omp_in_explicit_task ())
	    abort ();
	}
      }
      if (!omp_in_explicit_task ())
	abort ();
      #pragma omp taskwait
    }
  }
  return 0;
}
