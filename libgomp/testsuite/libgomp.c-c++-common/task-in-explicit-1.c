/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  if (omp_in_explicit_task ())
    abort ();
  #pragma omp task
  if (!omp_in_explicit_task ())
    abort ();
  #pragma omp task final (1)
  {
    if (!omp_in_explicit_task ())
      abort ();
    #pragma omp task 
    if (!omp_in_explicit_task ())
      abort ();
  }
  #pragma omp parallel
  {
    if (omp_in_explicit_task ())
      abort ();
    #pragma omp task if (0)
      {
	if (!omp_in_explicit_task ())
	  abort ();
	#pragma omp task if (0)
	  if (!omp_in_explicit_task ())
	    abort ();
      }
    #pragma omp task final (1)
      if (!omp_in_explicit_task ())
	abort ();
    #pragma omp barrier
    if (omp_in_explicit_task ())
      abort ();
    #pragma omp taskloop num_tasks (24)
    for (int i = 0; i < 32; ++i)
      if (!omp_in_explicit_task ())
	abort ();
    #pragma omp masked
    #pragma omp task
    if (!omp_in_explicit_task ())
      abort ();
    #pragma omp barrier
    if (omp_in_explicit_task ())
      abort ();
  }
  #pragma omp target
  {
    if (omp_in_explicit_task ())
      abort ();
    #pragma omp task if (0)
    if (!omp_in_explicit_task ())
      abort ();
    #pragma omp task
    if (!omp_in_explicit_task ())
      abort ();
  }
  #pragma omp target teams
  {
    #pragma omp distribute
    for (int i = 0; i < 4; ++i)
      if (omp_in_explicit_task ())
	abort ();
      else
	{
	  #pragma omp parallel
	  {
	    if (omp_in_explicit_task ())
	      abort ();
	    #pragma omp task
	    if (!omp_in_explicit_task ())
	      abort ();
	    #pragma omp barrier
	    if (omp_in_explicit_task ())
	      abort ();
	  }
	}
  }
  #pragma omp teams
  {
    #pragma omp distribute
    for (int i = 0; i < 4; ++i)
      if (omp_in_explicit_task ())
	abort ();
      else
	{
	  #pragma omp parallel
	  {
	    if (omp_in_explicit_task ())
	      abort ();
	    #pragma omp task
	    if (!omp_in_explicit_task ())
	      abort ();
	    #pragma omp barrier
	    if (omp_in_explicit_task ())
	      abort ();
	  }
	}
  }
  return 0;
}
