/* { dg-additional-options "-Wno-deprecated-declarations" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_set_dynamic (0);
  omp_set_nested (1);
  if (omp_in_parallel ())
    abort ();
  #pragma omp parallel num_threads (3)
    if (omp_get_thread_num () == 2)
      {
	if (!omp_in_parallel ())
	  abort ();
	#pragma omp parallel num_threads (3)
	  if (omp_get_thread_num () == 1)
	    {
	      if (!omp_in_parallel ()
		  || omp_get_level () != 2
		  || omp_get_ancestor_thread_num (0) != 0
		  || omp_get_ancestor_thread_num (1) != 2
		  || omp_get_ancestor_thread_num (2) != 1
		  || omp_get_ancestor_thread_num (3) != -1)
		abort ();
	      #pragma omp target if (0)
		{
		  if (omp_in_parallel ()
		      || omp_get_level () != 0
		      || omp_get_ancestor_thread_num (0) != 0
		      || omp_get_ancestor_thread_num (1) != -1)
		    abort ();
		  #pragma omp parallel num_threads (2)
		  {
		    if (!omp_in_parallel ()
			|| omp_get_level () != 1
			|| omp_get_ancestor_thread_num (0) != 0
			|| omp_get_ancestor_thread_num (1)
			   != omp_get_thread_num ()
			|| omp_get_ancestor_thread_num (2) != -1)
		      abort ();
		  }
		}
	      #pragma omp target if (0)
		{
		  #pragma omp teams thread_limit (2)
		    {
		      #pragma omp distribute dist_schedule(static,1)
		      for (int i = 0; i < 1; ++i)
			if (omp_in_parallel ()
			    || omp_get_level () != 0
			    || omp_get_ancestor_thread_num (0) != 0
			    || omp_get_ancestor_thread_num (1) != -1)
			  abort ();
		      #pragma omp parallel num_threads (2)
		      {
			if (!omp_in_parallel ()
			    || omp_get_level () != 1
			    || omp_get_ancestor_thread_num (0) != 0
			    || omp_get_ancestor_thread_num (1)
			       != omp_get_thread_num ()
			    || omp_get_ancestor_thread_num (2) != -1)
			  abort ();
		      }
		    }
		}
	    }
      }
  return 0;
}
