/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int err;

int
main ()
{
  int e;
#pragma omp parallel shared(err)
  {
    if (omp_in_final ())
      #pragma omp atomic write
	err = 1;
    #pragma omp task if (0) shared(err)
      {
	if (omp_in_final ())
	  #pragma omp atomic write
	    err = 1;
	#pragma omp task if (0) shared(err)
	  if (omp_in_final ())
	    #pragma omp atomic write
	      err = 1;
      }
    #pragma omp task final (1) shared(err)
      {
	if (!omp_in_final ())
	  #pragma omp atomic write
	    err = 1;
	#pragma omp taskyield
	#pragma omp taskwait
	#pragma omp task shared(err)
	  if (!omp_in_final ())
	    #pragma omp atomic write
	      err = 1;
      }
  }
  #pragma omp atomic read
    e = err;
  if (e)
    abort ();
  return 0;
}
