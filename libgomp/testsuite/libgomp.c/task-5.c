/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>

int serr;

int
main ()
{
  int e;
#pragma omp parallel shared(serr)
  {
    if (omp_in_final ())
      #pragma omp atomic write
	serr = 1;
    #pragma omp task if (0) shared(serr)
      {
	if (omp_in_final ())
	  #pragma omp atomic write
	    serr = 1;
	#pragma omp task if (0) shared(serr)
	  if (omp_in_final ())
	    #pragma omp atomic write
	      serr = 1;
      }
    #pragma omp task final (1) shared(serr)
      {
	if (!omp_in_final ())
	  #pragma omp atomic write
	    serr = 1;
	#pragma omp taskyield
	#pragma omp taskwait
	#pragma omp task shared(serr)
	  if (!omp_in_final ())
	    #pragma omp atomic write
	      serr = 1;
      }
  }
  #pragma omp atomic read
    e = serr;
  if (e)
    abort ();
  return 0;
}
