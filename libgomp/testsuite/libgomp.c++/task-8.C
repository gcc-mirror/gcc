// { dg-do run }

#include <omp.h>
#include <cstdlib>

int errval;

int
main ()
{
  int e;
#pragma omp parallel shared(errval)
  {
    if (omp_in_final ())
      #pragma omp atomic write
	errval = 1;
    #pragma omp task if (0) shared(errval)
      {
	if (omp_in_final ())
	  #pragma omp atomic write
	    errval = 1;
	#pragma omp task if (0) shared(errval)
	  if (omp_in_final ())
	    #pragma omp atomic write
	      errval = 1;
      }
    #pragma omp task final (1) shared(errval)
      {
	if (!omp_in_final ())
	  #pragma omp atomic write
	    errval = 1;
	#pragma omp taskyield
	#pragma omp taskwait
	#pragma omp task shared(errval)
	  if (!omp_in_final ())
	    #pragma omp atomic write
	      errval = 1;
      }
  }
  #pragma omp atomic read
    e = errval;
  if (e)
    abort ();
}
