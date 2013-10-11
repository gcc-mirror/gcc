#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  #pragma omp parallel
  #pragma omp single
  {
    int x = 1, y = 2;
    #pragma omp taskgroup
    {
      #pragma omp task shared (x) depend(in: x)
      {
	usleep (10000);
	if (x != 1)
	  abort ();
      }
      #pragma omp taskgroup
      {
	#pragma omp task shared (x) depend(in: x)
	{
	  usleep (15000);
	  if (x != 1)
	    abort ();
	}
	#pragma omp task shared (y) depend(inout: y)
	{
	  if (y != 2)
	    abort ();
	  y = 3;
	}
	#pragma omp taskgroup
	{
	  #pragma omp task shared (x) depend(in: x)
	  {
	    usleep (13000);
	    if (x != 1)
	      abort ();
	  }
	  #pragma omp taskgroup
	  {
	    #pragma omp task shared (x) depend(out: x)
	    x = 2;
	  }
	}
      }
    }
  }
  return 0;
}
