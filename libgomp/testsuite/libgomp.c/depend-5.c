#include <stdlib.h>

__attribute__((noinline, noclone)) void
f1 (int ifval)
{
  int x = 1, y = 2, z = 3;
  #pragma omp parallel
  #pragma omp single
  {
    #pragma omp task shared (x) depend(out: x)
    x = 2;
    #pragma omp task shared (x) depend(inout: x)
    {
      if (x != 2)
	abort ();
      x = 3;
    }
    #pragma omp task shared (x) depend(inout: x)
    {
      if (x != 3)
	abort ();
      x = 4;
    }
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (z) depend(in: z)
    if (z != 3)
      abort ();
    #pragma omp task shared (y) depend(in: y)
    if (y != 2)
      abort ();
    #pragma omp task shared (y) depend(in: y)
    if (y != 2)
      abort ();
    #pragma omp task shared (y) depend(in: y)
    if (y != 2)
      abort ();
    #pragma omp task shared (y) depend(in: y)
    if (y != 2)
      abort ();
    #pragma omp task if (ifval) shared (x, y) depend(in: x) depend(inout: y)
    {
      if (x != 4 || y != 2)
	abort ();
      y = 3;
    }
    if (ifval == 0)
      {
	/* The above if (0) task should have waited till all
	   the tasks with x and y dependencies finish.  */
	if (x != 4 || y != 3)
	  abort ();
	x = 5;
	y = 4;
      }
    #pragma omp task shared (z) depend(inout: z)
    {
      if (z != 3)
	abort ();
      z = 4;
    }
    #pragma omp task shared (z) depend(inout: z)
    {
      if (z != 4)
	abort ();
      z = 5;
    }
    #pragma omp taskwait
    if (x != (ifval ? 4 : 5) || y != (ifval ? 3 : 4) || z != 5)
      abort ();
    #pragma omp task if (ifval) shared (x, y) depend(in: x) depend(inout: y)
    {
      if (x != (ifval ? 4 : 5) || y != (ifval ? 3 : 4))
	abort ();
    }
  }
}

int
main ()
{
  f1 (0);
  f1 (1);
  return 0;
}
