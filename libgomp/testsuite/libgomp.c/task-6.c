#include <stdlib.h>
#include <unistd.h>

int
main ()
{
  int x = 0, y = 0;
  #pragma omp parallel shared(x, y)
  #pragma omp master
  {
    #pragma omp task depend(out:y) shared(x, y)
    {
      sleep (1);
      x = 1;
      y = 1;
    }
    #pragma omp task depend(inout:y) shared(x, y)
    {
      if (x != 1 || y != 1)
	abort ();
      y++;
    }
  }
  if (x != 1 || y != 2)
    abort ();
  x = 0;
  y = 0;
  #pragma omp parallel
  #pragma omp master
  {
    #pragma omp task depend(out:y)
    {
      sleep (1);
      x = 1;
      y = 1;
    }
    #pragma omp task depend(inout:y)
    {
      if (x != 1 || y != 1)
	abort ();
      y++;
    }
  }
  if (x != 1 || y != 2)
    abort ();
  return 0;
}
