/* PR middle-end/64734 */

#include <stdlib.h>

void
foo (int *x, int *y)
{
  #pragma omp target map (alloc:x[0]) map (alloc:y[0:8])
  {
    int i;
    for (i = 0; i < 8; i++)
      if (y[i] != 2 + i)
	break;
    if (i != 8 || *x != 1)
      *x = 6;
    else
      {
	*x = 8;
	for (i = 0; i < 8; i++)
	  y[i] = 9 + i;
      }
  }
  #pragma omp target update from (y[0:8]) from (x[0])
}

void
bar (void)
{
  int x = 1, y[32] = { 0 };
  #pragma omp target data map (to:y[0:32]) map (to:x)
    ;
}

int
main ()
{
  int x = 1, y[8] = { 2, 3, 4, 5, 6, 7, 8, 9 }, i;
  #pragma omp target data map (to:y[0:8]) map (to:x)
    ;
  #pragma omp target data map (to:y[0:8]) map (to:x)
    {
      #pragma omp target update from (y[0:8]) from (x)
    }

  #pragma omp target data map (to:y[0:8]) map (to:x)
    foo (&x, &y[0]);

  if (x != 8)
    abort ();
  for (i = 0; i < 8; i++)
    if (y[i] != 9 + i)
      abort ();

  return 0;
}
