/* PR middle-end/96459 */

#include <stdlib.h>

int
main ()
{
  int niters = 0, i, j, k;
  #pragma omp teams reduction(+:niters)
  {
    #pragma omp distribute collapse(3)
    for (i = 0; i < 3; i++)
      for (j = 0; j < 8; j += 2)
	for (k = 0; k < 25; k += 3)
	  niters++;
  }
  if (niters != 108)
    abort ();
  return 0;
}
