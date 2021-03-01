/* { dg-do run } */

#include <stdlib.h>

int
main (void)
{
  int i, j;
  int count = 0;

  #pragma omp parallel for collapse(2)
    for (i = 0; i < 80000; i++)
      for (j = 0; j < 80000; j++)
	if (i == 66666 && j == 77777)
	  /* In the collapsed loop space, this is iteration
	     66666*80000+77777==5,333,357,777.  If the type of the iterator
	     for the collapsed loop is only a 32-bit unsigned int, then this
	     iteration will exceed its maximum range and be skipped.  */
	  count++;

  if (count != 1)
    abort ();
}
