/* PR c++/38650 */
/* { dg-do run } */

#include <stdlib.h>

int e;

int
main ()
{
  volatile int i, j = 10;
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < j; i += 1)
    e++;
  if (e != 10)
    abort ();
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < j; ++i)
    e++;
  if (e != 10)
    abort ();
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < j; i++)
    e++;
  if (e != 10)
    abort ();
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < 10; i += 1)
    e++;
  if (e != 10)
    abort ();
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < 10; ++i)
    e++;
  if (e != 10)
    abort ();
  e = 0;
#pragma omp parallel for reduction(+:e)
  for (i = 0; i < 10; i++)
    e++;
  if (e != 10)
    abort ();
  return 0;
}
