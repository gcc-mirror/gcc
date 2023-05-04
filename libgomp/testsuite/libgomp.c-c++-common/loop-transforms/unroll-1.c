/* { dg-additional-options { -Wall -Wno-unknown-pragmas } } */

#include <stdio.h>

int compute_sum1 ()
{
  int sum = 0;
  int i,j;
#pragma omp parallel for reduction(+:sum) lastprivate(j)
#pragma omp unroll partial
  for (i = 3; i < 10; ++i)
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int compute_sum2()
{
  int sum = 0;
  int i,j;
#pragma omp parallel for reduction(+:sum) lastprivate(j)
#pragma omp unroll partial(5)
  for (i = 3; i < 10; ++i)
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int compute_sum3()
{
  int sum = 0;
  int i,j;
#pragma omp parallel for reduction(+:sum)
#pragma omp unroll partial(1)
  for (i = 3; i < 10; ++i)
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int main ()
{
  int result;
  result = compute_sum1 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  result = compute_sum2 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  result = compute_sum3 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  return 0;
}
