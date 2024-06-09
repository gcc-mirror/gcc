/* { dg-additional-options "-Wall -Wno-unknown-pragmas" } */

int
compute_sum1 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp parallel for reduction(+:sum) lastprivate(j)
  #pragma omp unroll partial
  for (i = 3; i < 10; ++i)
    for (j = -2; j < 7; ++j)
      sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int
compute_sum2 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp parallel for reduction(+:sum) lastprivate(j)
  #pragma omp unroll partial(5)
  for (i = 3; i < 10; ++i)
    for (j = -2; j < 7; ++j)
      sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int
compute_sum3 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp parallel for reduction(+:sum) lastprivate(j)
  #pragma omp unroll partial(1)
  for (i = 3; i < 10; ++i)
    for (j = -2; j < 7; ++j)
      sum++;

  if (j != 7)
    __builtin_abort ();

  return sum;
}

int
main ()
{
  if (compute_sum1 () != 7 * 9)
    __builtin_abort ();

  if (compute_sum2 () != 7 * 9)
    __builtin_abort ();

  if (compute_sum3 () != 7 * 9)
    __builtin_abort ();

  return 0;
}
