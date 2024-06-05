/* { dg-additional-options "-Wall -Wno-unknown-pragmas" } */

void
test1 (void)
{
  int sum = 0;

  for (int i = -3; i != 1; ++i)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

void
test2 (void)
{
  int sum = 0;

  #pragma omp unroll partial
  for (int i = -3; i != 1; ++i)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

void
test3 (void)
{
  int sum = 0;

  #pragma omp unroll partial
  for (int i = -3; i != 1; ++i)
    #pragma omp unroll partial
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

int sum;

void
test4 (void)
{
  #pragma omp for reduction(+:sum)
  #pragma omp unroll partial(5)
  for (int i = -3; i != 1; ++i)
    #pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

void
test5 (void)
{
  int sum = 0;

  #pragma omp parallel for reduction(+:sum)
  #pragma omp unroll partial(2)
  for (int i = -3; i != 1; ++i)
    #pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

void
test6 (void)
{
  int sum = 0;
  #pragma omp target parallel for reduction(+:sum)
  #pragma omp unroll partial(7)
  for (int i = -3; i != 1; ++i)
    #pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

void
test7 (void)
{
  int sum = 0;
#pragma omp target teams distribute parallel for reduction(+:sum)
#pragma omp unroll partial(7)
  for (int i = -3; i != 1; ++i)
#pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    __builtin_abort ();
}

int
main ()
{
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  test7 ();
  return 0;
}
