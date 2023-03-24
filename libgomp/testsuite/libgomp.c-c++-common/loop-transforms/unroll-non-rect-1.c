#include <stdio.h>
#include <stdlib.h>

void test1 ()
{
  int sum = 0;
  for (int i = -3; i != 1; ++i)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test2 ()
{
  int sum = 0;
  #pragma omp unroll partial
  for (int i = -3; i != 1; ++i)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test3 ()
{
  int sum = 0;
  #pragma omp unroll partial
  for (int i = -3; i != 1; ++i)
  #pragma omp unroll partial
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test4 ()
{
  int sum = 0;
#pragma omp for
#pragma omp unroll partial(5)
  for (int i = -3; i != 1; ++i)
#pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test5 ()
{
  int sum = 0;
#pragma omp parallel for reduction(+:sum)
#pragma omp unroll partial(2)
  for (int i = -3; i != 1; ++i)
#pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test6 ()
{
  int sum = 0;
#pragma omp target parallel for reduction(+:sum)
#pragma omp unroll partial(7)
  for (int i = -3; i != 1; ++i)
#pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
}

void test7 ()
{
  int sum = 0;
#pragma omp target teams distribute parallel for reduction(+:sum)
#pragma omp unroll partial(7)
  for (int i = -3; i != 1; ++i)
#pragma omp unroll partial(2)
    for (int j = -2; j < i * -1; ++j)
      sum++;

  if (sum != 14)
    {
      fprintf (stderr, "%s: Wrong sum: %d\n", __FUNCTION__, sum);
      abort ();
    }
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
