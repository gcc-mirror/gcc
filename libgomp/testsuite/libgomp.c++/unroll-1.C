// { dg-additional-options "-std=c++11 -O0" }

#include <vector>
#include <stdio.h>

constexpr unsigned
fib (unsigned n)
{
  return n <= 2 ? 1 : fib (n-1) + fib (n-2);
}

int
test1 ()
{
  std::vector<int> v;

  for (unsigned i = 0; i <= 9; i++)
    v.push_back (1);

  int sum = 0;
  for (int k = 0; k < 10; k++)
    #pragma omp unroll partial(fib(3))
    for (int i : v)
      {
	for (int j = 8; j != -2; --j)
	  sum = sum + i;
      }

  return sum;
}

int
test2 ()
{
  std::vector<int> v;

  for (unsigned i = 0; i <= 10; i++)
    v.push_back (i);

  int sum = 0;
  #pragma omp parallel for reduction(+:sum)
  for (int k = 0; k < 10; k++)
    #pragma omp unroll
    #pragma omp unroll partial(fib(4))
    for (int i : v)
      {
	#pragma omp unroll full
	for (int j = 8; j != -2; --j)
	  sum = sum + i;
      }

  return sum;
}

int
main ()
{
  if (test1 () != 1000)
    __builtin_abort ();

  if (test2 () != 5500)
    __builtin_abort ();
}
