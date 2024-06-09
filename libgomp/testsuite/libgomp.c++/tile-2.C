// { dg-additional-options "-std=c++11 -O0" }

#include <vector>

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
    #pragma omp tile sizes(fib(4))
    for (int i : v) {
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
  for (int k = 0; k < 10; k++)
    #pragma omp parallel for collapse(2) reduction(+:sum)
    #pragma omp tile sizes(fib(4), 1)
    for (int i : v)
      for (int j = 8; j > -2; --j)
	sum = sum + i;

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
