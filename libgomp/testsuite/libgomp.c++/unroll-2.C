// { dg-do run }
// { dg-additional-options "-std=c++11" }

#include <vector>
#include <iostream>

int
main ()
{
  std::vector<std::vector<int>> v;
  std::vector<int> w;

  for (unsigned i = 0; i < 10; i++)
    {
      std::vector<int> u;
      for (unsigned j = 0; j < 10; j++)
	u.push_back (j);
      v.push_back (u);
    }

  #pragma omp for
  #pragma omp unroll partial(7)
  for (auto u : v)
    for (int x : u)
      w.push_back (x);

  std::size_t l = w.size ();
  for (std::size_t i = 0; i < l; i++)
    if (w[i] != i % 10)
      __builtin_abort ();
}
