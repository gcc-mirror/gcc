// { dg-additional-options "-std=c++11 -O0" }

#include <vector>

int
main ()
{
  std::vector<int> v;
  std::vector<int> w;

  for (unsigned i = 0; i <= 9; i++)
    v.push_back (i);

  int iter = 0;
  #pragma omp for
  #pragma omp tile sizes(5)
  for (int i : v)
    {
      w.push_back (iter);
      iter++;
    }

  for (int i = 0; i < w.size (); i++)
    if (w[i] != i)
      __builtin_abort ();
}
