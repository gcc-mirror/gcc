// { dg-do run }

#include <cstdlib>
#include <cassert>

struct S {
  int *myarr;
};

#pragma omp declare mapper (S s) map(to:s.myarr) map (tofrom: s.myarr[0:20])

namespace A {
#pragma omp declare mapper (S s) map(to:s.myarr) map (tofrom: s.myarr[0:100])
}

namespace B {
#pragma omp declare mapper (S s) map(to:s.myarr) map (tofrom: s.myarr[100:100])
}

namespace A
{
  void incr_a (S &my_s)
  {
#pragma omp target
    {
      for (int i = 0; i < 100; i++)
	my_s.myarr[i]++;
    }
  }
}

namespace B
{
  void incr_b (S &my_s)
  {
#pragma omp target
    {
      for (int i = 100; i < 200; i++)
	my_s.myarr[i]++;
    }
  }
}

int main (int argc, char *argv[])
{
  S my_s;

  my_s.myarr = (int *) calloc (200, sizeof (int));

#pragma omp target
  {
    for (int i = 0; i < 20; i++)
      my_s.myarr[i]++;
  }

  A::incr_a (my_s);
  B::incr_b (my_s);

  for (int i = 0; i < 200; i++)
    assert (my_s.myarr[i] == (i < 20) ? 2 : 1);

  return 0;
}
