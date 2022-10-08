/* { dg-do run } */

#include <cassert>

#define N 1024

class M {
  int array[N];

public:
  M ()
  {
    for (int i = 0; i < N; i++)
      array[i] = 0;
  }

  void incr_with_this (int c)
  {
#pragma omp target map(this->array[:N])
    for (int i = 0; i < N; i++)
      array[i] += c;
  }

  void incr_without_this (int c)
  {
#pragma omp target map(array[:N])
    for (int i = 0; i < N; i++)
      array[i] += c;
  }

  void incr_implicit (int c)
  {
#pragma omp target
    for (int i = 0; i < N; i++)
      array[i] += c;
  }

  void check (int c)
  {
    for (int i = 0; i < N; i++)
      assert (array[i] == c);
  }
};

int
main (int argc, char *argv[])
{
  M m;

  m.check (0);
  m.incr_with_this (3);
  m.check (3);
  m.incr_without_this (5);
  m.check (8);
  m.incr_implicit (2);
  m.check (10);

  return 0;
}
