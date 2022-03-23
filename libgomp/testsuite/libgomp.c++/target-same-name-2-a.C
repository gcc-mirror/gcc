/* { dg-skip-if "" { *-*-* } } */
/* Used by target-same-name-2.c */

#include <complex>

template<typename T>
int
test_map ()
{
  std::complex<T> a(2, 1), a_check;
#pragma omp target map(from : a_check)
  {
    a_check = a;
  }
  if (a == a_check)
    return 42;
  return 0;
}

template<typename T>
static int
test_map_static ()
{
  std::complex<T> a(-4, 5), a_check;
#pragma omp target map(from : a_check)
  {
    a_check = a;
  }
  if (a == a_check)
    return 441;
  return 0;
}

int
test_a ()
{
  int res = test_map<float>();
  if (res != 42)
    __builtin_abort ();
  return res;
}

int
test_a2 ()
{
  int res = test_map_static<float>();
  if (res != 441)
    __builtin_abort ();
  return res;
}
