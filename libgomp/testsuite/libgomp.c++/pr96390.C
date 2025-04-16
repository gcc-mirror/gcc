/* { dg-additional-options "-O0 -fdump-tree-omplower" } */

#include <cstdlib>
#include <type_traits>

template<int Dim> struct V {
  int version_called;

  template<bool B = (Dim == 0),
           typename = typename std::enable_if<B>::type>
  V ()
  {
    version_called = 1;
  }

  template<typename TArg0,
           typename = typename std::enable_if<(std::is_same<unsigned long,
                                               typename std::decay<TArg0>::type>::value)>::type>
  V (TArg0)
  {
    version_called = 2;
  }
};

template<int Dim> struct S {
  V<Dim> v;
};

int
main ()
{
  int version_set[2] = {-1, -1};

#pragma omp target map(from: version_set[0:2])
  {
    S<0> s;
    version_set[0] = s.v.version_called;
    V<1> v2((unsigned long) 1);
    version_set[1] = v2.version_called;
  }

  if (version_set[0] != 1 || version_set[1] != 2)
    abort ();
  return 0;
}

/* "3" for S<0>::S, V<0>::V<>, and V<1>::V<long unsigned int>:  */
/* { dg-final { scan-tree-dump-times "__attribute__..omp declare target" 3 "omplower" } } */
