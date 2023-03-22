// { dg-do run }

#include <cassert>

#pragma omp declare target

template<typename T>
struct str {
  str(T x) : _x(x) { }
  T add(str o) { return _x + o._x; }
  T _x;
};

str<long> v1(5);

#pragma omp end declare target

int main()
{
  long res = -1;
  str<long> v2(2);

#pragma omp target map(from:res)
  {
    res = v1.add(v2);
  }

  assert (res == 7);

  return 0;
}
