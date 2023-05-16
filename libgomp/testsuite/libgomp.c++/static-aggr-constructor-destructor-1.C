// { dg-do run }

#include <cassert>

#pragma omp declare target

struct str {
  str(int x) : _x(x) { }
  int add(str o) { return _x + o._x; }
  int _x;
} v1(5);

#pragma omp end declare target

int main()
{
  int res = -1;
  str v2(2);

#pragma omp target map(from:res)
  {
    res = v1.add(v2);
  }

  assert (res == 7);

  return 0;
}
