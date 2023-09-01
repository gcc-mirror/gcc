// { dg-do run { target c++23 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;

void
test01()
{
  int x[3] = {};
  __gnu_test::test_output_range<int> rx(x);
  auto r0 = ranges::iota(rx, 0);
  VERIFY( r0.out.ptr == x+3 );
  VERIFY( r0.value == 3 );
  VERIFY( ranges::equal(x, (int[]){0,1,2}) );
  auto r1 = ranges::iota(x, x+2, 5);
  VERIFY( r1.out == x+2 );
  VERIFY( r1.value == 7 );
  VERIFY( ranges::equal(x, (int[]){5,6,2}) );
}

int
main()
{
  test01();
}
