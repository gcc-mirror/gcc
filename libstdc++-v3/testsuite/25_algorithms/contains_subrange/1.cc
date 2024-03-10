// { dg-do run { target c++23 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3,4,5};
  int y[] = {2,3,4};
  int z[] = {4,5,6};
  __gnu_test::test_forward_range<int> rx(x);
  __gnu_test::test_forward_range<int> ry(y);
  __gnu_test::test_forward_range<int> rz(z);
  VERIFY( ranges::contains_subrange(rx, ry) );
  VERIFY( !ranges::contains_subrange(rx, rz) );
  VERIFY( ranges::contains_subrange(rx, ry, ranges::less{}) );
  VERIFY( ranges::contains_subrange(rx, rz, ranges::less{}) );
  auto plus3 = [](int n) { return n+3; };
  VERIFY( !ranges::contains_subrange(rx, ry, {}, plus3) );
  VERIFY( ranges::contains_subrange(rx, rz, {}, plus3) );
  VERIFY( ranges::contains_subrange(rx, ry, {}, plus3, plus3) );
  VERIFY( !ranges::contains_subrange(rx, rz, {}, plus3, plus3) );

  VERIFY( ranges::contains_subrange(x, x+2, y, y+1) );
  VERIFY( !ranges::contains_subrange(x, x+2, y, y+2) );
}

int
main()
{
  test01();
}
