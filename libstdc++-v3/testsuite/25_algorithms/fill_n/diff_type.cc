// { dg-do compile { target c++11 } }

#include <algorithm>
#include <testsuite_iterators.h>

void
test_pr121890()
{
  // algorithms do not use iterator's difference_type for arithmetic
  int a[1];
  __gnu_test::random_access_container<int> c(a);
  std::fill_n(c.begin(), 1U, 0);
}
