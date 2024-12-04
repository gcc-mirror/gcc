// { dg-options "-ltbb" }
// { dg-do run { target c++17 } }
// { dg-require-effective-target tbb_backend }

// Bug 108236 std::exclusive_scan with execution policy does not work in-place

#include <numeric>
#include <execution>
#include <testsuite_hooks.h>

void
test_pr108236()
{
  int vals[]{1, 2, 3};
  // Output range is the same as the input range:
  std::exclusive_scan(std::execution::seq, vals, vals+3, vals, 99);
  VERIFY( vals[0] == 99 );
  VERIFY( vals[1] == 100 );
  VERIFY( vals[2] == 102 );
}

int main()
{
  test_pr108236();
}
