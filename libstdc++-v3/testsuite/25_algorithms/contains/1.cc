// { dg-do run { target c++23 } }

#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __cpp_lib_ranges_contains != 202207L
# error "Feature-test macro __cpp_lib_ranges_contains has wrong value in <algorithm>"
#endif

namespace ranges = std::ranges;

void
test01()
{
  int x[] = {1,2,3};
  using to_input = __gnu_test::test_input_range<int>;
  VERIFY( ranges::contains(to_input(x), 1) );
  VERIFY( ranges::contains(to_input(x), 2) );
  VERIFY( ranges::contains(to_input(x), 3) );
  VERIFY( !ranges::contains(to_input(x), 4) );
  VERIFY( !ranges::contains(x, x+2, 3) );
  auto neg = [](int n) { return -n; };
  VERIFY( ranges::contains(to_input(x), -1, neg) );
  VERIFY( ranges::contains(to_input(x), -2, neg) );
  VERIFY( ranges::contains(to_input(x), -3, neg) );
  VERIFY( !ranges::contains(to_input(x), -4, neg) );

  VERIFY( !ranges::contains(x, x+2, -3, neg) );
}

int
main()
{
  test01();
}
