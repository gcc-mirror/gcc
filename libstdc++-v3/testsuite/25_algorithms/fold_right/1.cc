// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <algorithm>
#include <ranges>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3, 4, 5};
  auto v = x | views::filter([](int) { return true; });
  static_assert( ranges::bidirectional_range<decltype(v)>
		 && !ranges::random_access_range<decltype(v)> );
  auto f = [](int& x, int&& acc) {
    return 2 * acc + x;
  };
  VERIFY( ranges::fold_right(v, 0, f) == 129 );
  VERIFY( ranges::fold_right(v, 1, f) == 161 );
  VERIFY( ranges::fold_right(v.begin(), v.begin(), 1, f) == 1 );

  VERIFY( ranges::fold_right_last(v, f).value() == 129 );
  VERIFY( !ranges::fold_right_last(v.begin(), v.begin(), f).has_value() );

  return true;
}

constexpr bool
test02()
{
  double x[] = {0.5, 0.25, 0.125, 0.125};
  VERIFY( ranges::fold_right(x, 0, std::plus{}) == 1.0 );

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
}
