// Verify std::inclusive_scan is C++20 iterator aware as per P2408R5.
// { dg-do compile { target c++23 } }

#include <numeric>
#include <ranges>
#include <testsuite_hooks.h>

constexpr bool
test01()
{
  int x[10] = {1,2,3,4,5,6,7,8,9,10};
  auto r = std::views::zip(x);
  auto it = r.begin();
  static_assert( std::random_access_iterator<decltype(it)>);
  static_assert( std::same_as<std::iterator_traits<decltype(it)>::iterator_category,
			      std::input_iterator_tag> );
  std::tuple<int> y[10];
  std::inclusive_scan(it, it+10, y,
		      [](std::tuple<int> a, std::tuple<int> b) -> std::tuple<int> {
			return std::get<0>(a) + std::get<0>(b);
		      });
  VERIFY( std::get<0>(y[9]) == 55 );
  return true;
}

static_assert(test01());
