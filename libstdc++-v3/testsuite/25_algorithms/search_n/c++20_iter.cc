// Verify std::search_n is C++20 iterator aware as per P2408R5.
// { dg-do compile { target c++20 } }

#include <algorithm>
#include <ranges>
#include <testsuite_hooks.h>

constexpr bool
test01()
{
  auto r = std::views::iota(0, 10);
  auto it = r.begin();
  static_assert( std::random_access_iterator<decltype(it)>);
  static_assert( std::same_as<std::iterator_traits<decltype(it)>::iterator_category,
			      std::input_iterator_tag> );
  it = std::search_n(it, it+10, 1, 5);
  VERIFY( it == r.begin() + 5 );
  return true;
}

static_assert(test01());
