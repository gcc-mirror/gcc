// Verify std::sample is C++20 iterator aware as per P2408R5.
// { dg-do compile { target c++20 } }

#include <algorithm>
#include <random>
#include <ranges>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

std::mt19937 rng;

void
test01()
{
  auto r = std::views::iota(10);
  auto it = r.begin();
  static_assert( std::random_access_iterator<decltype(it)>);
  static_assert( std::same_as<std::iterator_traits<decltype(it)>::iterator_category,
			      std::input_iterator_tag> );
  int buf[10];
  __gnu_test::output_container<int> s(buf);
  std::sample(it, it+10, s.begin(), 10, rng);
}
