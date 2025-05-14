// { dg-do run { target c++26 } }

#include <ranges>

#if __cpp_lib_ranges_to_input != 202502L
# error "Feature-test macro __cpp_lib_ranges_to_input has wrong value in <ranges>"
#endif

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

void
test01()
{
  std::vector<int> r{1,2,3};
  auto v = r | views::to_input;
  using type = decltype(v);
  static_assert( ranges::input_range<type> && !ranges::forward_range<type> );

  VERIFY( ranges::equal(v.base(), r) );
  VERIFY( v.size() == r.size() );
  VERIFY( v.end() == r.end() );
  auto it = v.begin();
  VERIFY( it != r.end() );
  *it = 42;
  ++it;
  *it = 43;
  it++;
  ranges::iter_swap(v.begin(), it);
  VERIFY( ranges::equal(r, (int[]){3,43,42}) );
  *it = ranges::iter_move(it);
  VERIFY( it == r.begin() + 2 );
  VERIFY( r.end() - it == 1 );
  VERIFY( it - r.end() == -1 );
}

void
test02()
{
  int x[] = {1,2,3};
  __gnu_test::test_input_range<int> rx(x);
  static_assert( !ranges::common_range<decltype(rx)> );
  auto v = rx | views::to_input;
  static_assert( std::same_as<decltype(v), decltype(views::all(rx))> );
  static_assert( std::same_as<decltype(x | views::to_input),
			      decltype(x | views::to_input | views::to_input)> );
}

int
main()
{
  test01();
  test02();
}
