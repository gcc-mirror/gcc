// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_chunk_by != 202202L
# error "Feature-test macro __cpp_lib_ranges_chunk_by has wrong value in <ranges>"
#endif

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 2, 3, 0, 4, 5, 2};
  auto v = x | views::chunk_by(ranges::less_equal{});
  static_assert(ranges::bidirectional_range<decltype(v)>
		&& ranges::common_range<decltype(v)>);
  VERIFY( ranges::equal(v, (std::initializer_list<int>[]){{1, 2, 2, 3}, {0, 4, 5}, {2}},
			ranges::equal) );
  VERIFY( ranges::equal(v | views::reverse,
			(std::initializer_list<int>[]){{2}, {0, 4, 5}, {1, 2, 2, 3}},
			ranges::equal) );
  VERIFY( ranges::equal(v | views::join, x) );
  auto i = v.begin();
  auto j = i;
  j++;
  VERIFY( i == i && i != v.end() );
  VERIFY( j == j && j != v.end() );
  VERIFY( j != i );
  j--;
  VERIFY( j == i );

  return true;
}

void
test02()
{
  int x[] = {1, 2, 3};
  __gnu_test::test_forward_range<int> rx(x);
  auto v = rx | views::chunk_by(ranges::equal_to{});
  static_assert(!ranges::bidirectional_range<decltype(v)>
		&& !ranges::common_range<decltype(v)>);
  VERIFY( ranges::equal(v, x | views::transform(views::single), ranges::equal) );
  auto i = v.begin();
  VERIFY( i != v.end() );
  ranges::advance(i, 3);
  VERIFY( i == v.end() );
}

void
test03()
{
  // LWG 3796
  ranges::chunk_by_view<ranges::empty_view<int>, ranges::equal_to> r;
}

constexpr bool
test04()
{
  // PR libstdc++/108291
  using namespace std::literals;
  std::string_view s = "hello";
  auto r = s | views::chunk_by(std::less{});
  VERIFY( ranges::equal(r,
			(std::string_view[]){"h"sv, "el"sv, "lo"sv},
			ranges::equal) );
  VERIFY( ranges::equal(r | views::reverse,
			(std::string_view[]){"lo"sv, "el"sv, "h"sv},
			ranges::equal) );

  return true;
}

void
test05()
{
  // PR libstdc++/109474
  std::vector<bool> v = {true, false, true, true, false, false};
  auto r = v | views::chunk_by(std::equal_to{});
  VERIFY( ranges::equal(r,
			(std::initializer_list<bool>[])
			  {{true}, {false}, {true, true}, {false, false}},
			ranges::equal) );
  VERIFY( ranges::equal(r | views::reverse,
			(std::initializer_list<bool>[])
			  {{false, false}, {true, true}, {false}, {true}},
			ranges::equal) );
}

int
main()
{
  static_assert(test01());
  test02();
  test03();
  static_assert(test04());
  test05();
}
