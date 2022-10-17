// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  auto v = views::repeat(42);
  static_assert(ranges::random_access_range<decltype(v)>
		&& !ranges::sized_range<decltype(v)>);
  auto i = ranges::begin(v);
  auto s = ranges::end(v);
  VERIFY( *i == 42 );
  VERIFY( i[0] == 42 );
  VERIFY( &i[0] == &i[1] );
  VERIFY( &*i == &*(i+1) );
  VERIFY( i != s );
  auto j = i + 5, k = i + 12;
  VERIFY( k - i == 12 );
  VERIFY( k - j == 7 );
  VERIFY( i - j == -5 );
  VERIFY( k > j );
  VERIFY( j < k );
  VERIFY( i + 5 == j );
  VERIFY( i != j );
  VERIFY( i + 5 <= j );
  VERIFY( j - 5 >= i );

  return true;
}

constexpr bool
test02()
{
  constexpr int bound = 20;
  auto v = views::repeat(42, bound);
  static_assert(ranges::random_access_range<decltype(v)>
		&& ranges::sized_range<decltype(v)>);
  VERIFY( ranges::equal(v, views::repeat(42) | views::take(bound)) );
  auto i = ranges::begin(v);
  auto s = ranges::end(v);
  VERIFY( *i == 42 );
  VERIFY( i[0] == 42 );
  VERIFY( &i[0] == &i[1] );
  VERIFY( &*i == &*(i+1) );
  VERIFY( i != s );
  auto j = i + 5, k = i + 12;
  VERIFY( k - i == 12 );
  VERIFY( k - j == 7 );
  VERIFY( i - j == -5 );
  VERIFY( k > j );
  VERIFY( j < k );
  VERIFY( i + 5 == j );
  VERIFY( i != j );
  VERIFY( i + 5 <= j );
  VERIFY( j - 5 >= i );

  VERIFY( ranges::size(v) == bound );
  VERIFY( s - i == bound );
  VERIFY( s - j == bound - (j - i) );
  VERIFY( i + bound == s );
  VERIFY( bound + i == s );

  return true;
}

constexpr bool
test03()
{
  struct A { int n, m; };
  auto v = ranges::repeat_view<A, unsigned>(std::piecewise_construct,
					    std::tuple{1, 2},
					    std::tuple{3});
  VERIFY( v[0].n == 1 );
  VERIFY( v[0].m == 2 );
  VERIFY( ranges::size(v) == 3 );

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
}
