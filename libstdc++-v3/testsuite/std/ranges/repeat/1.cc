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

constexpr bool
test04()
{
  // Verify P2474R2 changes to views::take/drop.
  auto r = views::repeat(42);

  auto rt = r | views::take(10);
  static_assert(views::__detail::__is_repeat_view<decltype(rt)>);
  VERIFY( ranges::equal(rt, views::repeat(42, 10)) );

  auto rd = r | views::drop(10);
  static_assert(std::same_as<decltype(rd), decltype(r)>);

  auto br = views::repeat(42, 37);

  auto brt = br | views::take(10);
  static_assert(std::same_as<decltype(brt), decltype(br)>);
  VERIFY( ranges::equal(brt, views::repeat(42, 10)) );

  auto brt100 = br | views::take(100);
  VERIFY( ranges::equal(brt100, br) );

  auto brd = br | views::drop(10);
  static_assert(std::same_as<decltype(brd), decltype(br)>);
  VERIFY( ranges::equal(brd, views::repeat(42, 27)) );

  auto brd100 = br | views::drop(100);
  VERIFY( ranges::empty(brd100) );

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
  static_assert(test04());
}
