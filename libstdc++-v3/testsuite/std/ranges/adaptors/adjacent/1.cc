// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <utility>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  static_assert(ranges::empty(std::array{1, 2, 3} | views::adjacent<0>));

  auto v1 = std::array{1, 2} | views::adjacent<1>;
  const auto i0 = v1.begin(), i1 = v1.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < v1.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( v1.end() - i1 == 1 );
  VERIFY( i1 - v1.end() == -1 );
  ranges::iter_swap(i0, i1);
  VERIFY( ranges::equal(std::move(v1) | views::keys, (int[]){2, 1}) );

  int x[] = {1, 2, 3, 4};
  auto v2 = x | views::pairwise;
  auto i2 = v2.begin();
  i2 += 2;
  i2 -= -1;
  VERIFY( i2 == v2.end() );
  VERIFY( ranges::size(v2) == 3 );
  VERIFY( ranges::size(std::as_const(v2)) == 3 );
  VERIFY( ranges::equal(v2 | views::keys, (int[]){1, 2, 3}) );
  VERIFY( ranges::equal(v2 | views::values, (int[]){2, 3, 4}) );

  int y[] = {1, 2, 3, 4, 5};
  const auto v3 = y | views::adjacent<3>;
  VERIFY( ranges::size(v3) == 3 );
  for (unsigned i = 0; i < ranges::size(x); i++)
    {
      VERIFY( &std::get<0>(v3[i]) == &y[i] + 0 );
      VERIFY( &std::get<1>(v3[i]) == &y[i] + 1 );
      VERIFY( &std::get<2>(v3[i]) == &y[i] + 2 );
    }

  // LWG 3848 - adjacent_view etc missing base accessor
  v3.base();

  const auto v5 = y | views::adjacent<5>;
  VERIFY( ranges::equal(v5, views::single(std::make_tuple(1, 2, 3, 4, 5))) );

  const auto v6 = y | views::adjacent<6>;
  VERIFY( ranges::empty(v6) );

  const auto v0 = y | views::adjacent<0>;
  VERIFY( ranges::empty(v0) );

  return true;
}

constexpr bool
test02()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_random_access_range;

  using ty1 = ranges::adjacent_view<views::all_t<test_forward_range<int>>, 2>;
  static_assert(ranges::forward_range<ty1>);
  static_assert(!ranges::bidirectional_range<ty1>);
  static_assert(!ranges::sized_range<ty1>);

  using ty2 = ranges::adjacent_view<views::all_t<test_random_access_range<int>>, 3>;
  static_assert(ranges::random_access_range<ty2>);
  static_assert(ranges::sized_range<ty2>);

  return true;
}

constexpr bool
test03()
{
  auto v = views::iota(0, 4) | views::filter([](auto) { return true; }) | views::pairwise;
  using ty = decltype(v);
  static_assert(ranges::forward_range<ty>);
  static_assert(ranges::common_range<ty>);
  static_assert(!ranges::sized_range<ty>);
  VERIFY( v.begin() == v.begin() );
  VERIFY( v.begin() != v.end() );
  VERIFY( ranges::next(v.begin(), 3) == v.end() );
  auto it = v.begin();
  ++it;
  it++;
  VERIFY( ranges::next(it) == v.end() );
  it--;
  --it;
  VERIFY( it == v.begin() );

  return true;
}

constexpr bool
test04()
{
  // PR libstdc++/106798
  auto r = views::single(0) | views::lazy_split(0) | views::pairwise;
  decltype(ranges::cend(r)) s = r.end();
  VERIFY( r.begin() != s );

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
