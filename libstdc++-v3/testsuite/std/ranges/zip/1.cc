// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_zip != 202110L
# error "Feature-test macro __cpp_lib_ranges_zip has wrong value in <ranges>"
#endif

#include <algorithm>
#include <utility>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  static_assert(ranges::empty(views::zip()));
  static_assert(ranges::empty(views::empty<int>));

  auto z1 = views::zip(std::array{1, 2});
  const auto i0 = z1.begin(), i1 = z1.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < z1.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( z1.end() - i1 == 1 );
  VERIFY( i1 - z1.end() == -1 );
  ranges::iter_swap(i0, i1);
  VERIFY( ranges::equal(std::move(z1) | views::keys, (int[]){2, 1}) );

  auto z2 = views::zip(std::array{1, 2}, std::array{3, 4, 5});
  auto i2 = z2.begin();
  i2 += 1;
  i2 -= -1;
  VERIFY( i2 == z2.end() );
  VERIFY( ranges::size(z2) == 2 );
  VERIFY( ranges::size(std::as_const(z2)) == 2 );
  VERIFY( std::get<0>(z2[0]) == 1 && std::get<1>(z2[0]) == 3 );
  VERIFY( std::get<0>(z2[1]) == 2 && std::get<1>(z2[1]) == 4 );
  for (const auto [x, y] : z2)
    {
      VERIFY( y - x == 2 );
      std::swap(x, y);
    }

  int x[2] = {1, 2}, y[2] = {3, 4}, z[2] = {5, 6};
  const auto z3 = views::zip(x, y, z);
  VERIFY( ranges::size(z3) == 2 );
  for (int i = 0; i < ranges::size(x); i++)
    {
      VERIFY( &std::get<0>(z3[i]) == &x[i] );
      VERIFY( &std::get<1>(z3[i]) == &y[i] );
      VERIFY( &std::get<2>(z3[i]) == &z[i] );
    }

  return true;
}

constexpr bool
test02()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_random_access_range;

  using ty1 = ranges::zip_view<views::all_t<test_forward_range<int>>,
			       views::all_t<test_random_access_range<int>>>;
  static_assert(ranges::forward_range<ty1>);
  static_assert(!ranges::random_access_range<ty1>);
  static_assert(!ranges::sized_range<ty1>);

  using ty2 = ranges::zip_view<views::all_t<test_forward_range<int>>,
			       views::all_t<test_input_range<int>>,
			       views::all_t<test_forward_range<int>>>;
  static_assert(ranges::input_range<ty2>);
  static_assert(!ranges::forward_range<ty2>);
  static_assert(!ranges::sized_range<ty2>);

  return true;
}

constexpr bool
test03()
{
  int u[] = {1, 2, 3, 4}, v[] = {4, 5, 6}, w[] = {7, 8, 9, 10};
  auto z = views::zip(u | views::filter([](auto) { return true; }), v, w);
  using ty = decltype(z);
  static_assert(ranges::forward_range<ty>);
  static_assert(!ranges::common_range<ty>);
  static_assert(!ranges::sized_range<ty>);
  VERIFY( z.begin() == z.begin() );
  VERIFY( z.begin() != z.end() );
  VERIFY( ranges::next(z.begin(), 3) == z.end() );
  auto it = z.begin();
  ++it;
  it++;
  it--;
  --it;
  VERIFY( it == z.begin() );

  return true;
}

constexpr bool
test04()
{
  // PR libstdc++/106766
#if __SIZEOF_INT128__
  auto r = views::zip(views::iota(__int128(0), __int128(1)));
#else
  auto r = views::zip(views::iota(0ll, 1ll));
#endif
  auto i = r.begin();
  auto s = r.end();
  VERIFY( s - i == 1 );
  VERIFY( i + 1 - i == 1 );

  return true;
}

constexpr bool
test05()
{
  // PR libstdc++/109203
  int x[] = {1, 1, 2};
  int y[] = {2, 1, 3};
  auto r = views::zip(x, y);
  ranges::sort(r);

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
  static_assert(test04());
  static_assert(test05());
}
