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
  auto v1 = std::array{1, 2, 3} | views::adjacent_transform<1>(std::identity{});
  VERIFY( ranges::equal(v1, (int[]){1, 2, 3}) );
  const auto i0 = v1.begin(), i1 = v1.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < v1.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( v1.end() - i1 == 2 );
  VERIFY( i1 - v1.end() == -2 );
  ranges::iter_swap(i0, i1);
  VERIFY( ranges::equal(std::move(v1), (int[]){2, 1, 3}) );

  auto v2 = std::array{1, -1, 2, -2} | views::pairwise_transform(std::multiplies{});
  auto i2 = v2.begin();
  i2 += 1;
  i2 -= -2;
  VERIFY( i2 == v2.end() );
  VERIFY( ranges::size(v2) == 3 );
  VERIFY( ranges::size(std::as_const(v2)) == 3 );
  VERIFY( ranges::equal(v2, (int[]){-1, -2, -4}) );

  int y[] = {1, 2, 3, 4, 5, 6};
  auto v3 = y | views::adjacent_transform<3>([](auto... xs) { return ranges::max({xs...}); });
  VERIFY( ranges::size(v3) == 4 );
  VERIFY( ranges::equal(v3, (int[]){3, 4, 5, 6}) );

  const auto v6 = y | views::adjacent_transform<6>([](auto...) { return 0; });
  VERIFY( ranges::equal(v6, (int[]){0}) );

  const auto v7 = y | views::adjacent_transform<7>([](auto...) { return 0; });
  VERIFY( ranges::empty(v7) );

  const auto v0 = y | views::adjacent_transform<0>([] { return 0; });
  VERIFY( ranges::empty(v0) );

  return true;
}

constexpr bool
test02()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_random_access_range;

  using ty1 = ranges::adjacent_transform_view<views::all_t<test_forward_range<int>>,
					      std::plus<>, 2>;
  static_assert(ranges::forward_range<ty1>);
  static_assert(!ranges::bidirectional_range<ty1>);
  static_assert(!ranges::sized_range<ty1>);

  using ty2 = ranges::adjacent_transform_view<views::all_t<test_random_access_range<int>>,
					      decltype([](int, int, int) { return 0;}), 3>;
  static_assert(ranges::random_access_range<ty2>);
  static_assert(ranges::sized_range<ty2>);

  return true;
}

constexpr bool
test03()
{
  auto v = views::iota(0, 4)
    | views::filter([](auto) { return true; })
    | views::pairwise_transform(std::plus{});
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

void
test04()
{
  extern int x[5];
  struct move_only {
    move_only() { }
    move_only(move_only&&) { }
    int operator()(int i, int j) const { return i + j; }
  };
  // P2494R2 Relaxing range adaptors to allow for move only types
  static_assert( requires { views::pairwise_transform(x, move_only{}); } );
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
  test04();
}
