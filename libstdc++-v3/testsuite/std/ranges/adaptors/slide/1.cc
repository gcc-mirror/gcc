// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <utility>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __cpp_lib_ranges_slide != 202202L
# error "Feature-test macro __cpp_lib_ranges_slide has wrong value in <ranges>"
#endif

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  auto v1 = std::array{1, 2} | views::slide(1);
  const auto i0 = v1.begin(), i1 = v1.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < v1.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( v1.end() - i1 == 1 );
  VERIFY( i1 - v1.end() == -1 );
  VERIFY( ranges::equal(std::move(v1) | views::join, (int[]){1, 2}) );

  int x[] = {1, 2, 3, 4};
  auto v2 = x | views::slide(2);
  auto i2 = v2.begin();
  i2 += 2;
  i2 -= -1;
  VERIFY( i2 == v2.end() );
  VERIFY( ranges::size(v2) == 3 );
  VERIFY( ranges::size(std::as_const(v2)) == 3 );
  VERIFY( ranges::equal(v2, (std::initializer_list<int>[]){{1, 2}, {2, 3}, {3, 4}},
			ranges::equal) );

  int y[] = {1, 2, 3, 4, 5};
  const auto v3 = y | views::slide(3);
  VERIFY( ranges::size(v3) == 3 );
  for (unsigned i = 0; i < ranges::size(x); i++)
    {
      VERIFY( &v3[i][0] == &y[i] + 0 );
      VERIFY( &v3[i][1] == &y[i] + 1 );
      VERIFY( &v3[i][2] == &y[i] + 2 );
    }

  const auto v5 = y | views::slide(5);
  VERIFY( ranges::size(v5) == 1 );
  VERIFY( ranges::equal(v5 | views::join, y) );

  const auto v6 = y | views::slide(6);
  VERIFY( ranges::empty(v6) );

  return true;
}

constexpr bool
test02()
{
  using __gnu_test::test_input_range;
  using __gnu_test::test_forward_range;
  using __gnu_test::test_random_access_range;

  using ty1 = ranges::slide_view<views::all_t<test_forward_range<int>>>;
  static_assert(ranges::forward_range<ty1>);
  static_assert(!ranges::bidirectional_range<ty1>);
  static_assert(!ranges::sized_range<ty1>);

  using ty2 = ranges::slide_view<views::all_t<test_random_access_range<int>>>;
  static_assert(ranges::random_access_range<ty2>);
  static_assert(ranges::sized_range<ty2>);

  return true;
}

constexpr bool
test03()
{
  auto v = views::iota(0, 4) | views::filter([](auto) { return true; }) | views::slide(2);
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

int
main()
{
  static_assert(test01());
  static_assert(test02());
  static_assert(test03());
}
