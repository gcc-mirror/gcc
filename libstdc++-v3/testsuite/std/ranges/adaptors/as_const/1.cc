// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_as_const != 202311L
# error "Feature-test macro __cpp_lib_ranges_as_const has wrong value in <ranges>"
#endif

#include <algorithm>
#include <span>
#include <utility>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  auto v = x | views::filter([](int x) { return (x % 2) == 0; }) | views::as_const;

  using ty = decltype(v);
  static_assert(ranges::constant_range<ty>);
  static_assert(!ranges::constant_range<decltype(v.base())>);
  static_assert(std::same_as<ranges::range_reference_t<ty>, const int&>);
  static_assert(std::same_as<ranges::range_reference_t<decltype(v.base())>, int&>);

  VERIFY( ranges::equal(v, (int[]){2, 4, 6, 8, 10}) );
  VERIFY( ranges::equal(v | views::reverse, (int[]){10, 8, 6, 4, 2}) );

  return true;
}

constexpr bool
test02()
{
  int x[] = {1, 2, 3};
  std::same_as<ranges::empty_view<const int>>
    auto v1 = views::empty<int> | views::as_const;
  std::same_as<ranges::ref_view<const int[3]>>
    auto v2 = x | views::as_const;
  std::same_as<ranges::ref_view<const int[3]>>
    auto v3 = std::as_const(x) | views::as_const;
  std::same_as<ranges::ref_view<const int[3]>>
    auto v4 = std::as_const(x) | views::all | views::as_const;
  std::same_as<std::span<const int>>
    auto v5 = std::span{x, x+3} | views::as_const;
  std::same_as<ranges::as_const_view<ranges::chunk_view<ranges::ref_view<int[3]>>>>
    auto v6 = x | views::chunk(2) | views::as_const;
  VERIFY( v6.size() == 2 );

  return true;
}

void
test03()
{
  // PR libstdc++/109525
  std::vector<int> v;
  std::same_as<ranges::ref_view<const std::vector<int>>>
    auto r = views::as_const(v);
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  test03();
}
