// { dg-do run { target c++26 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_concat != 202403L
# error "Feature-test macro __cpp_lib_ranges_concat has wrong value in <ranges>"
#endif

#include <algorithm>
#include <vector>
#include <array>
#include <sstream>
#include <utility>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  std::vector<int> v1{1, 2, 3}, v2{4, 5}, v3{};
  std::array a{6, 7, 8};
  auto s = views::single(9);

  auto v = views::concat(v1, v2, v3, a, s);
  VERIFY( ranges::size(v) == 9 );
  VERIFY( ranges::size(std::as_const(v)) == 9 );
  VERIFY( ranges::equal(v, views::iota(1, 10)) );
  VERIFY( ranges::equal(v | views::reverse,
			views::iota(1, 10) | views::reverse) );

  auto it0 = v.begin();
  auto cit = std::as_const(v).begin();
  VERIFY( it0 == it0 );
  VERIFY( cit == cit );
  VERIFY( it0 == cit );
  for (int i = 0; i < 10; i++)
    {
      VERIFY( it0 + i - it0 == i );
      VERIFY( it0 + i - (it0 + 1) == i - 1 );
      VERIFY( it0 + i - (it0 + 3) == i - 3 );
      VERIFY( it0 + i - (it0 + 5) == i - 5 );
      VERIFY( it0 + i - i + i == it0 + i );
      VERIFY( it0 + i - (it0 + i) == 0 );
    }
  VERIFY( std::default_sentinel - it0 == 9 );
  VERIFY( it0 + 9 == std::default_sentinel );

  auto it5 = it0+5;
  ranges::iter_swap(it0, it5);
  VERIFY( *it0 == 6 && *it5 == 1 );
  ranges::iter_swap(it0, it5);
  *it0 = ranges::iter_move(it0);
  return true;
}

void
test02()
{
  int x[] = {1, 2, 3, 4, 5};
  __gnu_test::test_input_range rx(x);
  auto v = views::concat(views::single(0), rx, views::empty<int>);
  static_assert(!ranges::forward_range<decltype(v)>);
  VERIFY( ranges::equal(v | views::drop(1), x) );
}

void
test03()
{
  // LWG 4166 - concat_view::end() should be more constrained in order to
  // support noncopyable iterators
  auto range_copyable_it = std::vector<int>{1, 2, 3};

  std::stringstream ss{"4 5 6"};
  auto range_noncopyable_it = views::istream<int>(ss);
  ranges::range auto view1 = views::concat(range_copyable_it, range_noncopyable_it);
  VERIFY( ranges::equal(view1, std::vector{1, 2, 3, 4, 5, 6}) );

  ss = std::stringstream{"4 5 6"};
  range_noncopyable_it = views::istream<int>(ss);
  ranges::range auto view2 = views::concat(range_noncopyable_it, range_copyable_it);
  VERIFY( ranges::equal(view2, std::vector{4, 5, 6, 1, 2, 3}) );
}

void
test04()
{
  // PR libstdc++/115215 - views::concat rejects non-movable reference
  int x[] = {1,2,3};
  struct nomove {
    nomove() = default;
    nomove(const nomove&) = delete;
  };
  auto v = x | views::transform([](int) { return nomove{}; });
  using type = decltype(views::concat(v));
  using type = decltype(v);
}

void
test05()
{
  // PR libstdc++/120934 - views::concat is ill-formed depending on argument order
  auto v1 = views::single(1);
  std::vector<int> vec = {2, 3};
  auto v2 = views::join(views::transform(vec, views::single));

  static_assert( ranges::range<decltype(views::concat(v1, v2))> );
  static_assert( ranges::range<decltype(views::concat(v2, v1))> );
}

int
main()
{
  static_assert(test01());
  test01();
  test02();
  test03();
  test04();
  test05();
}
