// { dg-do run { target c++26 } }

#include <ranges>

#if __cpp_lib_ranges_cache_latest != 202411L
# error "Feature-test macro __cpp_lib_ranges_cache_latest has wrong value in <ranges>"
#endif

#include <algorithm>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int xs[] = {1,2,3,4,5};
  auto v = xs | views::cache_latest;
  VERIFY( ranges::equal(v, xs) );
  VERIFY( ranges::size(v) == 5 );

  auto it = v.begin();
  auto st = v.end();
  VERIFY( st - it == 5 );
  VERIFY( it - st == -5 );
  it++;
  VERIFY( st - it == 4 );
  VERIFY( it - st == -4 );

  auto jt = v.begin();
  ranges::iter_swap(it, jt);
  VERIFY( ranges::equal(xs, (int[]){2,1,3,4,5}) );
  int n = ranges::iter_move(it);
  VERIFY( n == 1 );
  ranges::iter_swap(it, jt);

  auto w = views::iota(1, 6) | views::cache_latest;
  VERIFY( ranges::equal(w, xs) );

  return true;
}

constexpr bool
test02()
{
  // Motivating example from P3138R5
  int xs[] = {1, 2, 3, 4, 5};
  int transform_count = 0;
  auto v = xs | views::transform([&](int i){ ++transform_count; return i * i; })
	      | views::filter([](int i){ return i % 2 == 0; });
  VERIFY( ranges::equal(v, (int[]){4, 16}) );
  VERIFY( transform_count == 7 );

  transform_count = 0;
  auto w = xs | views::transform([&](int i){ ++transform_count; return i * i; })
	      | views::cache_latest
	      | views::filter([](int i){ return i % 2 == 0; });
  VERIFY( ranges::equal(w, (int[]){4, 16}) );
  VERIFY( transform_count == 5 );

  return true;
}

int
main()
{
  static_assert(test01());
  static_assert(test02());
  test01();
  test02();
}
