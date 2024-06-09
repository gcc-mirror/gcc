// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <algorithm>

#if __cpp_lib_ranges_fold != 202207L
# error "Feature-test macro __cpp_lib_ranges_fold has wrong value in <algorithm>"
#endif

#include <ranges>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3, 4, 5};
  auto f = [](int&& acc, int& x) {
    return 2 * acc + x;
  };
  VERIFY( ranges::fold_left(x, 0, f) == 57 );
  VERIFY( ranges::fold_left(x, 1, f) == 89 );
  VERIFY( ranges::fold_left(x+0, x+0, 1, f) == 1 );

  VERIFY( ranges::fold_left_first(x, f).value() == 57 );
  VERIFY( !ranges::fold_left_first(x+0, x+0, f).has_value() );

  return true;
}

void
test02()
{
  int x[] = {1, 2, 3, 4, 5};
  auto f = [](int&& acc, int& x) {
    return 2 * acc + x;
  };

  __gnu_test::test_input_range<int> rx(x);
  ranges::in_value_result ivr = ranges::fold_left_with_iter(rx, 0, f);
  VERIFY( ivr.in == rx.end() );
  VERIFY( ivr.value == 57 );

  rx.bounds.first = x;
  ranges::in_value_result ivr2 = ranges::fold_left_first_with_iter(rx, f);
  VERIFY( ivr2.in == rx.end() );
  VERIFY( ivr2.value.value() == 57 );

  rx.bounds.first = x;
  auto v = rx | views::take(0);
  ranges::in_value_result ivr3 = ranges::fold_left_first_with_iter(v, f);
  VERIFY( ivr3.in == v.end() );
  VERIFY( !ivr3.value.has_value() );
}

constexpr bool
test03()
{
  double x[] = {0.5, 0.25, 0.125, 0.125};
  VERIFY( ranges::fold_left(x, 0, std::plus{}) == 1.0 );
  VERIFY( ranges::fold_left_with_iter(x, 0, std::plus{}).value == 1.0 );

  return true;
}

int
main()
{
  static_assert(test01());
  test02();
  static_assert(test03());
}
