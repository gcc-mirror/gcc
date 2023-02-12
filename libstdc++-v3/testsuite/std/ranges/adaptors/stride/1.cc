// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __cpp_lib_ranges_stride != 202207L
# error "Feature-test macro __cpp_lib_ranges_stride has wrong value in <ranges>"
#endif

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3, 4, 5, 6, 7};

  auto v2 = x | views::stride(2);
  const auto i0 = v2.begin(), i1 = v2.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 != i1 );
  VERIFY( i0 < i1 );
  VERIFY( i0 <= i0 );
  VERIFY( i0 >= i0 );
  VERIFY( v2.end() > i1 );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( v2.end() - i1 == 3 );
  VERIFY( i1 - v2.end() == -3 );
  auto i2 = v2.begin();
  i2 += 2;
  i2 -= -2;
  VERIFY( i2 == v2.end() );
  VERIFY( ranges::size(v2) == 4 );
  VERIFY( ranges::equal(v2, (int[]){1, 3, 5, 7}) );
  VERIFY( ranges::equal(v2 | views::reverse, (int[]){7, 5, 3, 1}) );
  VERIFY( v2.stride() == 2 );

  auto v1 = x | views::stride(1);
  VERIFY( ranges::size(v1) == ranges::size(x) );
  VERIFY( ranges::equal(v1, x) );
  VERIFY( ranges::equal(v1 | views::reverse, x | views::reverse) );
  VERIFY( v1.stride() == 1 );

  auto v5 = x | views::stride(5);
  VERIFY( ranges::equal(v5, (int[]){1, 6}) );
  VERIFY( ranges::equal(v5 | views::reverse, (int[]){6, 1}) );
  VERIFY( v5.stride() == 5 );

  auto v10 = x | views::stride(10);
  VERIFY( ranges::equal(v10, (int[]){1}) );
  VERIFY( ranges::equal(v10 | views::reverse, (int[]){1}) );
  VERIFY( v10.stride() == 10 );

  return true;
}

template<class container>
void
test02()
{
  int x[] = {1, 2, 3, 4, 5, 6, 7, 8};
  container rx(x);
  auto v = rx | views::stride(3);
  VERIFY( ranges::equal(v, (int[]){1, 4, 7}) );
}

void
test03()
{
  // PR libstdc++/107313
  int x[] = {1, 2, 3, 4, 5};
  __gnu_test::test_input_range<int> rx(x);
  auto r = views::counted(rx.begin(), 4) | views::stride(2);
  auto i = r.begin();
  std::default_sentinel_t s = r.end();
  VERIFY( s != i );
  VERIFY( s - i == 2 && i - s == -2 );
  ++i;
  VERIFY( s != i );
  VERIFY( s - i == 1 && i - s == -1 );
  ++i;
  VERIFY( s == i );
  VERIFY( s - i == 0 && i - s == 0 );
}

int
main()
{
  static_assert(test01());
  test02<__gnu_test::test_input_range<int>>();
  test02<__gnu_test::test_forward_range<int>>();
  test03();
}
