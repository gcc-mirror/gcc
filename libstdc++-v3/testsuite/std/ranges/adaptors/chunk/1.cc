// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3, 4, 5};

  auto v2 = x | views::chunk(2);
  const auto i0 = v2.begin(), i1 = v2.begin() + 1;
  VERIFY( i0 + 1 - 1 == i0 );
  VERIFY( i0 < i1 );
  VERIFY( i1 < v2.end() );
  VERIFY( i1 - i0 == 1 );
  VERIFY( i0 - i1 == -1 );
  VERIFY( v2.end() - i1 == 2 );
  VERIFY( i1 - v2.end() == -2 );
  auto i2 = v2.begin();
  i2 += 2;
  i2 -= -1;
  VERIFY( i2 == v2.end() );
  VERIFY( ranges::size(v2) == 3 );
  VERIFY( ranges::equal(v2, (std::initializer_list<int>[]){{1, 2}, {3, 4}, {5}},
			ranges::equal) );

  auto v1 = x | views::chunk(1);
  VERIFY( ranges::size(v1) == ranges::size(x) );
  for (auto [r, n] : views::zip(v1, x))
    {
      VERIFY( ranges::size(r) == 1 );
      VERIFY( *r.begin() == n );
    }

  auto v5 = x | views::chunk(5);
  VERIFY( ranges::size(v5) == 1 );
  VERIFY( ranges::equal(v5[0], (int[]){1, 2, 3, 4, 5}) );

  auto v10 = x | views::chunk(10);
  VERIFY( ranges::size(v10) == 1 );
  VERIFY( ranges::equal(v10[0], (int[]){1, 2, 3, 4, 5}) );

  return true;
}

template<class wrapper>
void
test02()
{
  int x[] = {1, 2, 3, 4, 5, 6, 7, 8};
  wrapper rx(x);
  auto v = rx | views::chunk(3);
  auto i = ranges::begin(v);
  VERIFY( ranges::equal(*i, (int[]){1, 2, 3}) );
  ++i;
  VERIFY( ranges::equal(*i, (int[]){4, 5, 6}) );
  ++i;
  VERIFY( ranges::equal(*i, (int[]){7, 8}) );
  i++;
  VERIFY( i == ranges::end(v) );

  for (int i = 1; i <= 10; ++i)
    VERIFY( ranges::equal(wrapper(x) | views::chunk(i) | views::join, x) );
}

int
main()
{
  static_assert(test01());
  test02<__gnu_test::test_input_range<int>>();
  test02<__gnu_test::test_forward_range<int>>();
}
