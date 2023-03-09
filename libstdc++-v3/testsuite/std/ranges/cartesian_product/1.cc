// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <sstream>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

#if __cpp_lib_ranges_cartesian_product != 202207L
# error "Feature-test macro __cpp_lib_ranges_cartesian_product has wrong value in <ranges>"
#endif

namespace ranges = std::ranges;
namespace views = std::views;

constexpr bool
test01()
{
  int x[] = {1, 2, 3};
  int y[] = {4, 5, 6};
  int z[] = {7, 8};
  int w[] = {9};

  auto v0 = views::cartesian_product();
  VERIFY( ranges::end(v0) - ranges::begin(v0) == 0 );
  VERIFY( ranges::size(v0) == 0 );
  VERIFY( ranges::empty(v0) );

  auto v1 = views::cartesian_product(x);
  VERIFY( ranges::end(v1) - ranges::begin(v1) == 3 );
  VERIFY( ranges::size(v1) == 3 );
  VERIFY( ranges::equal(v1 | views::keys, x) );
  VERIFY( std::get<0>(v1[0]) == 1 );
  VERIFY( std::get<0>(v1[1]) == 2 );
  VERIFY( std::get<0>(v1[2]) == 3 );
  VERIFY( ranges::equal(v1 | views::reverse | views::keys, x | views::reverse));

  auto v2 = views::cartesian_product(x, y);
  VERIFY( ranges::size(v2) == 9 );
  VERIFY( ranges::end(v2) - ranges::begin(v2) == 9 );
  VERIFY( ranges::equal(v2 | views::keys,   (int[]){1, 1, 1, 2, 2, 2, 3, 3, 3}));
  VERIFY( ranges::equal(v2 | views::values, (int[]){4, 5, 6, 4, 5, 6, 4, 5, 6}));
  VERIFY( ranges::equal(v2 | views::reverse | views::keys,   (int[]){3, 3, 3, 2, 2, 2, 1, 1, 1}) );
  VERIFY( ranges::equal(v2 | views::reverse | views::values, (int[]){6, 5, 4, 6, 5, 4, 6, 5, 4}) );

  auto v3 = views::cartesian_product(x, y, z);
  VERIFY( ranges::size(v3) == 18 );
  VERIFY( ranges::equal(v3, (std::tuple<int,int,int>[]){{1,4,7}, {1,4,8}, {1,5,7}, {1,5,8},
							{1,6,7}, {1,6,8}, {2,4,7}, {2,4,8},
							{2,5,7}, {2,5,8}, {2,6,7}, {2,6,8},
							{3,4,7}, {3,4,8}, {3,5,7}, {3,5,8},
							{3,6,7}, {3,6,8}}) );

  auto v4 = views::cartesian_product(x, y, z, w);
  VERIFY( ranges::size(v4) == 18 );
  VERIFY( ranges::equal(v4 | views::elements<3>, views::repeat(9, 18)) );

  auto i4 = v4.begin(), j4 = i4 + 1;
  VERIFY( j4 > i4 );
  VERIFY( i4[0] == std::tuple(1, 4, 7, 9) );
  VERIFY( i4 + 18 == v4.end() );
  i4 += 5;
  VERIFY( i4 != v4.begin() );
  VERIFY( i4 - 5 == v4.begin() );
  VERIFY( *i4 == std::tuple(1, 6, 8, 9) );
  VERIFY( i4 - 5 != i4 );
  i4 -= 3;
  VERIFY( *i4 == std::tuple(1, 5, 7, 9) );
  VERIFY( j4 + 1 == i4 );
  ranges::iter_swap(i4, j4);
  VERIFY( *j4 == std::tuple(1, 5, 7, 9) );
  VERIFY( *i4 == std::tuple(1, 4, 8, 9) );

  return true;
}

void
test02()
{
  int x[] = {1, 2};
  __gnu_test::test_input_range<int> rx(x);
  auto v = views::cartesian_product(rx, x);
  auto i = v.begin();
  std::default_sentinel_t s = v.end();
  VERIFY( i != s );
  VERIFY( std::get<0>(*i) == 1 && std::get<1>(*i) == 1 );
  ++i;
  VERIFY( i != s );
  VERIFY( std::get<0>(*i) == 1 && std::get<1>(*i) == 2 );
  ++i;
  VERIFY( i != s );
  VERIFY( std::get<0>(*i) == 2 && std::get<1>(*i) == 1 );
  ++i;
  VERIFY( i != s );
  VERIFY( std::get<0>(*i) == 2 && std::get<1>(*i) == 2 );
  ++i;
  VERIFY( i == s );
}

void
test03()
{
  int x[2];
  __gnu_test::test_input_range<int> rx(x);
  auto v = views::cartesian_product(views::counted(rx.begin(), 2), x);
  VERIFY( v.size() == 4 );
  auto i = v.begin();
  std::default_sentinel_t s = v.end();
  VERIFY( i - s == -4 );
  VERIFY( s - i == 4 );
  ++i;
  VERIFY( i - s == -3 );
  VERIFY( s - i == 3 );
  ++i;
  VERIFY( i - s == -2 );
  VERIFY( s - i == 2 );
  ++i;
  VERIFY( i - s == -1 );
  VERIFY( s - i == 1 );
  ++i;
  VERIFY( i - s == 0 );
  VERIFY( s - i == 0 );
}

void
test04()
{
  // Exhaustively verify correctness of our iterator addition implementation
  // (which runs in constant time) for this 24-element cartesian_product_view.
  int x[4], y[3], z[2], w[1];
  auto v = views::cartesian_product(x, y, z, w);

  auto n = ranges::ssize(v);
  for (int i = 0; i <= n; i++)
    for (int j = 0; i + j <= n; j++)
      {
	auto b1 = v.begin();
	for (int k = 0; k < i + j; k++)
	  ++b1;
	VERIFY( b1 - v.begin() == i + j );
	auto b2 = (v.begin() + i) + j;
	auto b3 = v.begin() + (i + j);
	VERIFY( b1 == b2 && b2 == b3 );

	auto e1 = v.end();
	for (int k = 0; k < i + j; k++)
	  --e1;
	VERIFY( v.end() - e1 == i + j );
	auto e2 = (v.end() - i) - j;
	auto e3 = v.end() - (i + j);
	VERIFY( e1 == e2 && e2 == e3 );
      }
}

void
test05()
{
#if __SIZEOF_INT128__
  auto r = views::iota(__int128(0), __int128(5));
#else
  auto r = views::iota(0ll, 5ll);
#endif
  auto v = views::cartesian_product(r, r);
  VERIFY( ranges::size(v) == 25 );
  VERIFY( v.end() - v.begin() == 25 );
  VERIFY( v.begin() + ranges::ssize(v) - v.begin() == 25 );
}

constexpr bool
test06()
{
  int x[] = {1, 2, 3};
  auto v = views::cartesian_product(x, views::empty<int>, x);
  VERIFY( ranges::size(v) == 0 );
  VERIFY( ranges::begin(v) == ranges::end(v) );
  VERIFY( ranges::begin(v) - ranges::begin(v) == 0 );

  return true;
}

void
test07()
{
  // PR libstdc++/107572
  static std::istringstream ints("0 1 2 3 4");
  struct istream_range {
    auto begin() { return std::istream_iterator<int>{ints}; }
    auto end() { return std::istream_iterator<int>{}; }
    using iterator_concept = std::input_iterator_tag;
  };
  static_assert(!ranges::forward_range<istream_range>
		&& ranges::common_range<istream_range>);
  istream_range r;
  int i = 0;
  for (auto [v] : views::cartesian_product(r))
    {
      VERIFY( v == i );
      ++i;
    };
  VERIFY( i == 5 );
}

void
test08()
{
  // LWG 3820
  auto r = views::cartesian_product(views::iota(0));
  r.begin() += 3;
}

int
main()
{
  static_assert(test01());
  test02();
  test03();
  test04();
  test05();
  static_assert(test06());
  test07();
  test08();
}
