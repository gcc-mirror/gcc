// { dg-do run { target c++23 } }
// { dg-add-options no_pch }

#include <ranges>

#if __cpp_lib_ranges_enumerate != 202302L
# error "Feature-test macro __cpp_lib_ranges_enumerate has wrong value in <ranges>"
#endif

#include <algorithm>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

namespace ranges = std::ranges;
namespace views = std::views;

using __gnu_test::test_input_range;
using __gnu_test::test_forward_range;
using __gnu_test::test_bidirectional_range;
using __gnu_test::test_random_access_range;

constexpr bool
test01()
{
  int x[] = {1, 2, 3};
  auto v = x | views::enumerate;

  VERIFY( ranges::equal(v | views::keys, (int[]){0, 1, 2}) );
  VERIFY( ranges::equal(v | views::values, (int[]){1, 2, 3}) );

  auto it = v.begin();
  VERIFY( it == it );
  VERIFY( it != it + 1 );
  VERIFY( it != v.end() );

  VERIFY( it.index() == 0 );
  VERIFY( (++it).index() == 1 );
  VERIFY( (++it).index() == 2 );

  return true;
}

template<template<class> class Container>
void
test02()
{
  int x[] = {1, 2, 3};
  Container<int> rx (x);
  auto v = rx | views::enumerate;

  int j = 0;
  for (auto [i, y] : v)
    {
      VERIFY (&y == &x[j]);
      VERIFY (j == i);
      ++j;
    }
  VERIFY (j == ranges::size(x));

  if constexpr (ranges::bidirectional_range<decltype(rx)>)
    {
      static_assert(ranges::bidirectional_range<decltype(v)>);
      for (auto [i, y] : v | views::reverse)
	{
	  --j;
	  VERIFY (&y == &x[j]);
	  VERIFY (j == i);
	}
      VERIFY (j == 0);
    }

  if constexpr (ranges::random_access_range<decltype(rx)>)
    {
      static_assert(ranges::random_access_range<decltype(v)>);
      for (j = 0; j < ranges::ssize(x); ++j)
	{
	  VERIFY (std::get<0>(v[j]) == j);
	  VERIFY (&std::get<1>(v[j]) == &x[j]);
	  VERIFY (*(v.begin() + j) == v[j]);
	  VERIFY (*(v.begin() + (ranges::size(x) - 1) - j) == v[ranges::size(x) - 1 - j]);
	  VERIFY (v.begin() + j + 1 > v.begin() + j );
	  VERIFY (v.begin() + j < v.begin() + j + 1 );
	  VERIFY (v.begin() + j >= v.begin() );
	  VERIFY (v.begin() <= v.begin() + j );
	  VERIFY( v.begin() + j != v.end() );
	  VERIFY( v.begin() + j - v.begin() == j );
	  VERIFY( v.end() - (v.begin() + j) == ranges::ssize(x) - j );
	}
      VERIFY( v.begin() + j == v.end() );
    }
}

int
main()
{
  static_assert(test01());

  test02<test_input_range>();
  test02<test_forward_range>();
  test02<test_bidirectional_range>();
  test02<test_random_access_range>();
}
