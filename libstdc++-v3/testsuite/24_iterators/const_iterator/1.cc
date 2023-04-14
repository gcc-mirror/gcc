// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <iterator>
#include <array>
#include <concepts>
#include <string_view>
#include <vector>
#include <testsuite_iterators.h>

using __gnu_test::test_input_range;
using __gnu_test::test_forward_range;
using __gnu_test::test_bidirectional_range;
using __gnu_test::test_random_access_range;

namespace ranges = std::ranges;

template<class Iter, bool Const>
void
test01()
{
  if constexpr (Const)
    {
      static_assert( std::same_as<std::const_iterator<Iter>, Iter> );
      static_assert( std::same_as<std::const_sentinel<Iter>, Iter> );
      static_assert( std::same_as<std::iter_const_reference_t<Iter>,
				 std::iter_reference_t<Iter>> );
    }
  else
    {
      using Wrapped = std::basic_const_iterator<Iter>;

      static_assert( std::same_as<std::const_iterator<Iter>, Wrapped> );
      static_assert( std::same_as<std::const_sentinel<Iter>, Wrapped> );
      static_assert( std::same_as<std::iter_const_reference_t<Iter>,
				 std::iter_reference_t<Wrapped>> );

      static_assert( std::input_iterator<Iter> == std::input_iterator<Wrapped> );
      static_assert( std::forward_iterator<Iter> == std::forward_iterator<Wrapped> );
      static_assert( std::bidirectional_iterator<Iter> == std::bidirectional_iterator<Wrapped> );
      static_assert( std::random_access_iterator<Iter> == std::random_access_iterator<Wrapped> );
    }
}

template<class Range, bool Const>
void
test02()
{
  if constexpr (Const)
    {
      static_assert( ranges::constant_range<Range> );
      static_assert( std::same_as<ranges::const_iterator_t<Range>, ranges::iterator_t<Range>> );
      static_assert( std::same_as<ranges::const_sentinel_t<Range>, ranges::sentinel_t<Range>> );
      static_assert( std::same_as<ranges::range_const_reference_t<Range>,
				 ranges::range_reference_t<Range>> );

      static_assert( std::same_as<decltype(ranges::cbegin(std::declval<Range&>())),
				 decltype(ranges::begin(std::declval<Range&>()))> );
      static_assert( std::same_as<decltype(ranges::cend(std::declval<Range&>())),
				 decltype(ranges::end(std::declval<Range&>()))> );
    }
  else
    {
      static_assert( !ranges::constant_range<Range> );
      using Wrapped = std::basic_const_iterator<ranges::iterator_t<Range>>;

      static_assert( std::same_as<ranges::const_iterator_t<Range>, Wrapped> );
      if constexpr (ranges::common_range<Range>)
	static_assert( std::same_as<ranges::const_sentinel_t<Range>, Wrapped> );
      static_assert( std::same_as<ranges::range_const_reference_t<Range>,
				 std::iter_reference_t<Wrapped>> );

      static_assert( ranges::input_range<Range> == std::input_iterator<Wrapped> );
      static_assert( ranges::forward_range<Range> == std::forward_iterator<Wrapped> );
      static_assert( ranges::bidirectional_range<Range> == std::bidirectional_iterator<Wrapped> );
      static_assert( ranges::random_access_range<Range> == std::random_access_iterator<Wrapped> );

      if constexpr (ranges::constant_range<const Range&>)
	{
	  static_assert( std::same_as<decltype(ranges::cbegin(std::declval<Range&>())),
				     decltype(ranges::begin(std::declval<const Range&>()))> );
	  static_assert( std::same_as<decltype(ranges::cend(std::declval<Range&>())),
				     decltype(ranges::end(std::declval<const Range&>()))> );
	}
      else
	{
	  static_assert( std::same_as<decltype(ranges::cbegin(std::declval<Range&>())), Wrapped> );
	  if constexpr (ranges::common_range<Range>)
	    static_assert( std::same_as<decltype(ranges::cend(std::declval<Range&>())), Wrapped> );
	}
    }
}

void
test03()
{
  static_assert( std::same_as<std::const_sentinel<std::unreachable_sentinel_t>,
			     std::unreachable_sentinel_t> );
}

int
main()
{
  test01<int*, false>();
  test01<ranges::iterator_t<test_input_range<int>>, false>();
  test01<ranges::iterator_t<test_forward_range<int>>, false>();
  test01<ranges::iterator_t<test_bidirectional_range<int>>, false>();
  test01<ranges::iterator_t<test_random_access_range<int>>, false>();
  test01<std::array<int, 3>::iterator, false>();
  test01<std::vector<bool>::iterator, false>();

  test01<const int*, true>();
  test01<ranges::iterator_t<test_input_range<const int>>, true>();
  test01<ranges::iterator_t<test_forward_range<const int>>, true>();
  test01<ranges::iterator_t<test_bidirectional_range<const int>>, true>();
  test01<ranges::iterator_t<test_random_access_range<const int>>, true>();
  test01<std::array<const int, 3>::iterator, true>();
  test01<std::string_view::iterator, true>();
  test01<std::vector<bool>::const_iterator, true>();

  test02<int[42], false>();
  test02<test_input_range<int>, false>();
  test02<test_forward_range<int>, false>();
  test02<test_bidirectional_range<int>, false>();
  test02<test_random_access_range<int>, false>();
  test02<std::array<int, 3>, false>();
  test02<std::vector<bool>, false>();

  test02<const int[42], true>();
  test02<test_input_range<const int>, true>();
  test02<test_forward_range<const int>, true>();
  test02<test_bidirectional_range<const int>, true>();
  test02<test_random_access_range<const int>, true>();
  test02<std::array<const int, 3>, true>();
  test02<const std::array<int, 3>, true>();
  test02<std::string_view, true>();
  test02<const std::vector<bool>, true>();

  test03();
}
