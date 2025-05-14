// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

constexpr auto dyn = std::dynamic_extent;

class A {};

// Not constructible if the number of integer-like arguments isn't either
// rank() or rank_dynamic().
static_assert(!std::is_constructible_v<std::extents<int>, int>);
static_assert(!std::is_constructible_v<std::extents<int, dyn, dyn>, int>);
static_assert(!std::is_constructible_v<std::extents<int, 1, dyn, 3>, int, int>);

// Not constructible from non integer-like objects.
static_assert(!std::is_constructible_v<std::extents<int, 1>, int, A>);

// No implicit conversion from integer-like objects.
template<typename Extent, typename... OExtents>
  constexpr bool
  is_explicit()
  {
    return std::is_nothrow_constructible_v<Extent, OExtents...>
	   && !std::is_convertible_v<Extent, OExtents...>;
  }

static_assert(is_explicit<std::extents<int, 1>, int>());
static_assert(is_explicit<std::extents<int, 1>, unsigned int>());
static_assert(is_explicit<std::extents<unsigned int, 1>, int>());

constexpr bool
test_all()
{
  auto expected = std::extents<int, 1, 2, 3>(1, 2, 3);

  // From all extents.
  VERIFY((std::extents<int, 1, 2, 3>(1, 2, 3)) == expected);
  VERIFY((std::extents<int, dyn, 2, 3>(1, 2, 3)) == expected);
  VERIFY((std::extents<int, dyn, 2, dyn>(1, 2, 3)) == expected);

  VERIFY((std::extents<int, 1, 2, 3>{1, 2, 3}) == expected);
  VERIFY((std::extents<int, dyn, 2, 3>{1, 2, 3}) == expected);
  VERIFY((std::extents<int, dyn, 2, dyn>{1, 2, 3}) == expected);

  // From only dynamic extents.
  VERIFY((std::extents<int, dyn, 2, 3>(1)) == expected);
  VERIFY((std::extents<int, dyn, 2, dyn>(1, 3)) == expected);

  VERIFY((std::extents<int, dyn, 2, 3>{1}) == expected);
  VERIFY((std::extents<int, dyn, 2, dyn>{1, 3}) == expected);

  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
