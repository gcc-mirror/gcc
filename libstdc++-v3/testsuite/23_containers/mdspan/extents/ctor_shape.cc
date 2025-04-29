// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

constexpr auto dyn = std::dynamic_extent;

template<typename Extent, typename T, size_t N>
  constexpr bool
  constructible()
  {
    return std::is_nothrow_constructible_v<Extent, std::array<T, N>>
	   && std::is_nothrow_constructible_v<Extent, std::span<T, N>>;
  }

template<typename Extent, typename T, size_t N>
  constexpr bool
  not_constructible()
  {
    return !std::is_constructible_v<Extent, std::array<T, N>>
	   && !std::is_constructible_v<Extent, std::span<T, N>>;
  }

template<typename Extent, typename T, size_t N>
  constexpr bool
  convertible()
  {
    return std::is_convertible_v<std::array<T, N>, Extent>
	   && std::is_convertible_v<std::span<T, N>, Extent>;
  }

template<typename Extent, typename T, size_t N>
  constexpr bool
  not_convertible()
  {
    return !std::is_convertible_v<std::array<T, N>, Extent>
	   && !std::is_convertible_v<std::span<T, N>, Extent>;
  }

static_assert(constructible<std::extents<int, 1, 2>, int, 2>());
static_assert(not_constructible<std::extents<int, 1, 2>, int, 1>());

static_assert(constructible<std::extents<int>, int, 0>());
static_assert(convertible<std::extents<int>, int, 0>());
static_assert(convertible<std::extents<unsigned int>, int, 0>());
static_assert(convertible<std::extents<int>, unsigned int, 0>());

static_assert(constructible<std::extents<int, 1, dyn>, int, 1>());
static_assert(convertible<std::extents<int, 1, dyn>, int, 1>());
static_assert(convertible<std::extents<unsigned int, 1, dyn>, int, 1>());
static_assert(convertible<std::extents<int, 1, dyn>, unsigned int, 1>());

static_assert(constructible<std::extents<int, 1, dyn>, int, 2>());
static_assert(not_convertible<std::extents<int, 1, dyn>, int, 2>());
static_assert(not_convertible<std::extents<unsigned int, 1, dyn>, int, 2>());
static_assert(not_convertible<std::extents<int, 1, dyn>, unsigned int, 2>());

// Non-integer, but convertible.
static_assert(constructible<std::extents<int, dyn>, double, 1>());
static_assert(convertible<std::extents<int, dyn>, double, 1>());

namespace all_extents
{
  template<typename Shape>
    constexpr void
    test_ctor(Shape shape)
    {
      auto expected = std::extents<int, 1, 2, 3>();
      VERIFY((std::extents<int, 1, dyn, 3>(shape)) == expected);
      VERIFY((std::extents<int, dyn, dyn, dyn>(shape)) == expected);
      VERIFY((std::extents<int, 1, 2, 3>(shape)) == expected);
    }

  constexpr void
  test_common_shapes()
  {
    auto array = std::array<int, 3>{1, 2, 3};
    auto span_const = std::span<const int, 3>(array);
    auto span = std::span<int, 3>(array);

    test_ctor(array);
    test_ctor(span);
    test_ctor(span_const);
  }

  constexpr void
  test_empty_shapes()
  {
    auto shape = std::array<int, 0>();
    auto span = std::span<int, 0>(shape);

    auto expected = std::extents<int>();
    VERIFY((std::extents<int>(shape)) == expected);
    VERIFY((std::extents<int>(span)) == expected);
  }

  constexpr bool
  test_all()
  {
    test_common_shapes();
    test_empty_shapes();
    return true;
  }
}

namespace only_dynamic_extents
{
  template<typename Extents, typename Shape>
    constexpr void
    test_ctor(const Shape& shape)
    {
      Extents e = shape;

      VERIFY(e.rank_dynamic() == shape.size());

      size_t di = 0;
      for(size_t i = 0; i < e.rank(); ++i)
	if(e.static_extent(i) == dyn)
	  VERIFY(e.extent(i) == shape[di++]);
    }

  template<typename Extents, typename T, size_t N>
    constexpr void
    test_all_shape_types(std::array<T, N> shape)
    {
      test_ctor<Extents>(shape);
      test_ctor<Extents>(std::span<T, N>(shape));
      test_ctor<Extents>(std::span<const T, N>(shape));
    }

  constexpr void
  test_common_shapes()
  {
    auto s = std::array<int, 0>{};
    auto s2 = std::array<int, 1>{2};
    auto s123 = std::array<int, 3>{1, 2, 3};

    test_all_shape_types<std::extents<int, 1, dyn, 3>>(s2);
    test_all_shape_types<std::extents<int, dyn, dyn, dyn>>(s123);
    test_all_shape_types<std::extents<int, 1, 2, 3>>(s);
  }

  constexpr bool
  test_all()
  {
    test_common_shapes();
    return true;
  }
}

int
main()
{
  all_extents::test_all();
  static_assert(all_extents::test_all());

  only_dynamic_extents::test_all();
  static_assert(only_dynamic_extents::test_all());
  return 0;
}
