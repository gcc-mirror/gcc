// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

// Test the copy ctor and the ctor from other extents.

constexpr auto dyn = std::dynamic_extent;

// Not constructible
static_assert(!std::is_constructible_v<std::extents<int>,
				       std::extents<int, 1>>);

static_assert(!std::is_constructible_v<std::extents<int, 1, 1>,
				       std::extents<int, 1>>);

static_assert(!std::is_constructible_v<std::extents<int, dyn>,
				       std::extents<int, dyn, dyn>>);

static_assert(!std::is_constructible_v<std::extents<int, 2, 2>,
				       std::extents<int, 1, 2>>);

// Nothrow constructible
static_assert(std::is_nothrow_constructible_v<std::extents<int, 1>,
					      std::extents<unsigned int, dyn>>);
static_assert(std::is_nothrow_constructible_v<std::extents<unsigned int, dyn>,
					      std::extents<int, 1>>);

// Implicit conversion
static_assert(!std::is_convertible_v<std::extents<unsigned int>,
				     std::extents<int>>);
static_assert(std::is_convertible_v<std::extents<int>,
				    std::extents<unsigned int>>);

static_assert(!std::is_convertible_v<std::extents<unsigned int, 1>,
				     std::extents<int, 1>>);
static_assert(std::is_convertible_v<std::extents<int, 1>,
				    std::extents<unsigned int, 1>>);

static_assert(!std::is_convertible_v<std::extents<int, dyn>,
				     std::extents<int, 1>>);
static_assert(std::is_convertible_v<std::extents<int, 1>,
				    std::extents<int, dyn>>);

static_assert(!std::is_convertible_v<std::extents<unsigned int, 1>,
				     std::extents<int, dyn>>);
static_assert(std::is_convertible_v<std::extents<int, 1>,
				    std::extents<unsigned int, dyn>>);

template<typename T, size_t... Extents, typename Other>
  constexpr void
  test_ctor(const Other& other)
  {
    auto e = std::extents<T, Extents...>(other);
    VERIFY(e == other);
  }

constexpr int
test_all()
{
  auto e0 = std::extents<int>();
  test_ctor<int>(e0);

  auto e1 = std::extents<int, 1, 2, 3>();
  test_ctor<int, 1, 2, 3>(e1);
  test_ctor<int, 1, dyn, 3>(e1);
  test_ctor<unsigned int, 1, dyn, 3>(e1);

  auto e2 = std::extents<unsigned int, 1, dyn, 3>{1, 2, 3};
  test_ctor<int, 1, 2, 3>(e2);
  test_ctor<int, 1, dyn, 3>(e2);
  test_ctor<int, 1, dyn, dyn>(e2);
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
