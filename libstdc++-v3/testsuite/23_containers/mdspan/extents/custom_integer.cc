// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>
#include "../int_like.h"

// Test construction from a custom integer-like object, that has
// no copy/move ctor or copy/move assignment operator.

constexpr size_t dyn = std::dynamic_extent;

static_assert(std::is_convertible_v<IntLike, int>);
static_assert(std::is_nothrow_constructible_v<int, IntLike>);

template<typename Extents, bool Valid>
  void
  test_shape(const auto& shape)
  {
    static_assert(std::is_constructible_v<Extents, decltype(shape)> == Valid);

    if constexpr (Valid)
      {
	std::extents<int, 2, 3> expected;
	Extents actual(shape);
	VERIFY(actual == expected);
      }
  }

template<typename Int, bool Valid>
  void
  test_shape_all()
  {
    auto a2 = std::array<Int, 1>{Int(2)};
    auto s2 = std::span<Int, 1>(a2);

    auto a23 = std::array<Int, 2>{Int(2), Int(3)};
    auto s23 = std::span<Int, 2>(a23);

    auto check = [](const auto& dyn_exts, const auto& full_exts)
      {
	test_shape<std::extents<int, 2, 3>, Valid>(full_exts);
	test_shape<std::extents<int, dyn, 3>, Valid>(dyn_exts);
	test_shape<std::extents<int, dyn, 3>, Valid>(full_exts);
	test_shape<std::extents<int, dyn, dyn>, Valid>(full_exts);
      };

    check(a2, a23);
    check(s2, s23);
  }

// Needed because is_constructible requires that Ints are move constructible.
template<typename Extents, typename... Ints>
  concept has_ctor = requires
  {
    { Extents(Ints(0)...) } -> std::same_as<Extents>;
  };

template<typename Int, bool Valid>
  void
  test_pack_all()
  {
    static_assert(has_ctor<std::extents<int, dyn, 3>, Int> == Valid);
    static_assert(has_ctor<std::extents<int, dyn, 3>, Int, Int> == Valid);
    static_assert(has_ctor<std::extents<int, dyn, dyn>, Int, Int> == Valid);

    if constexpr (Valid)
      {
	std::extents<int, 2, 3> expected;

	std::extents<int, dyn, 3> e1(Int(2));
	VERIFY(e1 == expected);

	std::extents<int, dyn, 3> e2(Int(2), Int(3));
	VERIFY(e2 == expected);

	std::extents<int, dyn, dyn> e3(Int(2), Int(3));
	VERIFY(e3 == expected);
      }
  }

int
main()
{
  test_shape_all<int, true>();
  test_shape_all<IntLike, true>();
  test_shape_all<ThrowingInt, false>();
  test_shape_all<MutatingInt, false>();
  test_shape_all<RValueInt, false>();

  test_pack_all<int, true>();
  test_pack_all<IntLike, true>();
  test_pack_all<ThrowingInt, false>();
  test_pack_all<MutatingInt, true>();
  test_pack_all<RValueInt, true>();
  return 0;
}
