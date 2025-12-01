// { dg-do run { target c++26 } }
#include <mdspan>

#include "../int_like.h"
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;
constexpr auto all = std::full_extent;

constexpr bool
test_from_full_extent()
{
  auto exts = std::extents<int, 3, dyn, 7>{};
  auto sub_exts = submdspan_extents(exts, 1, all, all);
  VERIFY(sub_exts.rank() == 2);
  VERIFY(sub_exts.static_extent(0) == dyn);
  VERIFY(sub_exts.extent(0) == exts.extent(1));
  VERIFY(std::cmp_equal(sub_exts.static_extent(1), exts.extent(2)));
  return true;
}

template<template<typename, typename> typename Pair, template<int> typename Cw>
  constexpr bool
  test_from_tuple()
  {
    auto exts = std::extents<int, 3, 5, 7>{};
    auto s0 = Cw<1>{};
    auto s1 = Pair{Cw<1>{}, Cw<2>{}};
    auto s2 = Pair{Cw<1>{}, 4};
    auto sub_exts = submdspan_extents(exts, s0, s1, s2);
    VERIFY(sub_exts.rank() == 2);
    VERIFY(sub_exts.static_extent(0) == size_t(get<1>(s1) - get<0>(s1)));
    VERIFY(sub_exts.static_extent(1) == dyn);
    VERIFY(std::cmp_equal(sub_exts.extent(1), get<1>(s2) - get<0>(s2)));
    return true;
  }

template<template<int> typename Cw>
  constexpr bool
  test_from_tuple_all()
  {
    test_from_tuple<std::tuple, Cw>();
    test_from_tuple<std::pair, Cw>();
    return true;
  }


template<typename Int>
  void
  test_from_int_like_as_scalar()
  {
    auto exts = std::extents<int, 3, 5>{};
    auto sub_exts = submdspan_extents(exts, Int(1), std::tuple{1, 3});
    VERIFY(sub_exts.rank() == 1);
    VERIFY(sub_exts.static_extent(0) == dyn);
    VERIFY(sub_exts.extent(0) == 2);
  }

template<template<int> typename Cw>
  constexpr bool
  test_from_const_int()
  {
    auto exts = std::extents<int, 3, 5>{};
    auto sub_exts = submdspan_extents(exts, Cw<1>{}, std::tuple{1, 3});
    VERIFY(sub_exts.rank() == 1);
    VERIFY(sub_exts.static_extent(0) == dyn);
    VERIFY(sub_exts.extent(0) == 2);
    return true;
  }

template<typename Int>
  constexpr bool
  test_from_int_like_in_tuple()
  {
    auto exts = std::extents<int, 3, 5>{};
    auto sub_exts = submdspan_extents(exts, Int(1), std::tuple{Int(1), Int(3)});
    VERIFY(sub_exts.rank() == 1);
    VERIFY(sub_exts.static_extent(0) == dyn);
    VERIFY(sub_exts.extent(0) == 2);
    return true;
  }

template<template<int> typename Cw>
  constexpr bool
  test_from_strided_slice()
  {
    auto exts = std::extents<int, 5, 7, 11>{};
    {
      auto s0 = 1;
      auto s1 = std::strided_slice{0, 0, 0};
      auto s2 = std::strided_slice{1, Cw<0>{}, 0};
      auto sub_exts = submdspan_extents(exts, s0, s1, s2);
      VERIFY(sub_exts.rank() == 2);
      VERIFY(sub_exts.static_extent(0) == dyn);
      VERIFY(sub_exts.extent(0) == 0);
      VERIFY(sub_exts.static_extent(1) == 0);
    }

    {
      auto s0 = 1;
      auto s1 = std::strided_slice{0, 2, Cw<1>{}};
      auto s2 = std::strided_slice{1, Cw<2>{}, 1};
      auto sub_exts = submdspan_extents(exts, s0, s1, s2);
      VERIFY(sub_exts.rank() == 2);
      VERIFY(sub_exts.static_extent(0) == dyn);
      VERIFY(sub_exts.extent(0) == 2);
      VERIFY(sub_exts.static_extent(1) == dyn);
      VERIFY(sub_exts.extent(1) == 2);
    }

    {
      // selected = 1 x [1, 3] x [1, 4, 7, 10]
      auto s0 = 1;
      auto s1 = std::strided_slice{1, Cw<4>{}, 2};
      auto s2 = std::strided_slice{1, Cw<10>{}, Cw<3>{}};
      auto sub_exts = submdspan_extents(exts, s0, s1, s2);
      VERIFY(sub_exts.rank() == 2);
      VERIFY(sub_exts.static_extent(0) == dyn);
      VERIFY(sub_exts.extent(0) == 2);
      VERIFY(sub_exts.static_extent(1) == 4);
    }

    {
      // selected = [0, 2] x [1, 3] x [0, 3, 6]
      auto s0 = std::strided_slice(0, 3, 2);
      auto s1 = std::strided_slice(1, 4, 2);
      auto s2 = std::strided_slice(0, 7, 3);
      auto sub_exts = submdspan_extents(exts, s0, s1, s2);
      VERIFY(sub_exts.rank() == 3);
      VERIFY(sub_exts.extent(0) == 2);
      VERIFY(sub_exts.extent(1) == 2);
      VERIFY(sub_exts.extent(2) == 3);
    }
    return true;
  }

template<int Value>
  using CW = std::constant_wrapper<Value, int>;

template<int Value>
  using IC = std::integral_constant<int, Value>;

constexpr bool
test_all()
{
  test_from_full_extent();
  test_from_tuple_all<CW>();
  test_from_tuple_all<IC>();
  test_from_const_int<CW>();
  test_from_const_int<IC>();
  test_from_strided_slice<CW>();
  test_from_strided_slice<IC>();
  test_from_int_like_in_tuple<StructuralInt>();
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());

  test_from_int_like_as_scalar<CustomIndexType<CustomIndexKind::Const, true>>();
  test_from_int_like_as_scalar<CustomIndexType<CustomIndexKind::Throwing, true>>();
  test_from_int_like_as_scalar<CustomIndexType<CustomIndexKind::Mutating, true>>();
  test_from_int_like_as_scalar<CustomIndexType<CustomIndexKind::RValue, true>>();
  test_from_int_like_as_scalar<StructuralInt>();
  return 0;
}
