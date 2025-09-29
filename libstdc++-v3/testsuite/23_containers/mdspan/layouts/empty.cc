// { dg-do run { target c++23 } }
#include <mdspan>

#include <cstdint>
#include <algorithm>
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<typename Mapping>
  constexpr void
  invoke_stride(Mapping m)
  {
    // Only checking for UB, e.g. signed overflow.
    for(size_t i = 0; i < Mapping::extents_type::rank(); ++i)
      m.stride(i);
  }

template<typename Mapping>
  constexpr void
  verify_required_span_size(Mapping m)
  { VERIFY(m.required_span_size() == 0); }

template<typename Mapping>
  constexpr void
  verify_all(Mapping m)
  {
    verify_required_span_size(m);
    invoke_stride(m);
  }

template<typename Layout, typename Int>
  constexpr void
  test_static_overflow()
  {
    constexpr Int n1 = std::numeric_limits<Int>::max();
    constexpr size_t n2 = std::dynamic_extent - 1;
    // Allow some room for padding.
    constexpr size_t n = (std::cmp_less(n1, n2) ? size_t(n1) : n2) - 4;

    verify_all(typename Layout::mapping<std::extents<Int, n, n, 0, n, n>>{});
    verify_all(typename Layout::mapping<std::extents<Int, 0, n, n, n>>{});
    verify_all(typename Layout::mapping<std::extents<Int, dyn, n, n, n>>{});
    verify_all(typename Layout::mapping<std::extents<Int, n, n, n, 0>>{});
    verify_all(typename Layout::mapping<std::extents<Int, n, n, n, dyn>>{});
  }

template<typename Int, size_t N>
  constexpr std::array<Int, N>
  make_strides()
  {
    std::array<Int, N> strides;
    std::ranges::fill(strides, Int(1));
    return strides;
  }

template<typename Layout, typename Extents>
  constexpr typename Layout::mapping<Extents>
  make_mapping(Extents exts)
  {
    using IndexType = typename Extents::index_type;
    constexpr auto rank = Extents::rank();
    constexpr auto strides = make_strides<IndexType, rank>();

    if constexpr (std::same_as<Layout, std::layout_stride>)
      return typename Layout::mapping(exts, strides);
    else
      return typename Layout::mapping(exts);
  }

template<typename Layout, typename Int>
  constexpr void
  test_dynamic_overflow()
  {
    constexpr Int n1 = std::numeric_limits<Int>::max();
    constexpr size_t n2 = std::dynamic_extent - 1;
    // Allow some room for padding.
    constexpr Int n = (std::cmp_less(n1, n2) ? n1 : Int(n2)) - 4;

    verify_all(make_mapping<Layout>(
	std::extents<Int, dyn, dyn, 0, dyn, dyn>{n, n, n, n}));

    verify_all(make_mapping<Layout>(
	std::extents<Int, dyn, dyn, dyn, dyn, dyn>{n, n, 0, n, n}));

    verify_all(make_mapping<Layout>(
	std::extents<Int, dyn, dyn, dyn, 0>{n, n, n}));

    verify_all(make_mapping<Layout>(
	std::extents<Int, dyn, dyn, dyn, dyn>{n, n, n, 0}));

    verify_all(make_mapping<Layout>(
	std::extents<Int, 0, dyn, dyn, dyn>{n, n, n}));

    verify_all(make_mapping<Layout>(
	std::extents<Int, dyn, dyn, dyn, dyn>{0, n, n, n}));
  }

template<typename Layout, typename Int>
  constexpr void
  test_overflow()
  {
    test_static_overflow<Layout, Int>();
    test_dynamic_overflow<Layout, Int>();
  }

template<typename Layout>
  constexpr bool
  test_all()
  {
    test_overflow<Layout, signed char>();
    test_overflow<Layout, short int>();
    test_overflow<Layout, int>();
    test_overflow<Layout, long int>();
    test_overflow<Layout, long long int>();

    test_overflow<Layout, unsigned char>();
    test_overflow<Layout, unsigned short int>();
    test_overflow<Layout, unsigned int>();
    test_overflow<Layout, unsigned long int>();
    test_overflow<Layout, unsigned long long int>();
    test_overflow<Layout, size_t>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_padded_all()
  {
    static_assert(test_all<Layout<0>>());
    static_assert(test_all<Layout<1>>());
    static_assert(test_all<Layout<2>>());
    static_assert(test_all<Layout<dyn>>());
    return true;
  }

int
main()
{
  static_assert(test_all<std::layout_left>());
  static_assert(test_all<std::layout_right>());
  static_assert(test_all<std::layout_stride>());
#if __cplusplus > 202302L
  static_assert(test_padded_all<std::layout_left_padded>());
  static_assert(test_padded_all<std::layout_right_padded>());
#endif
  return 0;
}
