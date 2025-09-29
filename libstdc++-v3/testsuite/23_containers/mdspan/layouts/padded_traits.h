#ifndef TEST_MDSPAN_PADDED_TRAITS_H
#define TEST_MDSPAN_PADDED_TRAITS_H

#include <algorithm>

template<typename Layout>
  constexpr static bool is_left_padded = false;

#if __cplusplus > 202302L
template<size_t PaddingValue>
  constexpr static bool is_left_padded<std::layout_left_padded<PaddingValue>>
    = true;
#endif

template<typename Layout>
  constexpr bool
  is_padded_layout = is_left_padded<Layout>;

#if __cplusplus > 202302L

enum class PaddingSide
{
  Left
};

struct DeducePaddingSide
{
  template<template<size_t> typename Layout>
    constexpr static PaddingSide
    from_template()
    { return PaddingSide::Left; }

  template<typename Layout>
    constexpr static PaddingSide
    from_typename()
    { return PaddingSide::Left; }
};

template<PaddingSide Side>
  struct LayoutTraits;

template<>
  struct LayoutTraits<PaddingSide::Left>
  {
    using layout_same = std::layout_left;
    using layout_other = std::layout_right;

    template<typename Extents>
      using extents_type = Extents;

    template<typename Extents>
      constexpr static extents_type<Extents>
      make_extents(const Extents& exts)
      { return exts; }

    template<typename T, size_t N>
      constexpr static std::array<T, N>
      make_array(const std::array<T, N>& expected)
      { return expected; }

    template<typename Mapping>
      constexpr static auto
      padded_stride(const Mapping& m)
      { return m.stride(1); }

    template<typename Extents>
      constexpr static auto
      padded_extent(const Extents& exts)
      { return exts.extent(0); }
  };

#endif
#endif
