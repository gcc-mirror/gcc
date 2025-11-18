#ifndef TEST_MDSPAN_LAYOUT_TRAITS_H
#define TEST_MDSPAN_LAYOUT_TRAITS_H

#include <algorithm>

enum class PaddingSide
{
  Left,
  Right
};

template<typename Layout>
  constexpr static bool is_left_padded = false;

#if __cplusplus > 202302L
template<size_t PaddingValue>
  constexpr static bool is_left_padded<std::layout_left_padded<PaddingValue>>
    = true;
#endif

template<typename Layout>
  constexpr static bool is_right_padded = false;

#if __cplusplus > 202302L
template<size_t PaddingValue>
  constexpr static bool is_right_padded<std::layout_right_padded<PaddingValue>>
    = true;
#endif

template<typename Layout>
  constexpr bool
  is_padded_layout = is_left_padded<Layout> || is_right_padded<Layout>;

#if __cplusplus > 202302L
template<PaddingSide Side, typename Layout>
  constexpr bool
  is_same_padded;

template<typename Layout>
  constexpr bool
  is_same_padded<PaddingSide::Left, Layout> = is_left_padded<Layout>;

template<typename Layout>
  constexpr bool
  is_same_padded<PaddingSide::Right, Layout> = is_right_padded<Layout>;

template<typename Extents>
  constexpr auto
  dynamic_extents_array(const Extents& exts)
  {
    std::array<typename Extents::index_type, Extents::rank()> ret;
    for(size_t i = 0; i < Extents::rank(); ++i)
      ret[i] = exts.extent(i);
    return ret;
  }

struct DeducePaddingSide
{
  template<template<size_t> typename Layout>
    constexpr static PaddingSide
    from_template()
    {
      if constexpr (std::same_as<Layout<0>, std::layout_left_padded<0>>)
	return PaddingSide::Left;
      else
	return PaddingSide::Right;
    }

  template<typename Layout>
    constexpr static PaddingSide
    from_typename()
    {
      if constexpr (std::same_as<Layout, std::layout_left>)
	return PaddingSide::Left;
      else if constexpr (is_left_padded<Layout>)
	return PaddingSide::Left;
      else
	return PaddingSide::Right;
    }
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
      make_array(const std::array<T, N>& a)
      { return a; }

    template<typename... Indices>
      constexpr static auto
      make_indices(Indices... indices)
      { return std::array{indices...}; }

    template<typename... Ts>
      constexpr static std::tuple<Ts...>
      make_tuple(const std::tuple<Ts...>& tup)
      { return tup; }

    template<typename Mapping>
      constexpr static auto
      padded_stride(const Mapping& m)
      { return m.stride(1); }

    template<typename Extents>
      constexpr static auto
      padded_extent(const Extents& exts)
      { return exts.extent(0); }
  };

template<>
  struct LayoutTraits<PaddingSide::Right>
  {
    using layout_same = std::layout_right;
    using layout_other = std::layout_left;

    template<typename IndexType, size_t... Extents>
      constexpr static auto
      make_extents(const std::extents<IndexType, Extents...>& exts)
      {
	constexpr size_t rank = sizeof...(Extents);
	auto impl = [&]<size_t... I>(std::index_sequence<I...>)
	{
	  auto dyn_exts = make_array(dynamic_extents_array(exts));
	  return std::extents<IndexType, Extents...[rank - 1 - I]...>(dyn_exts);
	};
	return impl(std::make_index_sequence<rank>());
      }

    template<typename Extents>
      using extents_type = decltype(make_extents(std::declval<Extents>()));

    template<typename T, size_t N>
      constexpr static std::array<T, N>
      make_array(std::array<T, N> a)
      {
	std::ranges::reverse(a);
	return a;
      }

    template<typename... Indices>
      constexpr static auto
      make_indices(Indices... indices)
      { return make_array(std::array{indices...}); }

    template<typename... Ts>
      constexpr static auto
      make_tuple(const std::tuple<Ts...>& tup)
      {
	constexpr size_t rank = sizeof...(Ts);
	auto impl = [&]<size_t... I>(std::index_sequence<I...>)
	{
	  auto idx = [rank](size_t i) consteval
	  { return rank - 1 - i; };
	  return std::tuple<Ts...[idx(I)]...>{get<idx(I)>(tup)...};
	};
	return impl(std::make_index_sequence<rank>());
      }

    template<typename Mapping>
      constexpr static auto
      padded_stride(const Mapping& m)
      {
	auto rank = Mapping::extents_type::rank();
	return m.stride(rank - 2);
      }

    template<typename Extents>
      constexpr static auto
      padded_extent(const Extents& exts)
      {
	auto rank = Extents::rank();
	return exts.extent(rank - 1);
      }
  };

#endif
#endif
