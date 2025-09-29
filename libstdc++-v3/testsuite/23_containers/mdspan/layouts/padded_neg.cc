// { dg-do compile { target c++26 } }
#include <mdspan>

#include "padded_traits.h"
#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;

template<template<size_t> typename Layout>
  constexpr bool
  test_from_extens_representable_sta()
  {
    using E1 = std::extents<uint8_t, 8, 128>;
    auto m = typename Layout<dyn>::mapping(E1{}); // { dg-error "required from" }
    return true;
  }
static_assert(test_from_extens_representable_sta<std::layout_left_padded>()); // { dg-error "from here" }
static_assert(test_from_extens_representable_sta<std::layout_right_padded>()); // { dg-error "from here" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_extents_representable_padded_size()
  {
    using E1 = std::extents<uint8_t, 8, 128>;
    using E2 = std::dextents<uint8_t, 2>;

    auto m = typename Layout<dyn>::mapping(E2{E1{}}); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_extents_representable_padded_size<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_extents_representable_padded_size<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_extents_representable_stride()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<uint8_t, dyn, 1>>;
    auto m = typename Layout<128>::mapping(E1{129}); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_extents_representable_stride<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_extents_representable_stride<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_pad_representable_stride()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::dextents<uint8_t, 2>(129, 2));
    auto m = typename Layout<dyn>::mapping(exts, 128); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_from_pad_representable_stride<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_pad_representable_stride<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_pad_representable_padded_size()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::dextents<uint8_t, 2>(64, 2));
    auto m = typename Layout<dyn>::mapping(exts, 128); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_from_pad_representable_padded_size<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_pad_representable_padded_size<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_left()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using LayoutSame = typename Traits::layout_same;
    auto exts = Traits::make_extents(std::extents<uint8_t, 6, dyn>{4});
    auto ml = typename LayoutSame::mapping(exts);

    typename Layout<4>::mapping<decltype(exts)> m(ml); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_from_left<std::layout_left_padded>()); // { dg-error "required from here" }
static_assert(test_from_left<std::layout_right_padded>()); // { dg-error "required from here" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_left_bad_runtime_stride()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::extents<uint8_t, dyn, dyn>{6, 4});
    auto ml = typename Traits::layout_same::mapping(exts);

    typename Layout<4>::mapping<decltype(exts)> m(ml); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_left_bad_runtime_stride<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_left_bad_runtime_stride<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_left_representable_extents()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::extents<uint16_t, dyn, dyn>{8, 128});
    auto ml = typename Traits::layout_same::mapping(exts);

    typename Layout<8>::mapping<std::extents<uint8_t, dyn, dyn>> m(ml); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_from_left_representable_extents<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_left_representable_extents<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout, size_t PaddingValue>
  constexpr bool
  test_pad_overflow()
  {
    auto exts = std::extents<uint8_t, dyn>{4};
    auto n = size_t(1) << 9;
    auto m = typename Layout<PaddingValue>::mapping(exts, n);
    (void) m;
    return true;
  }
static_assert(test_pad_overflow<std::layout_left_padded, 1>());   // { dg-error "expansion of" }
static_assert(test_pad_overflow<std::layout_left_padded, dyn>()); // { dg-error "expansion of" }
static_assert(test_pad_overflow<std::layout_right_padded, 1>());   // { dg-error "expansion of" }
static_assert(test_pad_overflow<std::layout_right_padded, dyn>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout, size_t PaddingValue>
  constexpr bool
  test_from_pad_negative()
  {
    auto exts = std::extents(4);
    auto m = typename Layout<PaddingValue>::mapping(exts, -1);
    (void) m;
    return true;
  }
static_assert(test_from_pad_negative<std::layout_left_padded, 1>());   // { dg-error "expansion of" }
static_assert(test_from_pad_negative<std::layout_left_padded, dyn>()); // { dg-error "expansion of" }
static_assert(test_from_pad_negative<std::layout_right_padded, 1>());   // { dg-error "expansion of" }
static_assert(test_from_pad_negative<std::layout_right_padded, dyn>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout, size_t Pad>
  constexpr bool
  test_static_pad_same()
  {
    using Extents = std::extents<int, dyn>;
    using Mapping = typename Layout<Pad>::mapping<Extents>;
    auto exts = Extents{4};
    auto m = Mapping(exts, Pad + 1);      // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_static_pad_same<std::layout_left_padded, 1>()); // { dg-error "expansion of" }
static_assert(test_static_pad_same<std::layout_left_padded, 3>()); // { dg-error "expansion of" }
static_assert(test_static_pad_same<std::layout_right_padded, 1>()); // { dg-error "expansion of" }
static_assert(test_static_pad_same<std::layout_right_padded, 3>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_wrong_stride0()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto e = Traits::make_extents(std::extents{3, 5});
    auto s = Traits::make_array(std::array<int, 2>{2, 7});
    auto ms = std::layout_stride::mapping(e, s);
    auto m = typename Layout<dyn>::mapping<decltype(e)>(ms); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_stride_wrong_stride0<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_stride_wrong_stride0<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_wrong_stride1()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto e = Traits::make_extents(std::extents(3, 5));
    auto s = Traits::make_array(std::array<int, 2>{1, 3});
    auto ms = std::layout_stride::mapping(e, s);
    auto m = typename Layout<2>::mapping<decltype(e)>(ms); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_stride_wrong_stride1<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_stride_wrong_stride1<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_wrong_stride2()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto e = Traits::make_extents(std::extents(3, 5, 7));
    auto s = Traits::make_array(std::array<int, 3>{1, 4, 21});
    auto ms = std::layout_stride::mapping(e, s);
    auto m = typename Layout<dyn>::mapping<decltype(e)>(ms); // here (not implemented)
    (void) m;
    return true;
  }
static_assert(test_from_stride_wrong_stride2<std::layout_left_padded>());
static_assert(test_from_stride_wrong_stride2<std::layout_right_padded>());

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_oversized()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::extents<uint16_t, dyn, dyn>{3, 6});
    auto s = Traits::make_array(std::array<uint16_t, 2>{1, 128});
    auto ms = std::layout_stride::mapping(exts, s);

    using Mapping = typename Layout<dyn>::mapping<std::dextents<uint8_t, 2>>;
    Mapping m(ms); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_stride_oversized<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_stride_oversized<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_samepad_dyn()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto e = Traits::make_extents(std::extents(3, 5));
    auto mlp = typename Layout<dyn>::mapping(e);
    auto m = typename Layout<2>::mapping<decltype(e)>(mlp); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_samepad_dyn<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_samepad_dyn<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_samepad_sta()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto e = Traits::make_extents(std::extents{3, 5});
    auto mlp = typename Layout<3>::mapping(e);
    auto m = typename Layout<2>::mapping<decltype(e)>(mlp); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_samepad_sta<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_samepad_sta<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_samepad_oversized()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<uint16_t, 8, 128>>;
    using E2 = typename Traits::extents_type<std::extents<uint8_t, dyn, dyn>>;
    auto mlp = typename Layout<dyn>::mapping<E1>(E1{});
    auto m = typename Layout<dyn>::mapping<E2>(mlp); // { dg-error "expansion of" }
    (void) m;
    return true;
  }
static_assert(test_from_samepad_oversized<std::layout_left_padded>()); // { dg-error "expansion of" }
static_assert(test_from_samepad_oversized<std::layout_right_padded>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout, size_t RunId>
  constexpr bool
  test_to_same_not_exhaustive()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<int, 6, 5>>;
    using E2 = typename Traits::extents_type<std::extents<int, dyn, 5>>;

    [[maybe_unused]] auto msta = typename Layout<7>::mapping(E1{});
    if constexpr (RunId == 0)
      {
	auto m = typename Traits::layout_same::mapping<E1>(msta); // { dg-error "required from" }
	(void) m;
      }
    if constexpr (RunId == 1)
      {
	auto m = typename Traits::layout_same::mapping<E2>(msta); // { dg-error "expansion of" }
	(void) m;
      }

    [[maybe_unused]] auto mdyn = typename Layout<dyn>::mapping(E2{E1{}}, 7);
    if constexpr (RunId == 2)
      {
	auto m = typename Traits::layout_same::mapping<E1>(mdyn); // { dg-error "expansion of" }
	(void) m;
      }
    if constexpr (RunId == 3)
      {
	auto m = typename Traits::layout_same::mapping<E2>(mdyn); // { dg-error "expansion of" }
	(void) m;
      }
    return true;
  }
static_assert(test_to_same_not_exhaustive<std::layout_left_padded, 0>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_left_padded, 1>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_left_padded, 2>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_left_padded, 3>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_right_padded, 0>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_right_padded, 1>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_right_padded, 2>()); // { dg-error "expansion of" }
static_assert(test_to_same_not_exhaustive<std::layout_right_padded, 3>()); // { dg-error "expansion of" }

template<template<size_t> typename Layout>
  constexpr bool
  test_statically_bad_padding_value1()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    constexpr auto N = std::numeric_limits<size_t>::max() - 1;
    using Extents = typename Traits::extents_type<std::extents<size_t, N, 0>>;
    typename Layout<10>::mapping<Extents> m; // { dg-error "required from" }
    return true;
  }
static_assert(test_statically_bad_padding_value1<std::layout_left_padded>()); // { dg-error "required from" }
static_assert(test_statically_bad_padding_value1<std::layout_right_padded>()); // { dg-error "required from" }

template<template<size_t> typename Layout>
  constexpr bool
  test_statically_bad_padding_value2()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using Extents = typename Traits::extents_type<std::extents<uint8_t, 255, 0>>;
    typename Layout<2>::mapping<Extents> m; // { dg-error "required from" }
    return true;
  }
static_assert(test_statically_bad_padding_value2<std::layout_left_padded>()); // { dg-error "required from" }
static_assert(test_statically_bad_padding_value2<std::layout_right_padded>()); // { dg-error "required from" }

template<template<size_t> typename Layout>
  constexpr bool
  test_statically_oversized()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using Extents = typename Traits::extents_type<std::extents<uint8_t, 127, 2>>;
    typename Layout<2>::mapping<Extents> m; // { dg-error "required from" }
    return true;
  }
static_assert(test_statically_oversized<std::layout_left_padded>()); // { dg-error "from here" }
static_assert(test_statically_oversized<std::layout_right_padded>()); // { dg-error "from here" }

// { dg-prune-output "padding_value must be representable as index_type" }
// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "called in a constant expression" }
// { dg-prune-output "no matching function" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "__glibcxx_assert_fail()" }
// { dg-prune-output "must be compatible with other.stride" }
// { dg-prune-output "padding_value is dynamic_extent" }
// { dg-prune-output "_S_rank <= 1" }
