// { dg-do run { target c++26 } }
#include <mdspan>

#include <cstdint>
#include "../int_like.h"
#include "../layout_traits.h"
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<template<size_t> typename Layout>
  constexpr bool
  test_representable_padded_size()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    {
      using E = typename Traits::extents_type<std::extents<uint8_t, 64, 2>>;
      [[maybe_unused]] typename Layout<1>::mapping<E> m1;
    }

    {
      using E = typename Traits::extents_type<std::extents<uint8_t, 0, 2>>;
      [[maybe_unused]] typename Layout<0>::mapping<E> m1;
      [[maybe_unused]] typename Layout<1>::mapping<E> m2;
      [[maybe_unused]] typename Layout<128>::mapping<E> m3;
      [[maybe_unused]] typename Layout<255>::mapping<E> m4;
    }

    {
      using E = typename Traits::extents_type<std::extents<uint8_t, 0, 2>>;
      [[maybe_unused]] typename Layout<dyn>::mapping<E> m1(E{}, 0);
      [[maybe_unused]] typename Layout<dyn>::mapping<E> m2(E{}, 1);
      [[maybe_unused]] typename Layout<dyn>::mapping<E> m3(E{}, 128);
      [[maybe_unused]] typename Layout<dyn>::mapping<E> m4(E{}, 255);
    }

    {
      using E = typename Traits::extents_type<std::extents<uint8_t, dyn, 2>>;
      [[maybe_unused]] typename Layout<0>::mapping<E> m1;
      [[maybe_unused]] typename Layout<1>::mapping<E> m2;
      [[maybe_unused]] typename Layout<128>::mapping<E> m3;
      [[maybe_unused]] typename Layout<255>::mapping<E> m4;
    }
    return true;
  }

template<typename Layout, typename CanonicalExtents>
  constexpr void
  test_default_ctor_single(auto canonical_strides)
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    using E = typename Traits::extents_type<CanonicalExtents>;
    auto strides = Traits::make_array(canonical_strides);

    typename Layout::template mapping<E> msta;
    VERIFY(msta.stride(0) == strides[0]);
    VERIFY(msta.stride(1) == strides[1]);
  }


template<template<size_t> typename Layout>
  constexpr bool
  test_default_ctor()
  {
    using E1 = std::extents<size_t, 3, 5>;
    test_default_ctor_single<Layout<2>, E1>(std::array<size_t, 2>{1, 4});
    test_default_ctor_single<Layout<dyn>, E1>(std::array<size_t, 2>{1, 3});

    using E2 = std::extents<size_t, dyn, 5>;
    test_default_ctor_single<Layout<2>, E2>(std::array<size_t, 2>{1, 0});
    test_default_ctor_single<Layout<dyn>, E2>(std::array<size_t, 2>{1, 0});
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_exts()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts = Traits::make_extents(std::dextents<size_t, 2>{3, 5});

    typename Layout<0>::mapping m0_sta(exts);
    VERIFY(Traits::padded_stride(m0_sta) == Traits::padded_extent(exts));

    typename Layout<1>::mapping m1_sta(exts);
    VERIFY(Traits::padded_stride(m1_sta) == Traits::padded_extent(exts));

    typename Layout<2>::mapping m2_sta(exts);
    VERIFY(Traits::padded_stride(m2_sta) == 4);

    typename Layout<dyn>::mapping mdyn(exts);
    VERIFY(Traits::padded_stride(mdyn) == Traits::padded_extent(exts));
    return true;
  }

template<typename Layout, typename CustomPadType>
  constexpr bool
  test_from_pad_single()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    auto pad = 3;
    auto exts = Traits::make_extents(std::dextents<size_t, 3>{pad + 1, 5, 7});
    typename Layout::mapping m(exts, CustomPadType{pad});
    VERIFY(std::cmp_equal(Traits::padded_stride(m), 2*pad));
    VERIFY(m.extents() == exts);
    return true;
  }

template<typename Layout>
  constexpr void
  test_from_pad()
  {
    test_from_pad_single<Layout, int>();
    static_assert(test_from_pad_single<Layout, int>());

    test_from_pad_single<Layout, IntLike>();
    test_from_pad_single<Layout, MutatingInt>();
    test_from_pad_single<Layout, RValueInt>();

    using Extents = std::dims<3>;
    using Mapping = Layout::template mapping<Extents>;
    static_assert(!std::is_constructible_v<Mapping, Extents, ThrowingInt>);
    static_assert(!std::is_constructible_v<Mapping, Extents, NotIntLike>);
  }

template<template<size_t> typename Layout>
  constexpr void
  test_from_pad_all()
  {
    test_from_pad<Layout<3>>();
    test_from_pad<Layout<dyn>>();
  }

constexpr bool
is_same_mapping(const auto& lhs, const auto& rhs)
{
  if (lhs.extents().rank() != rhs.extents().rank())
    return false;

  if (lhs.extents() != rhs.extents())
    return false;

  for (size_t i = 0; i < lhs.extents().rank(); ++i)
    if (lhs.stride(i) != rhs.stride(i))
      return false;
  return true;
}

enum class ConversionRule
{
  Never,
  Regular
};

template<typename To, typename From>
consteval bool
should_convert(auto rule)
{
  if constexpr (rule == ConversionRule::Never)
    return false;
  else
    return std::is_convertible_v<typename From::extents_type,
				 typename To::extents_type>;
}

template<typename To, typename From>
  constexpr void
  check_convertible(const From& m, auto conversion_rule)
  {
    VERIFY(is_same_mapping(m, To(m)));
    constexpr bool expected = should_convert<To, From>(conversion_rule);
    static_assert(std::is_convertible_v<From, To> == expected);
  }

template<typename LayoutTo, typename Esta, typename Edyn, typename Ewrong>
  constexpr void
  check_convertible_variants(auto msta, auto conversion_rule)
  {
    using LayoutFrom = decltype(msta)::layout_type;
    constexpr auto cregular = std::cw<ConversionRule::Regular>;

    // There's a twist when both mappings are left-padded. There's two distinct
    // ctors: a defaulted copy ctor and a constrained template that enables
    // construction from left-padded mappings even if their layout_type (padding) is
    // different. The two ctors have different rules regarding conversion.

    if constexpr (!std::same_as<LayoutTo, LayoutFrom>)
      check_convertible<typename LayoutTo::mapping<Esta>>(msta, conversion_rule);
    else
      check_convertible<typename LayoutTo::mapping<Esta>>(msta, cregular);

    check_convertible<typename LayoutTo::mapping<Edyn>>(msta, conversion_rule);

    auto mdyn = typename LayoutFrom::mapping<Edyn>(msta);
    check_convertible<typename LayoutTo::mapping<Esta>>(mdyn, conversion_rule);
    if constexpr (!std::same_as<LayoutTo, LayoutFrom>)
      check_convertible<typename LayoutTo::mapping<Edyn>>(mdyn, conversion_rule);
    else
      check_convertible<typename LayoutTo::mapping<Edyn>>(mdyn, cregular);

    static_assert(!std::is_constructible_v<
	typename LayoutTo::mapping<Esta>, typename LayoutFrom::mapping<Ewrong>>);
  };

template<typename Layout>
  constexpr void
  test_from_same_1d()
  {
    using E1 = std::extents<int, 6>;
    using E2 = std::extents<int, dyn>;
    using E3 = std::extents<int, 5>;
    constexpr auto cr = std::cw<ConversionRule::Regular>;

    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    auto msta = typename Traits::layout_same::mapping(E1{});
    check_convertible_variants<Layout, E1, E2, E3>(msta, cr);
  }

template<typename Layout>
  constexpr void
  test_from_same_2d()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<int, 6, 5>>;
    using E2 = typename Traits::extents_type<std::extents<int, dyn, 5>>;
    using E3 = typename Traits::extents_type<std::extents<int, 6, 6>>;
    constexpr auto cr = std::cw<ConversionRule::Regular>;

    auto msta = typename Traits::layout_same::mapping(E1{});
    check_convertible_variants<Layout, E1, E2, E3>(msta, cr);
  }

template<template<size_t> typename Layout>
constexpr bool
test_from_same()
{
  auto check = []<typename PaddedLayout>(PaddedLayout)
  {
    test_from_same_1d<PaddedLayout>();
    test_from_same_2d<PaddedLayout>();
  };

  check(Layout<0>{});
  check(Layout<1>{});
  check(Layout<2>{});
  check(Layout<6>{});
  check(Layout<dyn>{});

  // rank == 1 is more permissive:
  test_from_same_1d<Layout<5>>();
  return true;
}

template<template<size_t> typename Layout, typename E1_, typename E2_,
	 typename E3_>
  constexpr bool
  test_from_stride_nd(auto strides_)
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<E1_>;
    using E2 = typename Traits::extents_type<E2_>;
    using E3 = typename Traits::extents_type<E3_>;
    auto strides = Traits::make_array(strides_);

    auto check = [&strides]<typename PaddedLayout>(PaddedLayout)
    {
      auto exts = E1{};
      constexpr auto cr = std::cw<ConversionRule::Never>;

      auto m = std::layout_stride::mapping(exts, strides);
      check_convertible_variants<PaddedLayout, E1, E2, E3>(m, cr);
    };

    check(Layout<0>{});
    check(Layout<1>{});
    check(Layout<2>{});
    check(Layout<6>{});
    check(Layout<dyn>{});
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_2d()
  {
    using E1 = std::extents<size_t, 6, 5>;
    using E2 = std::dims<2>;
    using E3 = std::extents<size_t, 6, 6>;

    auto strides = std::array<int, 2>{1, 6};
    test_from_stride_nd<Layout, E1, E2, E3>(strides);
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride_3d()
  {
    using E1 = std::extents<size_t, 6, 5, 7>;
    using E2 = std::dims<3>;
    using E3 = std::extents<size_t, 6, 6, 7>;

    auto strides = std::array<int, 3>{1, 6, 6*5};
    test_from_stride_nd<Layout, E1, E2, E3>(strides);
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_stride()
  {
    test_from_stride_2d<Layout>();
    test_from_stride_3d<Layout>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr void
  test_from_samepad_0d()
  {
    using E1 = std::extents<uint16_t>;
    using E2 = std::extents<uint8_t>;
    using E3 = std::extents<uint8_t, 1>;

    typename Layout<6>::mapping<E1> msta{E1{}};

    auto check = []<typename To>(To, auto m)
    {
      constexpr auto cr = std::cw<ConversionRule::Regular>;
      check_convertible_variants<To, E1, E2, E3>(m, cr);
    };

    check(Layout<6>{}, msta);
    check(Layout<dyn>{}, msta);
  }

template<template<size_t> typename Layout>
  constexpr void
  test_from_samepad_1d()
  {
    using E1 = std::extents<int, 6>;
    using E2 = std::extents<int, dyn>;
    using E3 = std::extents<int, 6, 6>;

    typename Layout<6>::mapping<E1> msta{E1{}};
    typename Layout<dyn>::mapping<E1> mdyn{E1{}};

    auto check = []<typename To>(To, auto m)
    {
      constexpr auto cr = std::cw<ConversionRule::Regular>;
      check_convertible_variants<To, E1, E2, E3>(m, cr);
    };

    // Remember, for rank <= 1 the padding_value is irrelevant.
    check(Layout<6>{}, msta);
    check(Layout<6>{}, mdyn);
    check(Layout<dyn>{}, msta);
    check(Layout<dyn>{}, mdyn);
  }

template<template<size_t> typename Layout>
  constexpr void
  test_from_samepad_2d()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<int, 6, 5>>;
    using E2 = typename Traits::extents_type<std::extents<int, dyn, 5>>;
    using E3 = typename Traits::extents_type<std::extents<int, 6, 6>>;

    typename Layout<6>::mapping<E1> msta{E1{}};
    typename Layout<dyn>::mapping<E1> mdyn{E1{}};

    constexpr auto cregular = std::cw<ConversionRule::Regular>;
    constexpr auto cnever = std::cw<ConversionRule::Never>;

    auto check = []<typename To>(To, auto m, auto cr)
    { check_convertible_variants<To, E1, E2, E3>(m, cr); };

    check(Layout<6>{}, msta, cnever);
    check(Layout<6>{}, mdyn, cnever);
    check(Layout<dyn>{}, msta, cregular);
    check(Layout<dyn>{}, mdyn, cnever);
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_samepad()
  {
    test_from_samepad_0d<Layout>();
    test_from_samepad_1d<Layout>();
    test_from_samepad_2d<Layout>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_from_other()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = std::extents<size_t, 3>;
    using E2 = std::dims<1>;
    using E3 = std::extents<size_t, 5>;

    auto check = []<typename PaddedLayout>(PaddedLayout)
    {
      constexpr auto cr = std::cw<ConversionRule::Regular>;

      using layout_other = typename Traits::layout_other;
      auto msta = typename layout_other::mapping(E1{});
      check_convertible_variants<PaddedLayout, E1, E2, E3>(msta, cr);
    };


    // Remember, the padding_value has no effect for rank <= 1.
    check(Layout<0>{});
    check(Layout<1>{});
    check(Layout<2>{});
    check(Layout<5>{});
    check(Layout<6>{});
    check(Layout<dyn>{});
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_to_same()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = typename Traits::extents_type<std::extents<int, 6, 5>>;
    using E2 = typename Traits::extents_type<std::extents<int, dyn, 5>>;
    using E3 = typename Traits::extents_type<std::extents<int, 6, 6>>;

    auto check = [](auto msta)
    {
      constexpr auto cr = std::cw<ConversionRule::Regular>;
      using LayoutSame = typename Traits::layout_same;
      check_convertible_variants<LayoutSame, E1, E2, E3>(msta, cr);
    };

    check(typename Layout<0>::mapping(E1{}));
    check(typename Layout<2>::mapping(E1{}));
    check(typename Layout<6>::mapping(E1{}));
    check(typename Layout<dyn>::mapping(E1{}, 0));
    check(typename Layout<dyn>::mapping(E1{}, 2));
    check(typename Layout<dyn>::mapping(E1{}, 6));
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_never_to_other()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using E1 = std::extents<size_t, 3>;
    using E2 = std::dims<1>;

    auto check = []<typename PaddedLayout>(PaddedLayout, auto exts)
    {
      using LayoutOther = typename Traits::layout_other;
      auto mr = typename LayoutOther::mapping(exts);
      auto mlp = typename PaddedLayout::mapping<decltype(exts)>{mr};
      static_assert(!std::is_constructible_v<decltype(mr), decltype(mlp)>);
    };

    check(Layout<2>{}, E1{});
    check(Layout<2>{}, E2{E1{}});
    return true;
  }

template<typename Layout>
  constexpr void
  test_strides()
  {
    auto check = [](auto exts)
    {
      auto m = typename Layout::mapping(exts);
      using IndexType = typename decltype(m)::index_type;
      constexpr size_t rank = decltype(m)::extents_type::rank();

      auto strides = m.strides();
      static_assert(std::same_as<decltype(strides),
				 std::array<IndexType, rank>>);
      VERIFY(strides.size() == rank);
      for (size_t i = 0; i < strides.size(); ++i)
	VERIFY(strides[i] == m.stride(i));
    };

    check(std::extents());
    check(std::extents(0));
    check(std::extents(3));
    check(std::extents(3, 5, 7));
    check(std::extents(3, 0, 7));
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_strides_all()
  {
    test_strides<Layout<0>>();
    test_strides<Layout<1>>();
    test_strides<Layout<3>>();
    test_strides<Layout<dyn>>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr void
  test_exhaustive_0d()
  {
    auto exts = std::extents<int>{};

    auto check = [](auto m)
    {
      static_assert(m.is_always_exhaustive());
      VERIFY(m.is_exhaustive());
    };

    check(typename Layout<0>::mapping(exts));
    check(typename Layout<1>::mapping(exts));
    check(typename Layout<2>::mapping(exts));
    check(typename Layout<dyn>::mapping(exts));
  }

template<template<size_t> typename Layout>
constexpr void
  test_exhaustive_1d()
  {
    auto check_dyn_and_sta = []<typename PaddedLayout>(PaddedLayout)
    {
      auto check = [](auto exts)
      {
	auto m = typename PaddedLayout::mapping(exts);
	static_assert(m.is_always_exhaustive());
	VERIFY(m.is_exhaustive());
      };

      check(std::extents(4));
      check(std::extents<int, 4>{});
    };

    check_dyn_and_sta(Layout<1>{});
    check_dyn_and_sta(Layout<2>{});
    check_dyn_and_sta(Layout<6>{});
    check_dyn_and_sta(Layout<dyn>{});
  }

template<template<size_t> typename Layout>
  constexpr void
  test_exhaustive_3d()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    auto exts_dyn = Traits::make_extents(std::extents(4, 5, 7));
    auto exts_sta = Traits::make_extents(std::extents<int, 4, 5, 7>{});
    auto ctrue = std::cw<true>;
    auto cfalse= std::cw<false>;

    auto check = [](auto m, auto static_expected, auto runtime_expected)
    {
      static_assert(m.is_always_exhaustive() == static_expected);
      VERIFY(m.is_exhaustive() == runtime_expected);
    };

    check(typename Layout<0>::mapping(exts_sta), ctrue, true);
    check(typename Layout<0>::mapping(exts_dyn), cfalse, true);
    check(typename Layout<1>::mapping(exts_sta), ctrue, true);
    check(typename Layout<1>::mapping(exts_dyn), cfalse, true);
    check(typename Layout<2>::mapping(exts_sta), ctrue, true);
    check(typename Layout<2>::mapping(exts_dyn), cfalse, true);
    check(typename Layout<6>::mapping(exts_dyn), cfalse, false);
    check(typename Layout<6>::mapping(exts_sta), cfalse, false);
    check(typename Layout<dyn>::mapping(exts_sta), cfalse, true);
    check(typename Layout<dyn>::mapping(exts_dyn, 2), cfalse, true);
    check(typename Layout<dyn>::mapping(exts_dyn, 3), cfalse, false);
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_exhaustive()
  {
    test_exhaustive_0d<Layout>();
    test_exhaustive_1d<Layout>();
    test_exhaustive_3d<Layout>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_op_eq()
  {
    // The generic cases are handled in layouts/mapping.cc. Here we check
    // special cases related to non exhaustive layouts.
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;

    auto exts_sta = Traits::make_extents(std::extents<size_t, 6, 5, 7>{});
    auto exts_dyn = std::dims<3>(exts_sta);
    auto exts_other = Traits::make_extents(std::extents<size_t, 7, 5, 7>{});

    auto m1 = typename Layout<0>::mapping(exts_sta);
    auto m2 = typename Layout<7>::mapping(exts_sta);

    VERIFY(m1 == typename Layout<0>::mapping(exts_sta));
    VERIFY(m1 == typename Layout<1>::mapping(exts_sta));
    VERIFY(m1 == typename Layout<2>::mapping(exts_sta));
    VERIFY(m1 == typename Layout<6>::mapping(exts_sta));
    VERIFY(m1 != typename Layout<7>::mapping(exts_sta));
    VERIFY(m1 == typename Layout<dyn>::mapping(exts_sta));
    VERIFY(m1 != typename Layout<dyn>::mapping(exts_sta, 7));

    VERIFY(m1 == typename Layout<0>::mapping(exts_dyn));
    VERIFY(m1 == typename Layout<1>::mapping(exts_dyn));
    VERIFY(m1 == typename Layout<2>::mapping(exts_dyn));
    VERIFY(m1 == typename Layout<6>::mapping(exts_dyn));
    VERIFY(m1 != typename Layout<7>::mapping(exts_dyn));
    VERIFY(m1 == typename Layout<dyn>::mapping(exts_dyn));
    VERIFY(m1 != typename Layout<dyn>::mapping(exts_dyn, 7));

    VERIFY(m2 == typename Layout<7>::mapping(exts_sta));
    VERIFY(m2 == typename Layout<dyn>::mapping(exts_sta, 7));
    VERIFY(m2 == typename Layout<7>::mapping(exts_dyn));
    VERIFY(m2 == typename Layout<dyn>::mapping(exts_dyn, 7));

    VERIFY(m2 != typename Layout<7>::mapping(exts_other));
    VERIFY(m2 != typename Layout<dyn>::mapping(exts_other, 7));
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_required_span_size_overflow()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    using Extents = std::dextents<uint8_t, 2>;
    auto exts = Traits::make_extents(Extents{64, 2});
    auto strides = Traits::make_array(std::array<uint8_t, 2>{1, 128});
    auto ms = std::layout_stride::mapping(exts, strides);
    auto m = typename Layout<dyn>::mapping<Extents>(ms);
    VERIFY(is_same_mapping(m, ms));
    VERIFY(m.required_span_size() == ms.required_span_size());
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_all()
  {
    test_representable_padded_size<std::layout_left_padded>();
    test_default_ctor<Layout>();
    test_from_exts<Layout>();
    test_from_stride<Layout>();
    test_from_samepad<Layout>();
    test_from_same<Layout>();
    test_from_other<Layout>();
    test_to_same<Layout>();
    test_never_to_other<Layout>();
    test_strides_all<Layout>();
    test_exhaustive<Layout>();
    test_op_eq<Layout>();
    test_required_span_size_overflow<Layout>();
    return true;
  }

int
main()
{
  test_all<std::layout_left_padded>();
  static_assert(test_all<std::layout_left_padded>());

  test_all<std::layout_right_padded>();
  static_assert(test_all<std::layout_right_padded>());

  test_from_pad_all<std::layout_left_padded>();
  test_from_pad_all<std::layout_right_padded>();
  return 0;
}
