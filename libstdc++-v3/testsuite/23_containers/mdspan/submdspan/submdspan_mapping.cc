// { dg-do run { target c++26 } }
#include <mdspan>

#include <iostream> // TODO remove
#include "../layout_traits.h"
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;
constexpr auto all = std::full_extent;

template<typename Mapping, typename... Slices>
  constexpr auto
  call_submdspan_mapping(const Mapping& m, std::tuple<Slices...> slices)
  {
    auto impl = [&]<size_t... I>(std::index_sequence<I...>)
    { return submdspan_mapping(m, get<I>(slices)...); };
    return impl(std::make_index_sequence<sizeof...(Slices)>());
  }

template<typename Layout>
  constexpr bool
  test_layout_common_return_types()
  {
    constexpr auto padding_side = DeducePaddingSide::from_typename<Layout>();
    using Traits = LayoutTraits<padding_side>;
    using layout_unpadded = typename Traits::layout_same;

    {
      auto m0 = typename Layout::mapping(std::extents());
      auto result = submdspan_mapping(m0);
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, Layout>);
    }

    auto exts = Traits::make_extents(std::dims<5, int>(3, 5, 7, 11, 13));
    auto m = typename Layout::mapping(exts);
    auto s251 = std::strided_slice{2, 5, std::cw<1>};

    {
      auto slices = std::tuple{0, 0, 0, 0, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, layout_unpadded>);
    }

    {
      auto s0 = std::strided_slice{1, 1, std::cw<1>};
      auto slices = std::tuple{s0, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(is_same_padded<padding_side, layout_type>);
    }

    {
      auto s0 = std::strided_slice{1, 2, std::cw<1>};
      auto slices = std::tuple{s0, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(is_same_padded<padding_side, layout_type>);
    }

    {
      auto s0 = std::strided_slice{1, 2, std::cw<1>};
      auto slices = std::tuple{s0, 0, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(is_same_padded<padding_side, layout_type>);
    }

    {
      auto s0 = std::strided_slice{1, 2, 1};
      auto slices = std::tuple{s0, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, std::layout_stride>);
    }

    {
      auto slices = std::tuple{1, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, std::layout_stride>);
    }

    {
      auto s3 = std::strided_slice{2, std::cw<7>, std::cw<2>};
      auto slices = std::tuple{all, all, all, s3, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, std::layout_stride>);
    }
    return true;
  }

template<typename Layout>
  constexpr bool
  test_layout_unpadded_return_types()
  {
    constexpr auto padding_side = DeducePaddingSide::from_typename<Layout>();
    using Traits = LayoutTraits<padding_side>;

    auto exts = Traits::make_extents(std::dims<5, int>(3, 5, 7, 11, 13));
    auto m = typename Layout::mapping(exts);
    auto s251 = std::strided_slice{2, 5, std::cw<1>};

    {
      auto slices = std::tuple{all, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, Layout>);
    }
    return true;
  }

template<typename Layout>
  constexpr bool
  test_layout_padded_return_types()
  {
    constexpr auto padding_side = DeducePaddingSide::from_typename<Layout>();
    using Traits = LayoutTraits<padding_side>;

    auto exts = Traits::make_extents(std::dims<5, int>(3, 5, 7, 11, 13));
    auto m = typename Layout::mapping(exts);
    auto s251 = std::strided_slice{2, 5, std::cw<1>};

    {
      auto slices = std::tuple{all, all, all, s251, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      using layout_expected = typename Traits::layout_same_padded<dyn>;
      static_assert(std::same_as<layout_type, layout_expected>);
    }

    {
      auto slices = std::tuple{all, 0, 0, 0, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      using layout_expected = typename Traits::layout_same;
      static_assert(std::same_as<layout_type, layout_expected>);
    }

    {
      auto s121 = std::strided_slice{1, 2, std::cw<1>};
      auto slices = std::tuple{s121, 0, 0, 0, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      using layout_expected = typename Traits::layout_same;
      static_assert(std::same_as<layout_type, layout_expected>);
    }

    {
      auto s121 = std::strided_slice{1, 2, std::cw<1>};
      auto slices = std::tuple{0, s121, 0, 0, 0};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      using layout_type = typename decltype(result.mapping)::layout_type;
      static_assert(std::same_as<layout_type, std::layout_stride>);
    }
    return true;
  }

template<typename Layout>
  constexpr bool
  test_layout_unpadded_padding_value()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    auto s0 = std::strided_slice{size_t(1), size_t(2), std::cw<size_t(1)>};
    auto s3 = std::strided_slice{size_t(2), size_t(5), std::cw<size_t(1)>};

    auto check = [&](auto exts, size_t expected)
    {
      auto m = typename Layout::mapping(Traits::make_extents(exts));
      auto slices = std::tuple{s0, size_t(0), all, s3, size_t(0)};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      auto padding_value = decltype(result.mapping)::padding_value;
      VERIFY(padding_value == expected);
    };

    check(std::extents(std::cw<3>, std::cw<5>, std::cw<7>, 11, 13), 3*5);
    check(std::extents(std::cw<3>, std::cw<5>, 7, 11, 13), 3*5);
    check(std::extents(std::cw<3>, 5, 7, 11, 13), dyn);
    check(std::extents(3, 5, 7, 11, 13), dyn);
    return true;
  }

template<typename Layout>
constexpr size_t static_padding_value = 1;

template<size_t PaddingValue>
constexpr size_t static_padding_value<std::layout_left_padded<PaddingValue>> = PaddingValue;

template<size_t PaddingValue>
constexpr size_t static_padding_value<std::layout_right_padded<PaddingValue>> = PaddingValue;

template<typename Layout>
  constexpr bool
  test_layout_padded_padding_value()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
    auto s0 = std::strided_slice{size_t(1), size_t(2), std::cw<size_t(1)>};
    auto s3 = std::strided_slice{size_t(2), size_t(5), std::cw<size_t(1)>};

    auto check = [&](auto exts, size_t expected)
    {
      auto m = typename Layout::mapping(Traits::make_extents(exts));
      auto slices = std::tuple{s0, size_t(0), all, s3, size_t(0)};
      auto result = call_submdspan_mapping(m, Traits::make_tuple(slices));
      auto padding_value = decltype(result.mapping)::padding_value;
      VERIFY(padding_value == expected);
    };

    auto pad = [](int n, int m) -> size_t
    {
      constexpr auto padding_value = static_padding_value<Layout>;
      if constexpr (padding_value != dyn)
	{
	  auto npad = ((n + padding_value - 1) / padding_value) * padding_value;
	  return npad * m;
	}
      else
	return dyn;
    };

    check(std::extents(std::cw<3>, std::cw<5>, std::cw<7>, 11, 13), pad(3, 5));
    check(std::extents(std::cw<3>, std::cw<5>, 7, 11, 13), pad(3, 5));
    check(std::extents(std::cw<3>, std::cw<6>, 7, 11, 13), pad(3, 6));
    check(std::extents(std::cw<3>, 5, 7, 11, 13), dyn);
    check(std::extents(3, 5, 7, 11, 13), dyn);
    return true;
  }

constexpr bool
test_layout_stride_return_types()
{
  auto exts = std::extents(3, 5);
  auto m = std::layout_stride::mapping(exts, std::array{2, 12});

  using index_type = decltype(exts)::index_type;
  auto s1 = std::strided_slice{index_type(2), index_type(2),
			       std::cw<index_type(2)>};
  auto result = submdspan_mapping(m, index_type(1), s1);
  using layout_type = decltype(result.mapping)::layout_type;
  static_assert(std::same_as<layout_type, std::layout_stride>);
  return true;
}

template<typename Layout>
  constexpr bool
  test_return_types_all()
  {
    return true;
  }

template<typename Layout>
  constexpr bool
  test_return_types_unpadded_all()
  {
    test_layout_common_return_types<Layout>();
    static_assert(test_layout_common_return_types<Layout>());

    test_layout_unpadded_return_types<Layout>();
    static_assert(test_layout_unpadded_return_types<Layout>());

    test_layout_unpadded_padding_value<Layout>();
    static_assert(test_layout_unpadded_padding_value<Layout>());
    return true;
  }

template<typename Layout>
  constexpr bool
  test_return_types_padded_all()
  {
    test_layout_common_return_types<Layout>();
    static_assert(test_layout_common_return_types<Layout>());

    test_layout_padded_return_types<Layout>();
    static_assert(test_layout_padded_return_types<Layout>());

    test_layout_padded_padding_value<Layout>();
    static_assert(test_layout_padded_padding_value<Layout>());
    return true;
  }

int
main()
{
  test_return_types_unpadded_all<std::layout_left>();
  test_return_types_unpadded_all<std::layout_right>();

  test_return_types_padded_all<std::layout_left_padded<1>>();
  test_return_types_padded_all<std::layout_left_padded<2>>();
  test_return_types_padded_all<std::layout_left_padded<dyn>>();

  test_return_types_padded_all<std::layout_right_padded<1>>();
  test_return_types_padded_all<std::layout_right_padded<2>>();
  test_return_types_padded_all<std::layout_right_padded<dyn>>();

  test_layout_stride_return_types();
  static_assert(test_layout_stride_return_types());
  return 0;
}

