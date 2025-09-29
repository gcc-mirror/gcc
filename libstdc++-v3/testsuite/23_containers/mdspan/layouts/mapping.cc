// { dg-do run { target c++23 } }
#include <mdspan>

#include "../int_like.h"
#include "padded_traits.h"
#include <cstdint>
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<typename Mapping>
  concept has_static_is_exhaustive = requires
  {
    { Mapping::is_exhaustive() } -> std::same_as<bool>;
  };

static_assert(has_static_is_exhaustive<std::layout_right::mapping<std::extents<int>>>);
static_assert(!has_static_is_exhaustive<std::layout_stride::mapping<std::extents<int>>>);

template<typename Layout, typename Extents>
  constexpr bool
  test_mapping_properties()
  {
    using M = typename Layout::mapping<Extents>;
    static_assert(std::__mdspan::__is_extents<typename M::extents_type>);
    static_assert(std::__mdspan::__mapping_alike<M>);
    static_assert(std::copyable<M>);
    static_assert(std::is_nothrow_move_constructible_v<M>);
    static_assert(std::is_nothrow_move_assignable_v<M>);
    static_assert(std::is_nothrow_swappable_v<M>);
    static_assert(std::is_same_v<typename M::extents_type, Extents>);
    static_assert(std::is_same_v<typename M::index_type,
				 typename M::extents_type::index_type>);
    static_assert(std::is_same_v<typename M::size_type,
				 typename M::extents_type::size_type>);
    static_assert(std::is_same_v<typename M::rank_type,
				 typename M::extents_type::rank_type>);
    static_assert(std::is_same_v<typename M::layout_type, Layout>);

    static_assert(std::is_trivially_copyable_v<M>);
    static_assert(std::regular<M>);

    static_assert(M::is_always_unique() && M::is_unique());
    static_assert(M::is_always_strided() && M::is_strided());
    if constexpr (has_static_is_exhaustive<M>)
      static_assert(M::is_always_exhaustive() && M::is_exhaustive());
    return true;
  }

template<typename Layout>
  constexpr bool
  test_mapping_properties_all()
  {
    test_mapping_properties<Layout, std::extents<int>>();
    test_mapping_properties<Layout, std::extents<int, 1>>();
    test_mapping_properties<Layout, std::extents<int, dyn>>();
    test_mapping_properties<Layout, std::extents<int, dyn, dyn>>();
    return true;
  }

// Check operator()(Indices...)
template<typename Mapping, size_t N>
  constexpr typename Mapping::index_type
  linear_index(const Mapping& mapping,
	       const std::array<typename Mapping::index_type, N>& indices)
  {
    typename Mapping::index_type ret = 0;
    for(size_t r = 0; r < indices.size(); ++r)
      ret += indices[r] * mapping.stride(r);
    return ret;
  }

template<typename Int, typename Mapping, typename... Indices>
  constexpr void
  test_linear_index(const Mapping& m, Indices... i)
  {
    using index_type = typename Mapping::index_type;
    index_type expected = linear_index(m, std::array{index_type(i)...});
    VERIFY(m(Int(i)...) == expected);
  }

template<typename Layout>
  constexpr void
  test_linear_index_0d()
  {
    constexpr typename Layout::mapping<std::extents<int>> m;
    VERIFY(m() == 0);
  }

template<typename Layout, typename Int>
  constexpr void
  test_linear_index_1d()
  {
    typename Layout::mapping<std::extents<int, 5>> m;
    test_linear_index<Int>(m, 0);
    test_linear_index<Int>(m, 1);
    test_linear_index<Int>(m, 4);
  }

template<typename Layout, typename Int>
  constexpr void
  test_linear_index_2d()
  {
    typename Layout::mapping<std::extents<int, 3, 256>> m;
    test_linear_index<Int>(m, 0, 0);
    test_linear_index<Int>(m, 1, 0);
    test_linear_index<Int>(m, 0, 1);
    test_linear_index<Int>(m, 1, 1);
    test_linear_index<Int>(m, 2, 4);
  }

template<typename Layout>
  struct MappingFactory
  {
    template<typename Extents>
      static constexpr typename Layout::mapping<Extents>
      create(Extents exts)
      { return exts; }
  };

template<>
  struct MappingFactory<std::layout_stride>
  {
    template<typename Extents>
      static constexpr std::layout_stride::mapping<Extents>
      create(Extents exts)
      {
	if constexpr (Extents::rank() == 0)
	  {
	    auto strides = std::array<size_t, 0>{};
	    return std::layout_stride::mapping(exts, strides);
	  }
	else if constexpr (Extents::rank() == 1)
	  {
	    auto strides = std::array<size_t, 1>{2};
	    return std::layout_stride::mapping(exts, strides);
	  }
	else if constexpr (Extents::rank() == 2)
	  {
	    size_t m = exts.extent(1);
	    auto strides = std::array<size_t, 2>{3*m, 2};
	    return std::layout_stride::mapping(exts, strides);
	  }
	else if constexpr (Extents::rank() == 3)
	  {
	    size_t n = exts.extent(0);
	    size_t m = exts.extent(1);
	    auto strides = std::array<size_t, 3>{3*m, 2, 11*m*n};
	    return std::layout_stride::mapping(exts, strides);
	  }
      }
  };

template<typename Layout, typename Int>
  constexpr void
  test_linear_index_3d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3, 5, 7));
    test_linear_index<Int>(m, 0, 0, 0);
    test_linear_index<Int>(m, 1, 0, 0);
    test_linear_index<Int>(m, 0, 1, 0);
    test_linear_index<Int>(m, 0, 0, 1);
    test_linear_index<Int>(m, 1, 1, 0);
    test_linear_index<Int>(m, 2, 4, 6);
  }

template<typename Mapping, typename... Ints>
  concept has_linear_index = requires (Mapping m)
  {
    { m(Ints(0)...) } -> std::same_as<typename Mapping::index_type>;
  };

template<typename Layout>
  constexpr void
  test_has_linear_index_0d()
  {
    using Mapping = typename Layout::mapping<std::extents<int>>;
    static_assert(has_linear_index<Mapping>);
    static_assert(!has_linear_index<Mapping, int>);
    static_assert(!has_linear_index<Mapping, IntLike>);
    static_assert(!has_linear_index<Mapping, NotIntLike>);
  }

template<typename Layout>
  constexpr void
  test_has_linear_index_1d()
  {
    using Mapping = typename Layout::mapping<std::extents<int, 3>>;
    static_assert(!has_linear_index<Mapping>);
    static_assert(has_linear_index<Mapping, int>);
    static_assert(has_linear_index<Mapping, double>);
    static_assert(has_linear_index<Mapping, IntLike>);
    static_assert(has_linear_index<Mapping, MutatingInt>);
    static_assert(!has_linear_index<Mapping, ThrowingInt>);
    static_assert(!has_linear_index<Mapping, NotIntLike>);
    static_assert(!has_linear_index<Mapping, int, int>);
  }

template<typename Layout>
  constexpr void
  test_has_linear_index_2d()
  {
    using Mapping = typename Layout::mapping<std::extents<int, 3, 5>>;
    static_assert(!has_linear_index<Mapping, int>);
    static_assert(has_linear_index<Mapping, int, int>);
    static_assert(has_linear_index<Mapping, double, double>);
    static_assert(has_linear_index<Mapping, IntLike, int>);
    static_assert(has_linear_index<Mapping, MutatingInt, int>);
    static_assert(!has_linear_index<Mapping, ThrowingInt, int>);
    static_assert(!has_linear_index<Mapping, NotIntLike, int>);
    static_assert(!has_linear_index<Mapping, int, int, int>);
  }

template<typename Layout, typename Int>
  constexpr bool
  test_linear_index_all()
  {
    test_linear_index_0d<Layout>();
    test_linear_index_1d<Layout, Int>();
    test_linear_index_2d<Layout, Int>();
    test_linear_index_3d<Layout, Int>();
    test_has_linear_index_0d<Layout>();
    test_has_linear_index_1d<Layout>();
    test_has_linear_index_2d<Layout>();
    return true;
  }

template<typename Mapping>
  constexpr typename Mapping::index_type
  linear_index_end(Mapping m)
  {
    using index_type = typename Mapping::index_type;
    constexpr size_t rank = Mapping::extents_type::rank();

    auto impl = [m]<index_type... Counts>(
	std::integer_sequence<index_type, Counts...>) -> index_type
      {
	auto exts = m.extents();
	if(((exts.extent(Counts) == 0) || ...))
	  return 0;
	return m((exts.extent(Counts) - 1)...) + 1;
      };

    return impl(std::make_integer_sequence<index_type, rank>());
  }

// Check required_span_size
template<typename Mapping>
  constexpr void
  test_required_span_size(Mapping m)
  { VERIFY(m.required_span_size() == linear_index_end(m)); }

template<typename Layout>
  constexpr void
  test_required_span_size_0d()
  {
    typename Layout::mapping<std::extents<int>> m;
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr void
  test_required_span_size_1d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3));
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr void
  test_required_span_size_2d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3, 5));
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr void
  test_required_span_size_3d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3, 5, 7));
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr void
  test_required_span_size_zero_1d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3, 0));
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr void
  test_required_span_size_zero_3d()
  {
    auto m = MappingFactory<Layout>::create(std::extents(3, 0, 7));
    test_required_span_size(m);
  }

template<typename Layout>
  constexpr bool
  test_required_span_size_all()
  {
    test_required_span_size_0d<Layout>();
    test_required_span_size_1d<Layout>();
    test_required_span_size_2d<Layout>();
    test_required_span_size_3d<Layout>();
    test_required_span_size_zero_1d<Layout>();
    test_required_span_size_zero_3d<Layout>();
    return true;
  }

// Check stride
template<typename Layout>
  constexpr void
  test_stride_1d()
  {
    typename Layout::mapping<std::extents<int, 3>> m;
    VERIFY(m.stride(0) == 1);
  }

template<>
  constexpr void
  test_stride_1d<std::layout_stride>()
  {
    std::array<int, 1> strides{13};
    std::layout_stride::mapping m(std::extents<int, 3>{}, strides);
    VERIFY(m.stride(0) == strides[0]);
    VERIFY(m.strides() == strides);
  }

template<typename Layout>
struct TestStride2D;

template<>
  struct TestStride2D<std::layout_left>
  {
    static constexpr void
    run()
    {
      std::layout_left::mapping<std::extents<int, 3, 5>> m;
      VERIFY(m.stride(0) == 1);
      VERIFY(m.stride(1) == 3);
    }
  };

template<>
  struct TestStride2D<std::layout_right>
  {
    static constexpr void
    run()
    {
      std::layout_right::mapping<std::extents<int, 3, 5>> m;
      VERIFY(m.stride(0) == 5);
      VERIFY(m.stride(1) == 1);
    }
  };

template<>
  struct TestStride2D<std::layout_stride>
  {
    static constexpr void
    run()
    {
      std::array<int, 2> strides{13, 2};
      std::layout_stride::mapping m(std::extents<int, 3, 5>{}, strides);
      VERIFY(m.stride(0) == strides[0]);
      VERIFY(m.stride(1) == strides[1]);
      VERIFY(m.strides() == strides);
    }
  };

#if __cplusplus > 202302L
template<typename Layout>
  requires is_left_padded<Layout> || is_right_padded<Layout>
  struct TestStride2D<Layout>
  {
    static constexpr void
    run()
    {
      using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
      using Extents = typename Traits::extents_type<std::extents<int, 3, 5>>;
      using Mapping = typename Layout::mapping<Extents>;
      constexpr size_t padding_value = Mapping::padding_value;

      Mapping m;
      size_t effective_pad = (padding_value == 0 || padding_value == dyn)
	? size_t(1) : padding_value;

      constexpr auto i0 = is_left_padded<Layout> ? 0 : 1;
      VERIFY(m.stride(i0) == 1);

      // The next multiple of padding_value, that's greater or equal
      // to exts.extent(0) is the unique value in the range:
      //   [exts.extent(0), exts.extent(0) + padding_value)
      // that is divisible by padding_value.
      auto stride = Traits::padded_stride(m);
      VERIFY((stride % effective_pad) == 0);
      VERIFY(3 <= stride && std::cmp_less(stride, 3 + effective_pad));
    }
  };
#endif

template<typename Layout>
  constexpr void
  test_stride_2d()
  {
    TestStride2D<Layout>::run();
  }

template<typename Layout>
struct TestStride3D;

template<>
  struct TestStride3D<std::layout_left>
  {
    static constexpr void
    run()
    {
      std::layout_left::mapping m(std::dextents<int, 3>(3, 5, 7));
      VERIFY(m.stride(0) == 1);
      VERIFY(m.stride(1) == 3);
      VERIFY(m.stride(2) == 3*5);
    }
  };


template<>
  struct TestStride3D<std::layout_right>
  {
    static constexpr void
    run()
    {
      std::layout_right::mapping m(std::dextents<int, 3>(3, 5, 7));
      VERIFY(m.stride(0) == 5*7);
      VERIFY(m.stride(1) == 7);
      VERIFY(m.stride(2) == 1);
    }
  };

template<>
  struct TestStride3D<std::layout_stride>
  {
    static constexpr void
    run()
    {
      std::dextents<int, 3> exts(3, 5, 7);
      std::array<int, 3> strides{11, 2, 41};
      std::layout_stride::mapping<std::dextents<int, 3>> m(exts, strides);
      VERIFY(m.stride(0) == strides[0]);
      VERIFY(m.stride(1) == strides[1]);
      VERIFY(m.stride(2) == strides[2]);
      VERIFY(m.strides() == strides);
    }
  };

#if __cplusplus > 202302L
template<typename Layout>
  requires is_left_padded<Layout> || is_right_padded<Layout>
  struct TestStride3D<Layout>
  {
    static constexpr void
    run()
    {
      using Traits = LayoutTraits<DeducePaddingSide::from_typename<Layout>()>;
      using Extents = typename Traits::extents_type<std::extents<int, 3, 5, 7>>;
      using Mapping = typename Layout::mapping<Extents>;
      constexpr size_t padding_value = Mapping::padding_value;

      Mapping m;
      size_t effective_pad = (padding_value == 0 || padding_value == dyn)
	? size_t(1) : padding_value;

      constexpr auto i0 = is_left_padded<Layout> ? 0 : 2;
      VERIFY(m.stride(i0) == 1);

      // The next multiple of padding_value, that's greater or equal
      // to exts.extent(0) is the unique value in the range:
      //   [exts.extent(0), exts.extent(0) + padding_value)
      // that is divisible by padding_value.
      auto stride = Traits::padded_stride(m);
      VERIFY((stride % effective_pad) == 0);
      VERIFY(3 <= stride && std::cmp_less(stride, 3 + effective_pad));

      constexpr auto i2 = is_left_padded<Layout> ? 2 : 0;
      VERIFY(stride * 5 == m.stride(i2));
    }
  };
#endif

template<typename Layout>
  constexpr void
  test_stride_3d()
  {
    TestStride3D<Layout>::run();
  }

template<typename Layout>
  constexpr bool
  test_stride_all()
  {
    test_stride_1d<Layout>();
    test_stride_2d<Layout>();
    test_stride_3d<Layout>();
    return true;
  }

template<typename Mapping>
  concept has_stride = requires (Mapping m)
  {
    { m.stride(0) } -> std::same_as<typename Mapping::index_type>;
  };

template<typename Layout>
  constexpr void
  test_has_stride_0d()
  {
    using Mapping = typename Layout::mapping<std::extents<int>>;
    constexpr bool expected = !(std::is_same_v<Layout, std::layout_left>
       || std::is_same_v<Layout, std::layout_right>);
    static_assert(has_stride<Mapping> == expected);
  }

template<typename Layout>
  constexpr void
  test_has_stride_1d()
  { static_assert(has_stride<typename Layout::mapping<std::extents<int, 1>>>); }

template<typename Layout>
  constexpr void
  test_has_stride_2d()
  {
    using Extents = std::extents<int, 1, 2>;
    static_assert(has_stride<typename Layout::mapping<Extents>>);
  }

// Check operator==
template<typename Layout>
  constexpr void
  test_eq()
  {
    typename Layout::mapping<std::extents<int, 1, 2>> m1;
    typename Layout::mapping<std::extents<int, 2, 2>> m2;
    typename Layout::mapping<std::dextents<int, 2>> m3(m1);

    VERIFY(m1 == m1);
    VERIFY(m1 != m2);
    VERIFY(m1 == m3);
    VERIFY(m2 != m3);
  }

template<typename Layout>
  constexpr void
  test_eq_zero()
  {
    typename Layout::mapping<std::extents<int, 0, 2>> m1;
    typename Layout::mapping<std::extents<int, 0, 2>> m2;
    typename Layout::mapping<std::extents<int, 2, 0>> m3;

    VERIFY(m1 == m2);
    VERIFY(m1 != m3);
  }

template<typename M1, typename M2>
  concept has_op_eq = requires (M1 m1, M2 m2)
  {
    { m1 == m2 } -> std::same_as<bool>;
    { m2 == m1 } -> std::same_as<bool>;
    { m1 != m2 } -> std::same_as<bool>;
    { m2 != m1 } -> std::same_as<bool>;
  };

template<typename SLayout, typename OLayout, bool Expected>
  constexpr void
  test_has_op_eq()
  {
    static_assert(has_op_eq<
	typename SLayout::mapping<std::extents<int>>,
	typename OLayout::mapping<std::extents<int>>> == Expected);

    static_assert(!has_op_eq<
	typename SLayout::mapping<std::extents<int>>,
	typename OLayout::mapping<std::extents<int, 1>>>);

    static_assert(has_op_eq<
	typename SLayout::mapping<std::extents<int, 1>>,
	typename OLayout::mapping<std::extents<int, 1>>> == Expected);

    static_assert(has_op_eq<
	typename SLayout::mapping<std::extents<int, 1>>,
	typename OLayout::mapping<std::extents<int, 2>>> == Expected);

    static_assert(!has_op_eq<
	typename SLayout::mapping<std::extents<int, 1>>,
	typename OLayout::mapping<std::extents<int, 1, 2>>>);

    static_assert(has_op_eq<
	typename SLayout::mapping<std::extents<int, 1, 2>>,
	typename OLayout::mapping<std::extents<int, 1, 2>>> == Expected);

    static_assert(has_op_eq<
	typename SLayout::mapping<std::extents<int, 1, 2>>,
	typename OLayout::mapping<std::extents<int, 2, 2>>> == Expected);

    static_assert(!has_op_eq<
	typename SLayout::mapping<std::extents<int, 1, 2>>,
	typename OLayout::mapping<std::extents<int, 1, 2, 3>>>);
  }

constexpr void
test_has_op_eq_peculiar()
{
  static_assert(has_op_eq<
      std::layout_right::mapping<std::extents<int>>,
      std::layout_left::mapping<std::extents<unsigned int>>>);

  static_assert(has_op_eq<
      std::layout_right::mapping<std::extents<int, 1>>,
      std::layout_left::mapping<std::extents<int, dyn>>>);

  static_assert(!has_op_eq<
      std::layout_right::mapping<std::extents<int, 1, 2>>,
      std::layout_left::mapping<std::extents<int, dyn, 2>>>);
}

template<typename Layout>
  constexpr bool
  test_mapping_all()
  {
    test_linear_index_all<Layout, uint8_t>();
    test_linear_index_all<Layout, int>();
    if !consteval
      {
	test_linear_index_all<Layout, IntLike>();
	test_linear_index_all<Layout, MutatingInt>();
	test_linear_index_all<Layout, RValueInt>();
      }

    test_required_span_size_all<Layout>();
    test_stride_all<Layout>();

    test_eq<Layout>();
    test_eq_zero<Layout>();
    return true;
  }

template<typename Layout>
  constexpr void
  test_all()
  {
    static_assert(std::is_trivially_default_constructible_v<Layout>);
    static_assert(std::is_trivially_copyable_v<Layout>);
    static_assert(test_mapping_properties_all<Layout>());

    test_mapping_all<Layout>();
    static_assert(test_mapping_all<Layout>());

    test_has_stride_0d<Layout>();
    test_has_stride_1d<Layout>();
    test_has_stride_2d<Layout>();
    test_has_op_eq<Layout, Layout, true>();
  }

#if __cplusplus > 202302L
template<template<size_t> typename Layout>
  constexpr bool
  test_padded_all()
  {
    test_all<Layout<0>>();
    test_all<Layout<1>>();
    test_all<Layout<2>>();
    test_all<Layout<5>>();
    test_all<Layout<dyn>>();
    return true;
  }

template<template<size_t> typename Layout>
  constexpr bool
  test_padded_has_op_eq()
  {
    using Traits = LayoutTraits<DeducePaddingSide::from_template<Layout>()>;
    test_has_op_eq<typename Traits::layout_same, Layout<0>, false>();
    test_has_op_eq<typename Traits::layout_same, Layout<6>, false>();
    test_has_op_eq<typename Traits::layout_same, Layout<dyn>, false>();
    // The next one looks strange, because it's neither. Somehow, the
    // conversion rules seem to be playing a critical role again.
    // test_has_op_eq<typename Traits::layout_other, Layout<0>, false>();

    test_has_op_eq<Layout<2>, Layout<6>, true>();
    test_has_op_eq<Layout<2>, Layout<dyn>, true>();
    return true;
  }
#endif

int
main()
{
  test_all<std::layout_left>();
  test_all<std::layout_right>();
  test_all<std::layout_stride>();
#if __cplusplus > 202302L
  test_padded_all<std::layout_left_padded>();
  test_padded_all<std::layout_right_padded>();
#endif

  test_has_op_eq<std::layout_right, std::layout_left, false>();
  test_has_op_eq<std::layout_right, std::layout_stride, true>();
  test_has_op_eq<std::layout_left, std::layout_stride, true>();
#if __cplusplus > 202302L
  test_padded_has_op_eq<std::layout_left_padded>();
  test_padded_has_op_eq<std::layout_right_padded>();
#endif

  test_has_op_eq_peculiar();
  return 0;
}
