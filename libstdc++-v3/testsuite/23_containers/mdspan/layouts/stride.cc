// { dg-do run { target c++23 } }
#include <mdspan>

#include "../int_like.h"
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<typename MappingStride>
  constexpr void
  test_ctor_default_stride()
  {
    using Extents = typename MappingStride::extents_type;
    MappingStride actual;
    typename std::layout_right::mapping<Extents> expected;

    constexpr auto rank = MappingStride::extents_type::rank();
    if constexpr (rank > 0)
      for(size_t i = 0; i < rank; ++i)
	VERIFY(actual.stride(i) == expected.stride(i));
  }

constexpr bool
test_ctor_default_stride_all()
{
  test_ctor_default_stride<
    std::layout_stride::mapping<std::extents<int, 3>>>();

  test_ctor_default_stride<
    std::layout_stride::mapping<std::extents<int, 3, 5, 7>>>();

  test_ctor_default_stride<
    std::layout_stride::mapping<std::dextents<int, 3>>>();

  test_ctor_default_stride<
    std::layout_stride::mapping<std::extents<int, 0, 5, 7>>>();

  test_ctor_default_stride<
    std::layout_stride::mapping<std::extents<int, 3, dyn, dyn>>>();

  test_ctor_default_stride<
    std::layout_stride::mapping<std::extents<int, dyn, dyn, 3>>>();
  return true;
}

template<typename E, typename E_arg, typename T, size_t N, bool Expected>
constexpr void
test_stride_constructible()
{
  static_assert(std::is_nothrow_constructible_v<
      std::layout_stride::mapping<E>, E_arg, std::span<T, N>> == Expected);
  static_assert(std::is_nothrow_constructible_v<
      std::layout_stride::mapping<E>, E_arg, std::array<T, N>> == Expected);
  static_assert(!std::is_constructible_v<std::layout_stride::mapping<E>,
					 E_arg>);
}

constexpr void
test_stride_constructible_all()
{
  using E0 = std::extents<int>;
  using E1 = std::extents<int, 2>;
  using E2 = std::extents<int, dyn>;

  test_stride_constructible<E0, E0, int, 0, true>();
  test_stride_constructible<E0, E0, IntLike, 0, true>();
  test_stride_constructible<E0, E0, ThrowingInt, 0, false>();
  test_stride_constructible<E0, E0, MutatingInt, 0, false>();
  test_stride_constructible<E0, E0, NotIntLike, 0, false>();
  test_stride_constructible<E1, E1, int, 1, true>();
  test_stride_constructible<E2, E1, int, 1, true>();
  test_stride_constructible<E1, E1, int, 2, false>();
  test_stride_constructible<E1, E0, int, 1, false>();
}

template<typename Extents, typename Shape>
  constexpr void
  test_ctor_shape_strides(Extents exts, Shape strides)
  {
    using M = std::layout_stride::mapping<Extents>;
    M m(exts, strides);

    if constexpr (Extents::rank() > 0)
      for(size_t i = 0; i < exts.rank(); ++i)
	{
	  VERIFY(m.stride(i) == strides[i]);
	  VERIFY(m.extents().extent(i) == exts.extent(i));
	}
  }

constexpr bool
test_ctor_shape_stride_all()
{
  test_ctor_shape_strides(std::extents<int>{}, std::array<int, 0>{});
  test_ctor_shape_strides(std::extents<int, 2>{}, std::array<int, 1>{3});
  test_ctor_shape_strides(std::extents<int, 2, 4, 6>{},
			  std::array<int, 3>{20, 5, 45});
  return true;
}

template<typename Extents, std::array<bool, 2> Strided,
	 std::array<bool, 2> Unique, std::array<bool, 2> Exhautive,
	 typename Extents::index_type Offset = 0>
  struct MappingLike
  {
    using extents_type = Extents;
    using index_type = typename Extents::index_type;

    constexpr
    MappingLike(extents_type extents,
		std::array<index_type, Extents::rank()> strides)
      : _extents(extents), _strides(strides)
    { }

    static constexpr bool
    is_always_strided() requires (Strided[0])
    { return Strided[1]; }

    static constexpr bool
    is_always_unique() requires (Unique[0])
    { return Unique[1]; }

    static constexpr bool
    is_always_exhaustive() requires (Exhautive[0])
    { return Exhautive[1]; }

    constexpr Extents
    extents() const { return _extents; }

    constexpr index_type
    stride(size_t i) const { return _strides[i]; }

    template<typename... Indices>
      constexpr index_type
      operator()(Indices... indices) const
      {
	if (empty())
	  VERIFY(false);

	std::array<index_type, Extents::rank()> ind_arr{indices...};
	index_type ret = Offset;
	for(size_t i = 0; i < Extents::rank(); ++i)
	  ret += ind_arr[i]*_strides[i];
	return ret;
      }

  private:
    constexpr bool
    empty() const
    {
      for (size_t i = 0; i < extents_type::rank(); ++i)
	if (_extents.extent(i) == 0)
	  return true;
      return false;
    }

    Extents _extents;
    std::array<index_type, Extents::rank()> _strides;
  };


template<size_t Rank>
struct ExtentLike
{
  using index_type = int;

  static constexpr size_t
  rank() { return Rank; }
};


template<typename E1>
constexpr void
test_mapping_like_constructible()
{
  using M = std::layout_stride::mapping<E1>;
  using E2 = std::dextents<typename E1::index_type, E1::rank()>;
  using E3 = std::dextents<typename E1::index_type, E1::rank() + 1>;
  using E4 = ExtentLike<E1::rank()>;

  constexpr auto TT = std::array{true, true};
  constexpr auto FT = std::array{false, true};
  constexpr auto TF = std::array{true, false};

  static_assert(std::is_constructible_v<M, MappingLike<E1, TT, TT, TT>>);
  static_assert(std::is_constructible_v<M, MappingLike<E2, TT, TT, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E3, TT, TT, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E1, FT, TT, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E1, TF, TT, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E1, TT, FT, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E1, TT, TF, TT>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E1, TT, TT, FT>>);
  static_assert(std::is_constructible_v<M, MappingLike<E1, TT, TT, TF>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E4, TT, TT, TF>>);
  static_assert(!std::is_constructible_v<M, MappingLike<E4, TT, TT, TT>>);
}

constexpr void
test_mapping_like_constructible_all()
{
  test_mapping_like_constructible<std::extents<int>>();
  test_mapping_like_constructible<std::extents<int, 2>>();
  test_mapping_like_constructible<std::extents<int, 2, 3>>();
}

template<typename E1, typename E2>
constexpr void
test_mapping_like_convertible()
{
  using M1 = std::layout_stride::mapping<E1>;
  using M2 = std::layout_stride::mapping<E2>;
  constexpr auto TT = std::array{true, true};

  static_assert(!std::is_convertible_v<MappingLike<E1, TT, TT, TT>, M1>);
  static_assert(!std::is_convertible_v<MappingLike<E2, TT, TT, TT>, M1>);
  static_assert(!std::is_convertible_v<MappingLike<E1, TT, TT, TT>, M2>);

  static_assert(std::is_convertible_v<std::layout_stride::mapping<E2>, M1>);
  static_assert(std::is_convertible_v<std::layout_left::mapping<E2>, M1>);
  static_assert(std::is_convertible_v<std::layout_right::mapping<E2>, M1>);

  static_assert(!std::is_convertible_v<std::layout_stride::mapping<E1>, M2>);
  static_assert(!std::is_convertible_v<std::layout_left::mapping<E1>, M2>);
  static_assert(!std::is_convertible_v<std::layout_right::mapping<E1>, M2>);
}

constexpr void
test_mapping_like_convertible_all()
{
  test_mapping_like_convertible<std::extents<unsigned int>,
				std::extents<int>>();
  test_mapping_like_convertible<std::extents<unsigned int, 2>,
				std::extents<int, 2>>();
  test_mapping_like_convertible<std::extents<int, dyn, 3>,
				std::extents<int, 2, 3>>();
}

template<typename Extents>
constexpr void
test_ctor_stride_like(Extents exts, std::array<int, Extents::rank()> strides)
{
  auto other_right = std::layout_right::mapping(exts);
  auto other_left = std::layout_left::mapping(exts);
  auto other_stride = std::layout_stride::mapping(exts, strides);

  VERIFY(std::layout_stride::mapping<Extents>(other_right) == other_right);
  VERIFY(std::layout_stride::mapping<Extents>(other_left) == other_left);
  VERIFY(std::layout_stride::mapping<Extents>(other_stride) == other_stride);
}

constexpr void
test_ctor_stride_like_all()
{
  using E1 = std::extents<int>;
  auto s1 = std::array<int, 0>{};
  test_ctor_stride_like(E1{}, s1);

  using E2 = std::extents<int, 3>;
  auto s2 = std::array<int, 1>{2};
  test_ctor_stride_like(E2{}, s2);

  using E3 = std::extents<int, 3, 5, 7>;
  auto s3 = std::array<int, 3>{5, 1, 15};
  test_ctor_stride_like(E3{}, s3);
}

constexpr bool
test_ctor_strides_all()
{
  test_ctor_default_stride_all();
  test_ctor_shape_stride_all();
  test_ctor_stride_like_all();
  return true;
}

// Check is_exhaustive.
template<typename Extents, typename Strides>
  constexpr void
  test_is_exhaustive(Extents extents, Strides strides, bool expected)
  {
    std::layout_stride::mapping<Extents> m(extents, strides);
    VERIFY(m.is_exhaustive() == expected);

    bool always_exhaustive = extents.rank() == 0 || m.required_span_size() == 0;
    VERIFY(m.is_always_exhaustive() == always_exhaustive);
  }

constexpr void
test_is_exhaustive_zero_1d()
{
  std::extents<int, 0> extents;
  test_is_exhaustive(extents, std::array{1}, true);
  test_is_exhaustive(extents, std::array{2}, true);
}

constexpr void
test_is_exhaustive_zero_3d()
{
  std::extents<int, 3, 0, 7> extents;

  test_is_exhaustive(extents, std::array{1, 1, 1}, true);
  test_is_exhaustive(extents, std::array{1, 2*21, 2*3}, true);
  test_is_exhaustive(extents, std::array{7, 2*21, 1}, true);
  test_is_exhaustive(extents, std::array{1, 21, 3}, true);
  test_is_exhaustive(extents, std::array{7, 21, 1}, true);
}

constexpr void
test_is_exhaustive_0d()
{
  std::extents<int> extents;
  test_is_exhaustive(extents, std::array<int, 0>{}, true);
}

constexpr void
test_is_exhaustive_1d()
{
  std::extents<int, 3> extents;
  test_is_exhaustive(extents, std::array{1}, true);
  test_is_exhaustive(extents, std::array{3}, false);
}


constexpr void
test_is_exhaustive_3d()
{
  std::extents<int, 3, dyn, 7> extents(5);

  test_is_exhaustive(extents, std::array{1, 3, 3*5}, true);
  test_is_exhaustive(extents, std::array{5*7, 1, 5}, true);
  test_is_exhaustive(extents, std::array{7, 3*7, 1}, true);

  test_is_exhaustive(extents, std::array{1, 3, 2*3*5}, false);
  test_is_exhaustive(extents, std::array{2*5*7, 1, 2*5}, false);
  test_is_exhaustive(extents, std::array{2*7, 2*3*7, 2}, false);
}

constexpr void
test_is_exhaustive_ones()
{
  std::extents<int, 1, 1, 3, 1> extents;
  test_is_exhaustive(extents, std::array{1, 1, 1, 1}, true);
  test_is_exhaustive(extents, std::array{1, 1, 1, 3}, true);
  test_is_exhaustive(extents, std::array{3, 3, 1, 3}, true);
  test_is_exhaustive(extents, std::array{3, 1, 1, 3}, true);
}

constexpr bool
test_is_exhaustive_all()
{
  test_is_exhaustive_zero_1d();
  test_is_exhaustive_zero_3d();
  test_is_exhaustive_ones();
  test_is_exhaustive_0d();
  test_is_exhaustive_1d();
  test_is_exhaustive_3d();
  return true;
}

template<typename Extents, int Offset>
  using OffsetMapping = MappingLike<Extents, {true, true}, {true, true},
				    {true, false}, Offset>;

template<typename Extents>
  constexpr void
  test_eq(Extents exts,
      std::array<typename Extents::index_type, Extents::rank()> left_strides,
      std::array<typename Extents::index_type, Extents::rank()> right_strides,
      std::array<typename Extents::index_type, Extents::rank()> padded_strides)
  {
    using DExtents = std::dextents<int, Extents::rank()>;

    std::layout_left::mapping<Extents> ml;
    std::layout_right::mapping<DExtents> mr(exts);

    std::layout_stride::mapping<Extents> msd;
    std::layout_stride::mapping<Extents> msl(exts, left_strides);
    std::layout_stride::mapping<Extents> msr(exts, right_strides);
    std::layout_stride::mapping<Extents> msp(exts, padded_strides);

    OffsetMapping<Extents, 0> mor{exts, right_strides};
    OffsetMapping<Extents, 0> mol{exts, left_strides};
    OffsetMapping<Extents, 0> mop{exts, padded_strides};
    OffsetMapping<Extents, 1> moo{exts, right_strides};

    VERIFY(msd == mr);
    VERIFY(msd == mor);
    VERIFY(msd != msp);
    VERIFY(msd != mop);

    VERIFY(msl == ml);
    VERIFY(msl == mol);
    VERIFY(msd != msp);
    VERIFY(msl != mop);

    VERIFY(msp == mop);
    VERIFY(msp != ml);
    VERIFY(msp != mr);

    VERIFY(msd != moo);
  }

constexpr void
test_eq_0d()
{
  using Extents = std::extents<int>;
  Extents exts;
  std::layout_left::mapping<Extents> ml;
  std::layout_right::mapping<Extents> mr;
  std::layout_stride::mapping<Extents> ms;
  OffsetMapping<Extents, 0> mor{exts, {}};
  OffsetMapping<Extents, 1> moo{exts, {}};

  VERIFY(ms == ml);
  VERIFY(ms == mr);
  VERIFY(ms == mor);
  VERIFY(ms != moo);
}

constexpr void
test_eq_1d()
{
  using Extents = std::extents<int, 2>;
  auto exhaustive_strides = std::array{1};
  auto padded_strides = std::array{2};

  test_eq(Extents{}, exhaustive_strides, exhaustive_strides, padded_strides);
}

constexpr void
test_eq_2d()
{
  using Extents = std::extents<int, 1, 2>;
  auto left_strides = std::array{1, 1};
  auto right_strides = std::array{2, 1};
  auto padded_strides = std::array{2, 8};

  test_eq(Extents{}, left_strides, right_strides, padded_strides);
}

constexpr void
test_eq_zero()
{
  using Extents = std::extents<int, 0, 2>;
  using Mapping = std::layout_stride::mapping<Extents>;

  Extents exts;
  std::array<int, 2> sl{1, 5};
  std::array<int, 2> sr{5, 1};

  Mapping m1(exts, sl);
  Mapping m2(exts, sl);
  Mapping m3(exts, sr);
  OffsetMapping<Extents, 0> m4(exts, sl);

  VERIFY(m1 == m2);
  VERIFY(m1 != m3);
  VERIFY(m1 == m4);

}

constexpr bool
test_eq_all()
{
  test_eq_0d();
  test_eq_1d();
  test_eq_2d();
  test_eq_zero();
  return true;
}

template<typename M1, typename M2>
  concept has_op_eq = requires (M1 m1, M2 m2)
  {
    { m1 == m2 } -> std::same_as<bool>;
    { m2 == m1 } -> std::same_as<bool>;
    { m1 != m2 } -> std::same_as<bool>;
    { m2 != m1 } -> std::same_as<bool>;
  };

constexpr void
test_has_op_eq()
{
  using E1 = std::extents<int>;
  using E2 = std::extents<int, 2>;
  using E3 = std::extents<int, 1, 2>;
  constexpr auto FT = std::array{false, true};

  static_assert(!has_op_eq<
      std::layout_stride::mapping<E1>, MappingLike<E1, FT, FT, FT>>);

  static_assert(!has_op_eq<
      std::layout_stride::mapping<E2>, MappingLike<E2, FT, FT, FT>>);

  static_assert(!has_op_eq<
      std::layout_stride::mapping<E3>, MappingLike<E3, FT, FT, FT>>);
}

int
main()
{
  test_ctor_strides_all();
  static_assert(test_ctor_strides_all());
  test_mapping_like_convertible_all();
  test_mapping_like_constructible_all();
  test_stride_constructible_all();
  test_is_exhaustive_all();
  static_assert(test_is_exhaustive_all());
  test_eq_all();
  static_assert(test_eq_all());
  test_has_op_eq();
  return 0;
}
