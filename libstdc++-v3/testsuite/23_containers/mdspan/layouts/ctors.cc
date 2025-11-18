// { dg-do run { target c++23 } }
#include <mdspan>

#include "../layout_traits.h"
#include <cstdint>
#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<typename Mapping, typename IndexType, size_t... Extents>
  constexpr void
  verify(std::extents<IndexType, Extents...> oexts)
  {
    auto m = Mapping(oexts);
    VERIFY(m.extents() == oexts);
  }

template<typename Mapping, typename OMapping>
  requires (requires { typename OMapping::layout_type; })
  constexpr void
  verify(OMapping other)
  {
    constexpr auto rank = Mapping::extents_type::rank();
    auto m = Mapping(other);
    VERIFY(m.extents() == other.extents());
    if constexpr (rank > 0)
      for(size_t i = 0; i < rank; ++i)
	VERIFY(std::cmp_equal(m.stride(i), other.stride(i)));
  }

template<typename To, typename From>
  constexpr void
  verify_convertible(From from)
  {
    static_assert(std::is_convertible_v<From, To>);
    verify<To>(from);
  }

template<typename To, typename From>
  constexpr void
  verify_nothrow_convertible(From from)
  {
    if constexpr (is_padded_layout<typename To::layout_type>)
      static_assert(std::is_constructible_v<To, From>);
    else
      static_assert(std::is_nothrow_constructible_v<To, From>);
    verify_convertible<To>(from);
  }

template<typename To, typename From>
  constexpr void
  verify_constructible(From from)
  {
    static_assert(!std::is_convertible_v<From, To>);
    static_assert(std::is_constructible_v<To, From>);
    verify<To>(from);
  }

template<typename To, typename From>
  constexpr void
  verify_nothrow_constructible(From from)
  {
    if constexpr (is_padded_layout<typename To::layout_type>)
      static_assert(std::is_constructible_v<To, From>);
    else
      static_assert(std::is_nothrow_constructible_v<To, From>);
    verify_constructible<To>(from);
  }

template<typename Mapping, typename OExtents>
  constexpr void
  assert_not_constructible()
  {
    static_assert(!std::is_constructible_v<Mapping, OExtents>);
  }

// ctor: mapping()
namespace default_ctor
{
  template<typename Layout, typename Extents>
    constexpr void
    test_default_ctor()
    {
      using Mapping = typename Layout::mapping<Extents>;

      Mapping m;
      for(size_t i = 0; i < Extents::rank(); ++i)
	if (Extents::static_extent(i) == std::dynamic_extent)
	  VERIFY(m.extents().extent(i) == 0);
	else
	  VERIFY(m.extents().static_extent(i) == Extents::static_extent(i));
    }

  template<typename Layout>
    constexpr bool
    test_default_ctor_all()
    {
      test_default_ctor<Layout, std::extents<int, dyn>>();
      test_default_ctor<Layout, std::extents<int, 1, 2>>();
      test_default_ctor<Layout, std::extents<int, dyn, 2>>();
      test_default_ctor<Layout, std::extents<int, dyn, dyn>>();
      test_default_ctor<Layout, std::extents<int, dyn, 2, dyn>>();
      test_default_ctor<Layout, std::extents<int, dyn, dyn, dyn>>();
      return true;
    }

  template<typename Layout>
  constexpr void
  test_all()
  {
    test_default_ctor_all<Layout>();
    static_assert(test_default_ctor_all<Layout>());
  }
}

// ctor: mapping(const extents&)
namespace from_extents
{
  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_convertible(OExtents oexts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      ::verify_nothrow_convertible<Mapping>(oexts);
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_constructible(OExtents oexts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      ::verify_nothrow_constructible<Mapping>(oexts);
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    assert_not_constructible()
    {
      using Mapping = typename Layout::mapping<Extents>;
      ::assert_not_constructible<Mapping, OExtents>();
    }

  template<typename Layout>
    constexpr bool
    test_ctor()
    {
      verify_nothrow_convertible<Layout, std::extents<int>>(
	std::extents<int>{});

      verify_nothrow_convertible<Layout, std::extents<int, 2>>(
	std::extents<int, 2>{});

      verify_nothrow_convertible<Layout, std::extents<int, dyn, 3>>(
	std::extents<int, dyn, 3>{2});

      verify_nothrow_constructible<Layout, std::extents<unsigned int>>(
	std::extents<int>{});

      verify_nothrow_constructible<Layout, std::extents<int, dyn>>(
	std::extents<int, 2>{});

      verify_nothrow_constructible<Layout, std::extents<int, dyn, 3>>(
	std::extents<int, 2, 3>{});

      assert_not_constructible<Layout, std::extents<int>,
			       std::extents<unsigned int>>();
      assert_not_constructible<Layout, std::extents<int, 2>,
			       std::extents<int, dyn>>();
      assert_not_constructible<Layout, std::extents<int, 2, 3>,
			       std::extents<int, dyn, 3>>();
      return true;
    }

  template<typename Layout, typename Extents>
    constexpr void
    assert_deducible(Extents exts)
    {
      typename Layout::mapping m(exts);
      static_assert(std::same_as<decltype(m),
		    typename Layout::mapping<Extents>>);
    }

  template<typename Layout>
    constexpr void
    test_deducible()
    {
      assert_deducible<Layout>(std::extents<int>());
      assert_deducible<Layout>(std::extents<int, 1>());
      assert_deducible<Layout>(std::extents<int, 1, 2, dyn>(3));
    }

  template<typename Layout>
    constexpr void
    test_all()
    {
      test_ctor<Layout>();
      static_assert(test_ctor<Layout>());
      test_deducible<Layout>();
    }
}

// ctor: mapping(mapping<OExtents>)
namespace from_same_layout
{
  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_convertible(OExtents exts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      using OMapping = typename Layout::mapping<OExtents>;

      ::verify_convertible<Mapping>(OMapping(exts));
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_convertible(OExtents exts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      using OMapping = typename Layout::mapping<OExtents>;

      ::verify_nothrow_convertible<Mapping>(OMapping(exts));
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_constructible(OExtents exts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      using OMapping = typename Layout::mapping<OExtents>;

      ::verify_nothrow_constructible<Mapping>(OMapping(exts));
    }

  template<typename Layout>
    constexpr bool
    test_ctor()
    {
      verify_nothrow_convertible<Layout, std::extents<unsigned int>>(
	std::extents<int>{});

      if constexpr (!is_padded_layout<Layout>)
	verify_nothrow_constructible<Layout, std::extents<int>>(
	  std::extents<unsigned int>{});
      else
	verify_convertible<Layout, std::extents<int>>(
	  std::extents<unsigned int>{});

      assert_not_constructible<
	typename Layout::mapping<std::extents<int>>,
	typename Layout::mapping<std::extents<int, 1>>>();

      assert_not_constructible<
	typename Layout::mapping<std::extents<int, 1>>,
	typename Layout::mapping<std::extents<int>>>();

      if constexpr (!is_padded_layout<Layout>)
	verify_nothrow_constructible<Layout, std::extents<int, 1>>(
	  std::extents<int, dyn>{1});
      else
	verify_convertible<Layout, std::extents<int, 1>>(
	  std::extents<int, dyn>{1});

      verify_nothrow_convertible<Layout, std::extents<int, dyn>>(
	std::extents<int, 1>{});

      assert_not_constructible<
	typename Layout::mapping<std::extents<int, 1, 2>>,
	typename Layout::mapping<std::extents<int, 1>>>();

      verify_nothrow_constructible<Layout, std::extents<int, 1, 2>>(
	std::extents<int, dyn, 2>{1});

      if constexpr (!is_padded_layout<Layout>)
	verify_nothrow_convertible<Layout, std::extents<int, dyn, 2>>(
	  std::extents<int, 1, 2>{});
      else
	verify_nothrow_constructible<Layout, std::extents<int, dyn, 2>>(
	  std::extents<int, 1, 2>{});
      return true;
    }

  template<typename Layout>
    constexpr void
    test_all()
    {
      test_ctor<Layout>();
      static_assert(test_ctor<Layout>());
    }
}

// ctor: mapping(layout_{right,left}::mapping<OExtents>)
namespace from_left_or_right
{
  template<typename SLayout, typename OLayout, typename SExtents,
	   typename OExtents>
    constexpr void
    verify_ctor(OExtents oexts)
    {
      using SMapping = typename SLayout::mapping<SExtents>;
      using OMapping = typename OLayout::mapping<OExtents>;

      constexpr bool expected = std::is_convertible_v<OExtents, SExtents>;
      if constexpr (expected)
	verify_nothrow_convertible<SMapping>(OMapping(oexts));
      else
	verify_nothrow_constructible<SMapping>(OMapping(oexts));
    }

  template<typename SLayout, typename OLayout>
    constexpr bool
    test_ctor()
    {
      assert_not_constructible<
	typename SLayout::mapping<std::extents<int>>,
	typename OLayout::mapping<std::extents<int, 1>>>();

      verify_ctor<OLayout, SLayout, std::extents<int>>(
	std::extents<unsigned int>{});

      verify_ctor<OLayout, SLayout, std::extents<unsigned int>>(
	std::extents<int>{});

      assert_not_constructible<
	typename SLayout::mapping<std::extents<int, 1>>,
	typename OLayout::mapping<std::extents<int>>>();

      verify_ctor<OLayout, SLayout, std::extents<int, 1>>(
	std::extents<int, 1>{});

      verify_ctor<OLayout, SLayout, std::extents<int, 1>>(
	std::extents<unsigned int, 1>{});

      verify_ctor<OLayout, SLayout, std::extents<unsigned int, 1>>(
	std::extents<int, 1>{});

      assert_not_constructible<
	typename SLayout::mapping<std::extents<int, 1, 2>>,
	typename OLayout::mapping<std::extents<int, 1, 2>>>();
      return true;
    }

  template<typename SLayout, typename OLayout>
    constexpr void
    test_all()
    {
      test_ctor<SLayout, OLayout>();
      static_assert(test_ctor<SLayout, OLayout>());
    }
}

// ctor: mapping(layout_stride::mapping<OExtents>)
namespace from_stride
{
  template<typename Mapping>
    constexpr auto
    strides(Mapping m)
    {
      constexpr auto rank = Mapping::extents_type::rank();
      std::array<typename Mapping::index_type, rank> s;

      if constexpr (rank > 0)
	for(size_t i = 0; i < rank; ++i)
	  s[i] = m.stride(i);
      return s;
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_convertible(OExtents oexts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      using OMapping = std::layout_stride::mapping<OExtents>;

      constexpr auto other = OMapping(oexts, strides(Mapping(Extents(oexts))));
      ::verify_nothrow_convertible<Mapping>(other);
    }

  template<typename Layout, typename Extents, typename OExtents>
    constexpr void
    verify_nothrow_constructible(OExtents oexts)
    {
      using Mapping = typename Layout::mapping<Extents>;
      using OMapping = std::layout_stride::mapping<OExtents>;

      constexpr auto other = OMapping(oexts, strides(Mapping(Extents(oexts))));
      ::verify_nothrow_constructible<Mapping>(other);
    }

  template<typename Layout>
    constexpr bool
    test_ctor()
    {
      assert_not_constructible<
	typename Layout::mapping<std::extents<int>>,
	std::layout_stride::mapping<std::extents<int, 1>>>();

      assert_not_constructible<
	typename Layout::mapping<std::extents<int, 1>>,
	std::layout_stride::mapping<std::extents<int>>>();

      assert_not_constructible<
	typename Layout::mapping<std::extents<int, 2>>,
	std::layout_stride::mapping<std::extents<int, 1>>>();

      verify_nothrow_convertible<Layout, std::extents<int>>(
	std::extents<int>{});

      verify_nothrow_convertible<Layout, std::extents<unsigned int>>(
	std::extents<int>{});

      // Rank ==  0 doesn't check IndexType for convertibility.
      verify_nothrow_convertible<Layout, std::extents<int>>(
	std::extents<unsigned int>{});

      verify_nothrow_constructible<Layout, std::extents<int, 3>>(
	std::extents<int, 3>{});

      verify_nothrow_constructible<Layout, std::extents<unsigned int, 3>>(
	std::extents<int, 3>{});

      verify_nothrow_constructible<Layout, std::extents<int, 3>>(
	std::extents<unsigned int, 3>{});

      verify_nothrow_constructible<Layout, std::extents<int, 3, 5>>(
	std::extents<int, 3, 5>{});

      verify_nothrow_constructible<Layout, std::extents<unsigned int, 3, 5>>(
	std::extents<int, 3, 5>{});

      verify_nothrow_constructible<Layout, std::extents<int, 3, 5>>(
	std::extents<unsigned int, 3, 5>{});
      return true;
    }

  template<typename Layout>
    constexpr void
    test_all()
    {
      test_ctor<Layout>();
      static_assert(test_ctor<Layout>());
    }
}

template<typename Layout>
  constexpr void
  test_all()
  {
    default_ctor::test_all<Layout>();
    from_extents::test_all<Layout>();
    from_same_layout::test_all<Layout>();
    from_stride::test_all<Layout>();
  }

template<template<size_t> typename Layout>
  constexpr void
  test_padded_all()
  {
    test_all<Layout<0>>();
    test_all<Layout<1>>();
    test_all<Layout<2>>();
    test_all<Layout<dyn>>();
  }

int
main()
{
  test_all<std::layout_left>();
  test_all<std::layout_right>();
#if __cplusplus > 202302L
  test_padded_all<std::layout_left_padded>();
  test_padded_all<std::layout_right_padded>();
#endif

  from_left_or_right::test_all<std::layout_left, std::layout_right>();
  from_left_or_right::test_all<std::layout_right, std::layout_left>();
  return 0;
}
