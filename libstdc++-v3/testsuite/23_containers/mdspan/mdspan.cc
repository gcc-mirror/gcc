// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>
#include "int_like.h"
#include "layout_like.h"
#include <stdexcept>

constexpr auto dyn = std::dynamic_extent;

template<typename MDSpan, typename T, typename E, typename L = std::layout_right,
	 typename A = std::default_accessor<T>>
  constexpr void
  assert_typedefs()
  {
    static_assert(std::same_as<typename MDSpan::extents_type, E>);
    static_assert(std::same_as<typename MDSpan::layout_type, L>);
    static_assert(std::same_as<typename MDSpan::accessor_type, A>);
    static_assert(std::same_as<typename MDSpan::mapping_type,
			       typename L::mapping<E>>);
    static_assert(std::same_as<typename MDSpan::element_type, T>);
    static_assert(std::same_as<typename MDSpan::value_type,
			       std::remove_const_t<T>>);
    static_assert(std::same_as<typename MDSpan::index_type,
			       typename E::index_type>);
    static_assert(std::same_as<typename MDSpan::size_type,
			       typename E::size_type>);
    static_assert(std::same_as<typename MDSpan::rank_type,
			       typename E::rank_type>);
    static_assert(std::same_as<typename MDSpan::data_handle_type,
			       typename A::data_handle_type>);
    static_assert(std::same_as<typename MDSpan::reference,
			       typename A::reference>);
  }

template<typename T, typename E, typename L, template<typename U> typename A>
  constexpr void
  test_typedefs()
  { assert_typedefs<std::mdspan<T, E, L, A<T>>, T, E, L, A<T>>(); }

constexpr void
test_typedefs_all()
{
  using E = std::extents<int, 1, 2>;
  using L = std::layout_left;

  test_typedefs<double, E, L, std::default_accessor>();
  test_typedefs<const double, E, L, std::default_accessor>();
}

template<typename MDSpan>
  constexpr void
  test_rank()
  {
    using Extents = typename MDSpan::extents_type;
    static_assert(MDSpan::rank() == Extents::rank());
    static_assert(MDSpan::rank_dynamic() == Extents::rank_dynamic());
  }

constexpr bool
test_rank_all()
{
  test_rank<std::mdspan<double, std::extents<int>>>();
  test_rank<std::mdspan<double, std::extents<int, 1>>>();
  test_rank<std::mdspan<double, std::extents<int, dyn>>>();
  return true;
}

template<typename Extents>
  constexpr void
  test_extent(Extents exts)
  {
    double data =  1.0;
    auto md = std::mdspan(&data, exts);
    using MDSpan = decltype(md);

    for(size_t i = 0; i < MDSpan::rank(); ++i)
      {
	VERIFY(MDSpan::static_extent(i) == Extents::static_extent(i));
	VERIFY(md.extent(i) == exts.extent(i));
      }
  }

constexpr bool
test_extent_all()
{
  // For rank == 0, check existence of the methods without calling them.
  test_extent(std::extents<int>{});
  test_extent(std::extents<int, 0>{});
  test_extent(std::extents<int, dyn>{});
  return true;
}

template<typename MDSpan>
  constexpr void
  test_class_properties()
  {
    static_assert(std::copyable<MDSpan>);
    static_assert(std::is_nothrow_move_constructible_v<MDSpan>);
    static_assert(std::is_nothrow_move_assignable_v<MDSpan>);
    static_assert(std::is_nothrow_swappable_v<MDSpan>);
    constexpr bool trivially_copyable =
      std::is_trivially_copyable_v<typename MDSpan::accessor_type>
      && std::is_trivially_copyable_v<typename MDSpan::mapping_type>
      && std::is_trivially_copyable_v<typename MDSpan::data_handle_type>;
    static_assert(std::is_trivially_copyable_v<MDSpan> == trivially_copyable);
  }

constexpr bool
test_class_properties_all()
{
  test_class_properties<std::mdspan<double, std::extents<int>>>();
  test_class_properties<std::mdspan<double, std::extents<int, 1>>>();
  test_class_properties<std::mdspan<double, std::extents<int, dyn>>>();
  return true;
}

template<typename T>
  class ThrowingDefaultAccessor
  {
  public:
    using element_type = T;
    using reference = element_type&;
    using data_handle_type = element_type*;
    using offset_policy = ThrowingDefaultAccessor;

    ThrowingDefaultAccessor() noexcept(false)
    { }

    reference
    access(data_handle_type p, size_t i) const
    { return p[i]; }

    typename offset_policy::data_handle_type
    offset(data_handle_type p, size_t i) const
    { return p + i; }
  };

constexpr bool
test_default_ctor()
{
  static_assert(!std::is_default_constructible_v<std::mdspan<double,
						 std::extents<int>>>);
  static_assert(!std::is_default_constructible_v<std::mdspan<double,
						 std::extents<int, 1>>>);
  static_assert(std::is_default_constructible_v<std::mdspan<double,
						std::extents<int, dyn>>>);

  std::mdspan<double, std::extents<int, dyn>> md;
  VERIFY(md.data_handle() == nullptr);
  VERIFY(md.empty());
  return true;
}

template<template<typename T> typename Accessor, bool Expected>
  constexpr void
  test_nothrow_default_ctor()
  {
    using Extents = std::extents<int, dyn>;
    using Layout = std::layout_left;
    using MDSpan = std::mdspan<double, Extents, Layout, Accessor<double>>;

    static_assert(std::is_default_constructible_v<MDSpan>);
    static_assert(std::is_nothrow_default_constructible_v<MDSpan> == Expected);
  }

constexpr bool
test_from_other()
{
  using Extents = std::extents<int, 3, 5, 7>;
  auto exts = Extents{};

  auto mapping = std::layout_right::mapping(exts);
  constexpr size_t n = mapping.required_span_size();
  std::array<double, n> storage{};

  auto md1 = std::mdspan(storage.data(), exts);
  auto md2 = std::mdspan<double, std::dextents<int, 3>>(md1);

  VERIFY(md1.data_handle() == md2.data_handle());
  VERIFY(md1.size() == md2.size());

  static_assert(!std::is_convertible_v<
      std::mdspan<double, std::extents<unsigned int, 2>>,
      std::mdspan<double, std::extents<int, 2>>>);

  static_assert(std::is_convertible_v<
      std::mdspan<double, std::extents<int, 2>>,
      std::mdspan<const double, std::extents<int, 2>>>);

  static_assert(!std::is_constructible_v<
      std::mdspan<double, std::extents<int, 2>>,
      std::mdspan<const double, std::extents<int, 2>>>);

  return true;
}

template<typename T, typename E, typename L = std::layout_right,
	 typename A = std::default_accessor<T>>
  constexpr void
  assert_deduced_typedefs(auto md)
  { assert_typedefs<decltype(md), T, E, L, A>(); }

constexpr bool
test_from_carray()
{
  constexpr size_t n = 5;
  double data[n] = {1.1, 2.2, 3.3, 4.4, 5.5};

  auto md = std::mdspan(data);
  assert_deduced_typedefs<double, std::extents<size_t, n>>(md);
  VERIFY(md.rank() == 1);
  VERIFY(md.rank_dynamic() == 0);
  VERIFY(md[2] == data[2]);
  return true;
}

constexpr bool
test_from_pointer()
{
  double value = 12.3;
  auto md = std::mdspan(&value);
  assert_deduced_typedefs<double, std::extents<size_t>>(md);
  VERIFY(md.rank() == 0);
  VERIFY(md.rank_dynamic() == 0);
  VERIFY(md[] == value);
  return true;
}

constexpr bool
test_from_pointer_and_shape()
{
  constexpr size_t n = 6;
  std::array<double, n> data{1.1, 2.2, 3.3, 4.4, 5.5, 6.6};
  std::array<int, 2> shape{2, 3};
  std::span<const int, 2> shape_view(shape);

  auto verify = [&data](auto md)
  {
    assert_deduced_typedefs<double, std::dextents<size_t, 2>>(md);
    VERIFY(md.rank() == 2);
    VERIFY(md.rank_dynamic() == 2);
    VERIFY(md[0, 0] == data[0]);
    VERIFY(md[0, 1] == data[1]);
    VERIFY(md[1, 0] == data[3]);
  };

  verify(std::mdspan(data.data(), shape[0], shape[1]));
  verify(std::mdspan(data.data(), shape));
  verify(std::mdspan(data.data(), shape_view));

  std::mdspan<double, std::dextents<size_t, 2>> md1 = {data.data(), shape};
  verify(md1);

  std::mdspan<double, std::dextents<size_t, 2>> md2 = {data.data(), shape_view};
  verify(md2);

  static_assert(std::is_constructible_v<
      std::mdspan<float, std::extents<int, 3, 5>>, float*>);
  static_assert(!std::is_constructible_v<
      std::mdspan<float, std::extents<int, 3, 5>>, float*, int>);
  static_assert(std::is_constructible_v<
      std::mdspan<float, std::extents<int, 3, 5>>, float*, int, int>);
  static_assert(std::is_constructible_v<
      std::mdspan<float, std::extents<int, 3, 5>>, float*, std::span<int, 0>>);
  static_assert(std::is_constructible_v<
      std::mdspan<float, std::extents<int, 3, 5>>, float*, std::span<int, 2>>);
  static_assert(!std::is_convertible_v<
      float*, std::mdspan<float, std::extents<int, 3, 5>>>);

  static_assert(std::is_constructible_v<
      std::mdspan<float, std::dextents<int, 2>>, float*, std::span<int, 2>>);
  static_assert(!std::is_constructible_v<
      std::mdspan<float, std::dextents<int, 2>>, float*, std::span<int, 1>>);
  static_assert(!std::is_constructible_v<
      std::mdspan<float, std::dextents<int, 2>>, float*, std::span<int, 3>>);
  static_assert(!std::is_constructible_v<
      std::mdspan<float, std::dextents<int, 2>>, float*, std::span<int, dyn>>);
  return true;
}

constexpr bool
test_from_pointer_and_constant()
{
  std::array<double, 6> buffer{};
  double * ptr = buffer.data();

  auto verify = [ptr](auto actual, auto exts)
    {
      auto expected = std::mdspan<double, decltype(exts)>(ptr, exts);
      static_assert(std::same_as<decltype(actual), decltype(expected)>);
      VERIFY(actual.extents() == expected.extents());
    };

  auto i3 = std::integral_constant<int, 3>{};
  auto i6 = std::integral_constant<int, 6>{};

  verify(std::mdspan(ptr, 6), std::extents(6));
  verify(std::mdspan(ptr, i6), std::extents(i6));
  verify(std::mdspan(ptr, 2, i3), std::extents(2, i3));
  verify(std::mdspan(ptr, std::true_type{}, i3), std::extents(1, i3));

#if __glibcxx_constant_wrapper
  auto c3 = std::constant_wrapper<3>{};
  verify(std::mdspan(ptr, 2, c3), std::extents(2, i3));
  verify(std::mdspan(ptr, 2, std::cw<3>), std::extents(2, i3));
  verify(std::mdspan(ptr, std::cw<true>, std::cw<3>), std::extents(1, i3));
#endif
  return true;
}

constexpr bool
test_from_extents()
{
  constexpr size_t n = 3*5*7;
  std::array<double, n> storage{};
  using Extents = std::extents<int, 3, 5, 7>;
  auto exts = Extents{};
  auto md = std::mdspan(storage.data(), exts);

  assert_deduced_typedefs<double, Extents>(md);
  VERIFY(md.data_handle() == storage.data());
  VERIFY(md.extents() == exts);
  return true;
}

constexpr bool
test_from_mapping()
{
  constexpr size_t n = 3*5*7;
  std::array<double, n> storage{};
  using Extents = std::extents<int, 3, 5, 7>;

  auto exts = Extents{};
  auto m = std::layout_left::mapping(exts);
  auto md = std::mdspan(storage.data(), m);

  assert_deduced_typedefs<double, Extents, std::layout_left>(md);
  VERIFY(md.data_handle() == storage.data());
  VERIFY(md.mapping() == m);
  return true;
}

constexpr bool
test_from_accessor()
{
  constexpr size_t n = 3*5*7;
  std::array<double, n> storage{};
  using Extents = std::extents<int, 3, 5, 7>;

  auto exts = Extents{};
  auto m = std::layout_left::mapping(exts);
  auto a = std::default_accessor<double>{};
  auto md = std::mdspan(storage.data(), m, a);

  assert_deduced_typedefs<double, Extents, std::layout_left>(md);
  VERIFY(md.data_handle() == storage.data());
  VERIFY(md.mapping() == m);
  return true;
}

template<typename MDSpan, typename Pointer, typename... Ints>
  concept has_pack_ctor = requires
  {
    { MDSpan(Pointer{}, Ints(0)...) } -> std::same_as<MDSpan>;
  };

template<typename CustomInt, bool ValidForPacks, bool ValidForArrays>
  constexpr bool
  test_from_int_like()
  {
    constexpr size_t n = 3*5*7;
    std::array<double, n> storage{};

    auto verify = [&](auto md)
      {
	VERIFY(md.data_handle() == storage.data());
	VERIFY(md.extent(0) == 3);
	VERIFY(md.extent(1) == 5);
	VERIFY(md.extent(2) == 7);
      };

    static_assert(has_pack_ctor<std::mdspan<float, std::dextents<int, 3>>,
	float*, CustomInt, int, CustomInt> == ValidForPacks);

    static_assert(std::is_constructible_v<
	std::mdspan<float, std::dextents<int, 3>>, float*,
	std::span<CustomInt, 3>> == ValidForArrays);

    static_assert(std::is_constructible_v<
	std::mdspan<float, std::dextents<int, 3>>, float*,
	std::array<CustomInt, 3>> == ValidForArrays);

    if constexpr (ValidForPacks)
      verify(std::mdspan(storage.data(), CustomInt(3), 5, CustomInt(7)));

    if constexpr (ValidForArrays)
      {
	auto shape = std::array{CustomInt(3), CustomInt(5), CustomInt(7)};
	auto shape_view = std::span<CustomInt, 3>{shape};
	verify(std::mdspan(storage.data(), shape));
	verify(std::mdspan(storage.data(), shape_view));
      }
    return true;
  }

template<typename T, bool NothrowConstructible = true,
	 bool NothrowAssignable = true>
  class OpaqueAccessor
  {
    struct Handle
    {
      constexpr
      Handle(T * other)
      : ptr(other)
      { }

      constexpr
      Handle(const Handle&) noexcept(NothrowConstructible) = default;

      constexpr
      Handle(Handle&&) noexcept(NothrowConstructible) = default;

      constexpr Handle&
      operator=(const Handle&) noexcept(NothrowAssignable) = default;

      constexpr Handle&
      operator=(Handle&&) noexcept(NothrowAssignable) = default;

      T * ptr;
    };

  public:
    using element_type = T;
    using reference = T&;
    using data_handle_type = Handle;
    using offset_policy = OpaqueAccessor;

    reference
    access(data_handle_type p, size_t i) const
    {
      ++access_count;
      return p.ptr[i];
    }

    typename offset_policy::data_handle_type
    offset(data_handle_type p, size_t i) const
    {
      ++offset_count;
      return typename offset_policy::data_handle_type{(void*)(p.ptr + i)};
    }

    mutable size_t access_count = 0;
    mutable size_t offset_count = 0;
  };

void
test_from_opaque_accessor()
{
  constexpr size_t n = 3*5*7;
  std::array<double, n> storage{};
  using Extents = std::extents<int, 3, 5, 7>;

  auto exts = Extents{};
  auto m = std::layout_left::mapping(exts);
  auto a = OpaqueAccessor<double>{};
  auto handle = OpaqueAccessor<double>::data_handle_type{storage.data()};
  auto md = std::mdspan(handle, m, a);

  using MDSpan = decltype(md);
  static_assert(std::same_as<MDSpan::accessor_type, decltype(a)>);

  VERIFY(md[0, 0, 0] == 0.0);
  VERIFY(md.accessor().access_count == 1);

  VERIFY(md[2, 4, 6] == 0.0);
  VERIFY(md.accessor().access_count == 2);
}

template<typename T, typename Base>
  class BaseClassAccessor
  {
  public:
    using element_type = T;
    using reference = Base&;
    using data_handle_type = T*;
    using offset_policy = BaseClassAccessor;

    static_assert(std::common_reference_with<reference&&, element_type&>);

    reference
    access(data_handle_type p, size_t i) const
    { return p[i]; }

    typename offset_policy::data_handle_type
    offset(data_handle_type p, size_t i) const
    { return typename offset_policy::data_handle_type{p + i}; }
  };

struct Base
{
  double value = 1.0;
};

struct Derived : Base
{
  double value = 2.0;
};

void
test_from_base_class_accessor()
{
  constexpr size_t n = 3*5*7;
  std::array<Derived, n> storage{};
  using Extents = std::extents<int, 3, 5, 7>;

  auto exts = Extents{};
  auto m = std::layout_left::mapping(exts);
  auto a = BaseClassAccessor<Derived, Base>{};
  auto md = std::mdspan(storage.data(), m, a);

  using MDSpan = decltype(md);
  static_assert(std::same_as<MDSpan::accessor_type, decltype(a)>);
  static_assert(std::same_as<decltype(md[0, 0, 0]), Base&>);
  VERIFY(md[0, 0, 0].value == 1.0);
  VERIFY(md[2, 4, 6].value == 1.0);
}

constexpr bool
test_from_mapping_like()
{
  double data = 1.1;
  auto m = LayoutLike::mapping<std::extents<int, 1, 2, 3>>{};
  auto md = std::mdspan(&data, m);
  VERIFY(md[0, 0, 0] == data);
  VERIFY(md[0, 1, 2] == data);
  return true;
}

template<typename MDSpan>
  constexpr void
  test_empty(MDSpan md)
  {
    VERIFY(md.empty() == (md.size() == 0));
  }

constexpr bool
test_empty_all()
{
  test_empty(std::mdspan<double, std::extents<int, dyn>>{});
  return true;
}

template<typename MDSpan, typename... Args>
concept indexable = requires (MDSpan md, Args... args)
{
  { md[args...] } -> std::same_as<typename MDSpan::reference>;
};

template<typename Int, bool ValidForPacks, bool ValidForArrays>
  constexpr bool
  test_access()
  {
    using Extents = std::extents<int, 3, 5, 7>;
    auto exts = Extents{};

    auto mapping = std::layout_left::mapping(exts);
    constexpr size_t n = mapping.required_span_size();
    std::array<double, n> storage{};

    auto md = std::mdspan(storage.data(), mapping);
    using MDSpan = decltype(md);

    for(int i = 0; i < exts.extent(0); ++i)
      for(int j = 0; j < exts.extent(1); ++j)
	for(int k = 0; k < exts.extent(2); ++k)
	  {
	    storage[mapping(i, j, k)] = 1.0;
	    if constexpr (ValidForPacks)
	      VERIFY(md[Int(i), Int(j), Int(k)] == 1.0);

	    if constexpr (ValidForArrays)
	      {
		std::array<Int, 3> ijk{Int(i), Int(j), Int(k)};
		VERIFY(md[ijk] == 1.0);
		VERIFY(md[std::span(ijk)] == 1.0);
	      }
	    storage[mapping(i, j, k)] = 0.0;
	  }

    if constexpr (!ValidForPacks)
      static_assert(!indexable<MDSpan, Int, int, Int>);

    if constexpr (!ValidForArrays)
      {
	static_assert(!indexable<MDSpan, std::array<Int, 3>>);
	static_assert(!indexable<MDSpan, std::span<Int, 3>>);
      }
    return true;
  }

constexpr bool
test_swap()
{
  using Extents = std::dextents<int, 2>;
  auto e1 = Extents{3, 5};
  auto e2 = Extents{7, 11};

  std::array<double, 3*5> s1{};
  std::array<double, 7*11> s2{};

  auto md1 = std::mdspan(s1.data(), e1);
  auto md2 = std::mdspan(s2.data(), e2);

  std::swap(md1, md2);

  VERIFY(md1.data_handle() == s2.data());
  VERIFY(md2.data_handle() == s1.data());

  VERIFY(md1.size() == s2.size());
  VERIFY(md2.size() == s1.size());
  return true;
}

namespace adl
{
  template<typename T>
    struct SwappableAccessor
    {
      using element_type = T;
      using reference = T&;
      using data_handle_type = T*;
      using offset_policy = SwappableAccessor;

      reference
      access(data_handle_type p, size_t i) const
      { return p[i]; }

      typename offset_policy::data_handle_type
      offset(data_handle_type p, size_t i) const
      { return p + i; }

      friend void
      swap(SwappableAccessor&, SwappableAccessor&)
      { ++swap_count; }

      static inline size_t swap_count = 0;
    };
}

void
test_swap_adl()
{
  using Extents = std::extents<int, dyn>;
  using Layout = std::layout_left;
  using Accessor = adl::SwappableAccessor<double>;
  Accessor::swap_count = 0;

  std::mdspan<double, Extents, Layout, Accessor> m1, m2;
  swap(m1, m2);
  VERIFY(Accessor::swap_count == 1);
}

template<bool Constructible, bool Assignable>
constexpr void
test_nothrow_movable()
{
  using Layout = std::layout_left;
  using Extents = std::dextents<int, 3>;
  using Accessor = OpaqueAccessor<int, Constructible, Assignable>;
  using Handle = Accessor::data_handle_type;
  static_assert(std::is_nothrow_move_assignable_v<Accessor>);
  static_assert(std::is_nothrow_move_constructible_v<Accessor>);
  static_assert(std::is_nothrow_move_assignable_v<Handle> == Assignable);
  static_assert(std::is_nothrow_move_constructible_v<Handle> == Constructible);

  using MDSpan = std::mdspan<int, Extents, Layout, Accessor>;
  static_assert(std::is_nothrow_move_assignable_v<MDSpan> == Assignable);
  static_assert(std::is_nothrow_move_constructible_v<MDSpan> == Constructible);
}

constexpr void
test_nothrow_movable_all()
{
  using MDSpan = std::mdspan<double, std::dextents<int, 3>>;
  static_assert(std::is_nothrow_move_assignable_v<MDSpan>);
  static_assert(std::is_nothrow_move_constructible_v<MDSpan>);

  test_nothrow_movable<true, true>();
  test_nothrow_movable<true, false>();
  test_nothrow_movable<false, true>();
  test_nothrow_movable<false, false>();
}

template<typename Layout, bool Expected>
  constexpr void
  test_nothrow_is_methods()
  {
    using Extents = std::extents<int, dyn>;
    using MDSpan = std::mdspan<double, Extents, Layout>;
    static_assert(noexcept(MDSpan::is_always_unique()) == Expected);
    static_assert(noexcept(MDSpan::is_always_exhaustive()) == Expected);
    static_assert(noexcept(MDSpan::is_always_strided()) == Expected);

    static_assert(noexcept(std::declval<MDSpan>().is_unique()) == Expected);
    static_assert(noexcept(std::declval<MDSpan>().is_exhaustive()) == Expected);
    static_assert(noexcept(std::declval<MDSpan>().is_strided()) == Expected);
  }

int
main()
{
  test_typedefs_all();

  test_rank_all();
  test_extent_all();
  static_assert(test_extent_all());

  test_class_properties_all();
  static_assert(test_class_properties_all());

  test_empty_all();
  static_assert(test_empty_all());

  test_default_ctor();
  static_assert(test_default_ctor());

  test_nothrow_default_ctor<std::default_accessor, true>();
  test_nothrow_default_ctor<ThrowingDefaultAccessor, false>();

  test_from_other();
  static_assert(test_from_other());

  test_from_carray();
  static_assert(test_from_carray());

  test_from_pointer_and_shape();
  static_assert(test_from_pointer_and_shape());

  test_from_pointer_and_constant();
  static_assert(test_from_pointer_and_constant());

  test_from_extents();
  static_assert(test_from_extents());

  test_from_mapping();
  static_assert(test_from_mapping());

  test_from_accessor();
  static_assert(test_from_accessor());

  test_from_int_like<int, true, true>();
  static_assert(test_from_int_like<int, true, true>());
  test_from_int_like<IntLike, true, true>();
  test_from_int_like<ThrowingInt, false, false>();
  test_from_int_like<MutatingInt, true, false>();
  test_from_int_like<RValueInt, true, false>();

  test_from_opaque_accessor();
  test_from_base_class_accessor();
  test_from_mapping_like();
  static_assert(test_from_mapping_like());

  test_access<int, true, true>();
  static_assert(test_access<int, true, true>());
  test_access<IntLike, true, true>();
  test_access<ThrowingInt, false, false>();
  test_access<MutatingInt, true, false>();
  test_access<RValueInt, true, false>();

  test_swap();
  static_assert(test_swap());
  test_swap_adl();

  test_nothrow_movable_all();
  test_nothrow_is_methods<std::layout_right, true>();
  test_nothrow_is_methods<ThrowingLayout, false>();
  return 0;
}
