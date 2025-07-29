// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

template<typename Accessor>
  constexpr bool
  test_class_properties()
  {
    static_assert(std::is_trivially_copyable_v<Accessor>);
    static_assert(std::semiregular<Accessor>);
    return true;
  }

template<typename Accessor>
  constexpr bool
  test_accessor_policy()
  {
    static_assert(std::copyable<Accessor>);
    static_assert(std::is_nothrow_move_constructible_v<Accessor>);
    static_assert(std::is_nothrow_move_assignable_v<Accessor>);
    static_assert(std::is_nothrow_swappable_v<Accessor>);
    return true;
  }

class Base
{ };

class Derived : public Base
{ };

template<template<typename T> typename Accessor>
  constexpr bool
  test_ctor()
  {
    // T -> T
    static_assert(std::is_nothrow_constructible_v<Accessor<double>,
						  Accessor<double>>);
    static_assert(std::is_convertible_v<Accessor<double>, Accessor<double>>);

    // T -> const T
    static_assert(std::is_convertible_v<Accessor<double>,
					Accessor<const double>>);
    static_assert(std::is_convertible_v<Accessor<Derived>,
					Accessor<const Derived>>);

    // const T -> T
    static_assert(!std::is_constructible_v<Accessor<double>,
					   Accessor<const double>>);
    static_assert(!std::is_constructible_v<Accessor<Derived>,
					   Accessor<const Derived>>);

    // T <-> volatile T
    static_assert(std::is_convertible_v<Accessor<int>, Accessor<volatile int>>);
    static_assert(!std::is_constructible_v<Accessor<int>,
					   Accessor<volatile int>>);

    // size difference
    static_assert(!std::is_constructible_v<Accessor<char>, Accessor<int>>);

    // signedness
    static_assert(!std::is_constructible_v<Accessor<int>,
					   Accessor<unsigned int>>);
    static_assert(!std::is_constructible_v<Accessor<unsigned int>,
					   Accessor<int>>);

    // Derived <-> Base
    static_assert(!std::is_constructible_v<Accessor<Base>, Accessor<Derived>>);
    static_assert(!std::is_constructible_v<Accessor<Derived>, Accessor<Base>>);
    return true;
  }

template<template<typename T> typename Accessor>
  constexpr bool
  test_properties()
  {
    test_class_properties<Accessor<double>>();
    test_accessor_policy<Accessor<double>>();
    test_ctor<Accessor>();
    return true;
  }

static_assert(test_properties<std::default_accessor>());

template<typename A>
  constexpr size_t
  accessor_alignment = alignof(typename A::element_type);

template<typename Accessor>
  constexpr void
  test_access(Accessor accessor)
  {
    constexpr size_t N = accessor_alignment<Accessor>;
    alignas(N) std::array<double, 5> a{10, 11, 12, 13, 14};
    for (size_t i = 0; i < a.size(); ++i)
      VERIFY(accessor.access(a.data(), i) == 10 + i);
  }

template<typename Accessor>
  constexpr void
  test_offset(Accessor accessor)
  {
    constexpr size_t N = accessor_alignment<Accessor>;
    alignas(N) std::array<double, 5> a{10, 11, 12, 13, 14};
    for (size_t i = 0; i < a.size(); ++i)
      VERIFY(accessor.offset(a.data(), i) == a.data() + i);
  }

template<typename Accessor>
  constexpr bool
  test_all()
  {
    auto accessor = Accessor{};
    test_offset(accessor);
    test_access(accessor);
    return true;
  }

int
main()
{
  test_all<std::default_accessor<double>>();
  static_assert(test_all<std::default_accessor<double>>());
  return 0;
}
