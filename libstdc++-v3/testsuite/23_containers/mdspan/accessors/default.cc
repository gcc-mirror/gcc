// { dg-do run { target c++23 } }
#include <mdspan>

#include <testsuite_hooks.h>

constexpr size_t dyn = std::dynamic_extent;

template<typename Accessor>
  constexpr void
  test_accessor_policy()
  {
    static_assert(std::copyable<Accessor>);
    static_assert(std::is_nothrow_move_constructible_v<Accessor>);
    static_assert(std::is_nothrow_move_assignable_v<Accessor>);
    static_assert(std::is_nothrow_swappable_v<Accessor>);
  }

constexpr bool
test_access()
{
  std::default_accessor<double> accessor;
  std::array<double, 5> a{10, 11, 12, 13, 14};
  VERIFY(accessor.access(a.data(), 0) == 10);
  VERIFY(accessor.access(a.data(), 4) == 14);
  return true;
}

constexpr bool
test_offset()
{
  std::default_accessor<double> accessor;
  std::array<double, 5> a{10, 11, 12, 13, 14};
  VERIFY(accessor.offset(a.data(), 0) == a.data());
  VERIFY(accessor.offset(a.data(), 4) == a.data() + 4);
  return true;
}

class Base
{ };

class Derived : public Base
{ };

constexpr void
test_ctor()
{
  // T -> T
  static_assert(std::is_nothrow_constructible_v<std::default_accessor<double>,
						std::default_accessor<double>>);
  static_assert(std::is_convertible_v<std::default_accessor<double>,
				      std::default_accessor<double>>);

  // T -> const T
  static_assert(std::is_convertible_v<std::default_accessor<double>,
				      std::default_accessor<const double>>);
  static_assert(std::is_convertible_v<std::default_accessor<Derived>,
				      std::default_accessor<const Derived>>);

  // const T -> T
  static_assert(!std::is_constructible_v<std::default_accessor<double>,
					 std::default_accessor<const double>>);
  static_assert(!std::is_constructible_v<std::default_accessor<Derived>,
					 std::default_accessor<const Derived>>);

  // T <-> volatile T
  static_assert(std::is_convertible_v<std::default_accessor<int>,
				      std::default_accessor<volatile int>>);
  static_assert(!std::is_constructible_v<std::default_accessor<int>,
					 std::default_accessor<volatile int>>);

  // size difference
  static_assert(!std::is_constructible_v<std::default_accessor<char>,
					 std::default_accessor<int>>);

  // signedness
  static_assert(!std::is_constructible_v<std::default_accessor<int>,
					 std::default_accessor<unsigned int>>);
  static_assert(!std::is_constructible_v<std::default_accessor<unsigned int>,
					 std::default_accessor<int>>);

  // Derived <-> Base
  static_assert(!std::is_constructible_v<std::default_accessor<Base>,
					 std::default_accessor<Derived>>);
  static_assert(!std::is_constructible_v<std::default_accessor<Derived>,
					 std::default_accessor<Base>>);

}

int
main()
{
  test_accessor_policy<std::default_accessor<double>>();
  test_access();
  static_assert(test_access());
  test_offset();
  static_assert(test_offset());
  test_ctor();
  return 0;
}
