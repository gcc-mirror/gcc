// { dg-do run { target c++26 } }
#include <type_traits>
#include <concepts>

#include <testsuite_hooks.h>

template<typename Type, Type Value>
  struct ConstWrapper
  {
    constexpr static Type value = Value;
  };

constexpr void
check_same(auto actual, auto expected)
{
  VERIFY(actual == expected);
  static_assert(std::same_as<decltype(actual), decltype(expected)>);
}

constexpr void
test_mix_integer_constant()
{
  auto i4 = std::integral_constant<int, 4>{};
  auto c3 = std::cw<3>;
  auto w2 = ConstWrapper<int, 2>{};

  check_same(i4 + c3, std::cw<7>);
  check_same(c3 + i4, std::cw<7>);
  check_same(c3 + w2, std::cw<5>);
  check_same(w2 + c3, std::cw<5>);
}

constexpr void
test_array()
{
  constexpr double x[] = {1.1, 2.2, 3.3};
  auto cx = std::cw<x>;
  auto i2 = std::integral_constant<int, 2>{};
  auto w2 = ConstWrapper<int, 2>{};

  check_same(x[i2], x[2]);
  check_same(cx[i2], std::cw<x[2]>);
  check_same(cx[w2], std::cw<x[2]>);
}

constexpr void
test_function_object()
{
  auto cadd = std::cw<[](int i, int j) { return i + j; }>;
  auto i4 = std::integral_constant<int, 4>{};
  auto c3 = std::cw<3>;
  auto w2 = ConstWrapper<int, 2>{};

  check_same(cadd(i4, c3), std::cw<7>);
  check_same(cadd(c3, i4), std::cw<7>);
  check_same(cadd(w2, c3), std::cw<5>);
  check_same(cadd(c3, w2), std::cw<5>);
}

constexpr bool
test_all()
{
  test_mix_integer_constant();
  test_array();
  test_function_object();
  return true;
}

int
main()
{
  test_all();
  static_assert(test_all());
  return 0;
}
