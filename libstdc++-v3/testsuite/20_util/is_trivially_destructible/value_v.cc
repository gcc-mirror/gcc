// { dg-do compile { target c++17 } }
#include <type_traits>
#include <testsuite_tr1.h>

template<typename T>
constexpr void test_cv()
{
  static_assert(std::is_trivially_destructible_v<const T>
      == std::is_trivially_destructible_v<T>);
  static_assert(std::is_trivially_destructible_v<volatile T>
      == std::is_trivially_destructible_v<T>);
  static_assert(std::is_trivially_destructible_v<const volatile T>
      == std::is_trivially_destructible_v<T>);
}

template<typename T, bool Expected>
void test()
{
  static_assert(std::is_trivially_destructible_v<T> == Expected);
  test_cv<T>();
}

void test01()
{
  using namespace __gnu_test;

  test<int, true>();
  test<int&, true>();
  test<int&&, true>();
  test<int[1], true>();
  test<TType, true>();
  test<TType[1], true>();
  test<PODType, true>();
  test<PODType[1], true>();
  test<NType, false>();
  test<SLType, false>();
  test<int(), false>();
  test<void, false>();
  test<int[], false>();
}
