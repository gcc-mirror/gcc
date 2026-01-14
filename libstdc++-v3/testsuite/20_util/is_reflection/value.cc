// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <type_traits>
#include <testsuite_tr1.h>

void test01()
{
  using std::is_reflection;
  using namespace __gnu_test;
  int v = 1;

  static_assert(test_category<is_reflection, decltype(^^long)>(true), "");
  static_assert(test_category<is_reflection, const decltype(^^test01)>(true), "");
  static_assert(test_category<is_reflection, volatile decltype(^^__gnu_test)>(true), "");
  static_assert(test_category<is_reflection, const volatile decltype(^^v)>(true), "");

  // Sanity check.
  static_assert(test_category<is_reflection, int>(false), "");
}
