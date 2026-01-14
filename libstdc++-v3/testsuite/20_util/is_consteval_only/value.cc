// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <type_traits>
#include <testsuite_tr1.h>

void test01()
{
  using std::is_consteval_only;
  using namespace __gnu_test;
  int v = 1;
  struct S1 { decltype(^^long) a; };
  union U2 { int a; decltype(^^test01) b; };
  struct S3 { const decltype(^^__gnu_test) *c; };
  struct S4 : public S3 {};
  struct S5 { int a; long *b; };

  static_assert(test_category<is_consteval_only, decltype(^^long)>(true), "");
  static_assert(test_category<is_consteval_only, const decltype(^^test01)>(true), "");
  static_assert(test_category<is_consteval_only, volatile decltype(^^__gnu_test)>(true), "");
  static_assert(test_category<is_consteval_only, const volatile decltype(^^v)>(true), "");
  static_assert(test_category<is_consteval_only, const S1>(true), "");
  static_assert(test_category<is_consteval_only, U2>(true), "");
  static_assert(test_category<is_consteval_only, S3>(true), "");
  static_assert(test_category<is_consteval_only, S4>(true), "");

  // Sanity check.
  static_assert(test_category<is_consteval_only, int>(false), "");
  static_assert(test_category<is_consteval_only, S5>(false), "");
}
