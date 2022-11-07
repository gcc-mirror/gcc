// { dg-do run { xfail *-*-* } }
// { dg-options "-D_GLIBCXX_DEBUG_BACKTRACE -lstdc++_libbacktrace" }
// { dg-require-effective-target stacktrace }

#include <debug/vector>
#include <debug/checks.h>

void test01()
{
  __gnu_test::check_assign1<__gnu_debug::vector<int> >();
}

int main()
{
  test01();
  return 0;
}
