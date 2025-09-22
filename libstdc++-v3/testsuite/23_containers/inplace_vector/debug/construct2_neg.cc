// { dg-do run { target c++26 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <inplace_vector>
#include <debug/checks.h>

void test01()
{
  __gnu_test::check_construct2<std::inplace_vector<int, 10> >();
}

int main()
{
  test01();
  return 0;
}
