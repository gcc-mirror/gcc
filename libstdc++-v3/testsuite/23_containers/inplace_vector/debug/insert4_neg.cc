// { dg-do run { target c++26 xfail *-*-* } }

#include <debug/inplace_vector>
#include <debug/checks.h>

void test01()
{
  __gnu_test::check_insert1<__gnu_debug::inplace_vector<int, 10>>();
}

int main()
{
  test01();
  return 0;
}
