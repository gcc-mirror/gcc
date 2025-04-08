// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_set>
#include <debug/unordered_checks.h>

void test01()
{
  __gnu_test::invalid_local_iterator_const_conversion
    <std::unordered_multiset<int>>();
}

int main()
{
  test01();
  return 0;
}
