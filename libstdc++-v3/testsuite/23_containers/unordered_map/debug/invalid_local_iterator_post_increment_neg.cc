// { dg-do run { target c++11 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_map>
#include <debug/unordered_checks.h>

void test01()
{
  __gnu_test::invalid_local_iterator_post_increment<std::unordered_map<int, int>>();
}

int main()
{
  test01();
  return 0;
}
