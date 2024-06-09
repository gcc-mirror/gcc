// { dg-do run { target c++11 } }
// { dg-require-debug-mode "" }

// PR libstdc++/114316

#include <unordered_set>
#include <algorithm>

#include <testsuite_hooks.h>

void test01()
{
  std::unordered_set<int>::iterator it{};
  VERIFY( std::find(it, it, 0) == it );
}

void test02()
{
  std::unordered_set<int>::local_iterator it{};
  VERIFY( std::find(it, it, 0) == it );
}

int main()
{
  test01();
  test02();
  return 0;
}
