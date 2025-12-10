// { dg-do run { target c++20 } }

#include <debug/string>
#include <testsuite_hooks.h>

using __gnu_debug::string;

void test01()
{
  string str("abcdefghijklmnopqrstuvwxyz");

  auto before = str.begin();
  auto last = str.end() - 1;

  VERIFY( std::erase(str, 'd') == 1 );

  VERIFY(before._M_singular());
  VERIFY(last._M_singular());
}

int main()
{
  test01();
  return 0;
}
