// { dg-do run { target c++20 } }
// { dg-require-debug-mode "" }

#include <deque>
#include <testsuite_hooks.h>

void test01()
{
  std::deque<int> d;

  for (int i = 0; i != 10; ++i)
    d.push_back(i);

  auto before = d.begin() + 4;
  auto last = d.end() - 1;

  VERIFY( std::erase(d, 6) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_singular());
}

int main()
{
  test01();
  return 0;
}
