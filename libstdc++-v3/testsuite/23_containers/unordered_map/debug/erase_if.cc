// { dg-do run { target c++20 } }
// { dg-require-debug-mode "" }

#include <unordered_map>
#include <testsuite_hooks.h>

auto is_six_pair = [](const std::pair<const int, int>& p)
{
  return p.first == 6;
};

void test01()
{
  std::unordered_map<int, int> m;
  for (int i = 0; i != 10; ++i)
    m.insert({ i, i });

  auto before = m.find(1);
  auto match = m.find(6);
  auto last = m.find(9);

  VERIFY( std::erase_if(m, is_six_pair) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(match._M_singular());
  VERIFY(last._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
