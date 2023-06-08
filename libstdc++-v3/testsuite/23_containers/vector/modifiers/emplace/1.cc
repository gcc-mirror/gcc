// { dg-do run { target c++11 } }

#include <vector>
#include <testsuite_hooks.h>

void
test01()
{
  std::vector<int> c;
  std::vector<int>::iterator pos;

  // libstdc++/52799
  pos = c.emplace(c.begin());
  VERIFY( c.size() == 1 );
  VERIFY( c[0] == 0 );
  VERIFY( pos == c.begin() );
  pos = c.emplace(c.begin(), 2);
  VERIFY( c.size() == 2 );
  VERIFY( c[0] == 2 );
  VERIFY( c[1] == 0 );
  VERIFY( pos == c.begin() );
  pos = c.emplace(c.end(), 3);
  VERIFY( c.size() == 3 );
  VERIFY( c[0] == 2 );
  VERIFY( c[1] == 0 );
  VERIFY( c[2] == 3 );
  VERIFY( pos == --c.end() );

  // const_iterator
  pos = c.emplace(c.cbegin());
  VERIFY( c.size() == 4 );
  VERIFY( c[0] == 0 );
  VERIFY( c[1] == 2 );
  VERIFY( pos == c.cbegin() );
  pos = c.emplace(c.cbegin() + 2, 22);
  VERIFY( c.size() == 5 );
  VERIFY( c[0] == 0 );
  VERIFY( c[1] == 2 );
  VERIFY( c[2] == 22 );
  VERIFY( pos == c.cbegin() + 2 );
}

struct V
{
  explicit V(int a, int b = 0) : val(a+b) { }
  int val;
};

void
test02()
{
  std::vector<V> c;
  std::vector<V>::iterator pos;

  pos = c.emplace(c.end(), 1);
  VERIFY( c.size() == 1 );
  VERIFY( c[0].val == 1 );
  VERIFY( pos == --c.end() );
  pos = c.emplace(c.cend(), 2, 3);
  VERIFY( c.size() == 2 );
  VERIFY( c[0].val == 1 );
  VERIFY( c[1].val == 5 );
  VERIFY( pos == --c.cend() );
}

int main()
{
  test01();
  test02();
}
