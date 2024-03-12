// { dg-do run { target c++11 } }

#include <list>
#include <testsuite_hooks.h>

void
test01()
{
  std::list<int> c;
  std::list<int>::iterator pos;

  // libstdc++/52799
  pos = c.emplace(c.begin());
  VERIFY( c.size() == 1 );
  VERIFY( *pos == 0 );
  VERIFY( pos == c.begin() );
  pos = c.emplace(c.begin(), 2);
  VERIFY( c.size() == 2 );
  VERIFY( *pos == 2 );
  VERIFY( pos == c.begin() );
  VERIFY( *++pos == 0 );
  pos = c.emplace(c.end(), 3);
  VERIFY( c.size() == 3 );
  VERIFY( *pos == 3 );
  VERIFY( pos == --c.end() );
  VERIFY( *--pos == 0 );
  VERIFY( *--pos == 2 );

  // const_iterator
  pos = c.emplace(c.cbegin());
  VERIFY( c.size() == 4 );
  VERIFY( *pos == 0 );
  VERIFY( pos == c.cbegin() );
  VERIFY( *++pos == 2 );
  std::list<int>::const_iterator i = std::next(c.cbegin(), 2);
  pos = c.emplace(i, 22);
  VERIFY( c.size() == 5 );
  VERIFY( *pos == 22 );
  VERIFY( pos == std::prev(i) );
  VERIFY( *--pos == 2 );
  VERIFY( *--pos == 0 );
}

struct V
{
  explicit V(int a, int b = 0) : val(a+b) { }
  int val;
};

void
test02()
{
  std::list<V> c;
  std::list<V>::iterator pos;

  pos = c.emplace(c.end(), 1);
  VERIFY( c.size() == 1 );
  VERIFY( pos->val == 1 );
  VERIFY( pos == --c.end() );
  pos = c.emplace(c.cend(), 2, 3);
  VERIFY( c.size() == 2 );
  VERIFY( pos->val == 5 );
  VERIFY( pos == --c.cend() );
  VERIFY( c.front().val == 1 );
}

int main()
{
  test01();
  test02();
}
