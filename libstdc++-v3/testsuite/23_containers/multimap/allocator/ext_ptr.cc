// { dg-do run { target c++11 } }

#include <map>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct T { int i; };

bool operator<(const T& l, const T& r)
{ return l.i < r.i; }

struct L : std::less<T>
{ };

using __gnu_test::CustomPointerAlloc;

template class std::multimap<T, int, L,
			CustomPointerAlloc<std::pair<const T, int>>>;

void test01()
{
  typedef CustomPointerAlloc<std::pair<const T, int>> alloc_type;
  typedef std::multimap<T, int, L, alloc_type> test_type;
  test_type v;
  v.insert({ T(), 0 });
  VERIFY( ++v.begin() == v.end() );
}

int main()
{
  test01();
}
