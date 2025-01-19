// { dg-do run { target c++11 } }

#include <set>
#include <memory>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct T { int i; };

bool operator<(const T& l, const T& r)
{ return l.i < r.i; }

struct L : std::less<T>
{ };

using __gnu_test::CustomPointerAlloc;

template class std::set<T, L, CustomPointerAlloc<T>>;

void test01()
{
  typedef CustomPointerAlloc<T> alloc_type;
  typedef std::set<T, L, alloc_type> test_type;
  test_type v;
  v.insert(T());
  VERIFY( ++v.begin() == v.end() );
}

int main()
{
  test01();
}
