// { dg-do run { target c++11 } }

// PR libstdc++/118087
// std::deque::emplace does not do uses-allocator construction

#include <deque>
#include <scoped_allocator>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

template<typename T>
using Alloc = __gnu_test::propagating_allocator<T, true>;

struct X
{
  using allocator_type = Alloc<int>;
  X() { }
  X(const X&) { }
  X(X&&) { }
  X(const allocator_type& a) : alloc(a) { }
  X(const X&, const allocator_type& a) : alloc(a) { }
  X(X&&, const allocator_type& a) : alloc(a) { }

  X& operator=(const X&) = default;

  allocator_type alloc{-1};
};

int main()
{
  std::deque<X, std::scoped_allocator_adaptor<Alloc<X>>> d(2, Alloc<X>(50));
  VERIFY(d[0].alloc.get_personality() == 50);
  VERIFY(d[1].alloc.get_personality() == 50);

  d.emplace(d.begin() + 1);
  VERIFY(d[1].alloc.get_personality() == 50);

  d.emplace_front();
  VERIFY(d[0].alloc.get_personality() == 50);

  d.emplace_back();
  VERIFY(d[d.size() - 1].alloc.get_personality() == 50);
}
