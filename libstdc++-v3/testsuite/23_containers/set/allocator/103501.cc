// { dg-do run { target c++11 } }

// PR libstdc++/103501

#include <set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct X
{
  int i;

  X(int i) : i(i) { }
  X(const X& x) noexcept : i(x.i) { }
  X(X&& x) noexcept : i(x.i) { x.i = -1; }

  bool operator<(const X& rhs) const { return i < rhs.i; }
};

int main()
{
  using Alloc = __gnu_test::uneq_allocator<X>;
  std::set<X, std::less<X>, Alloc> s1{ {1, 2, 3}, Alloc(1)};
  std::set<X, std::less<X>, Alloc> s2{ std::move(s1), Alloc(2) };
  const X* prev = nullptr;
  for (const X& x : s1)
  {
    if (prev)
      VERIFY( *prev < x );
    prev = &x;
  }
}
