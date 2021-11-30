// { dg-do run { target c++11 } }

// PR libstdc++/103501

#include <set>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

struct Y
{
  int i;

  Y(int i) : i(i) { }
  Y(const Y& y) noexcept : i(y.i) { }
  Y(Y&& y) noexcept : i(y.i) { y.i = -y.i; }

  bool operator<(const Y& rhs) const { return i < rhs.i; }
};

int main()
{
  using Alloc = __gnu_test::uneq_allocator<Y>;
  std::multiset<Y, std::less<Y>, Alloc> s1{ {1, 2, 3}, Alloc(1)};
  std::multiset<Y, std::less<Y>, Alloc> s2{ std::move(s1), Alloc(2) };
  const Y* prev = nullptr;
  for (const Y& y : s1)
  {
    if (prev)
      VERIFY( !(y < *prev) );
    prev = &y;
  }
}
