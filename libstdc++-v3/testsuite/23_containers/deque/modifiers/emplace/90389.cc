// { dg-do run { target c++11 } }

// Bug 90389 - std::deque::emplace tries to call wrong overload internally

#include <deque>
#include <testsuite_hooks.h>

struct X
{
  X() = default;
  X(void*, void*, std::size_t) { }
};

void
test_pr90389()
{
  const int n = 3;
  std::deque<X> d(n);
  d.emplace(d.begin(), nullptr, nullptr, d.size());
  VERIFY( d.size() == n+1 );
}

struct Y
{
  Y() = default;
  Y(std::size_t, const Y&) { }
};

void
test_pr118079()
{
  const int n = 3;
  std::deque<Y> d(n);
  const Y y{};
  d.emplace(d.begin(), d.size(), y);
  VERIFY( d.size() == n+1 );
}

int main()
{
  test_pr90389();
  test_pr118079();
}
