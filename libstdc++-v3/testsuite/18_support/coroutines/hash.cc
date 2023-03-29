// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <coroutine>
#include <testsuite_hooks.h>

void
test01()
{
  auto coro = std::noop_coroutine();
  std::hash<std::noop_coroutine_handle> h;
  std::size_t v = h(coro);

  const auto& ch = h;
  std::size_t v2 = ch(coro); // PR libstdc++/109165

  VERIFY( v2 == v );
}

int main()
{
  test01();
}
