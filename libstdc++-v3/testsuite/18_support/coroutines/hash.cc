// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <coroutine>
#include <testsuite_hooks.h>

void
test01()
{
  std::hash<std::noop_coroutine_handle> h;
  std::size_t v = h(std::noop_coroutine());

  const auto& ch = h;
  std::size_t v2 = h(std::noop_coroutine()); // PR libstdc++/109165

  VERIFY( v2 == v );
}

int main()
{
  test01();
}
