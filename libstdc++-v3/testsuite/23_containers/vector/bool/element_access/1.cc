// { dg-do compile { target c++23 } }

#include <vector>
#include <testsuite_hooks.h>

constexpr bool
test01()
{
  // P2321R2
  // constexpr const reference& vector<bool>::operator=(bool x) const noexcept;

  std::vector<bool> v(1);
  const auto e = v[0];
  e = true;
  VERIFY( v[0] );

  return true;
}

int
main()
{
  static_assert(test01());
}
