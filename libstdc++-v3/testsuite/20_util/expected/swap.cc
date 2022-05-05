// { dg-options "-std=gnu++23" }
// { dg-do run { target c++23 } }

#include <expected>
#include <testsuite_hooks.h>

constexpr bool
test_swap()
{
  std::expected<int, int> e1(1), e2(2);
  std::expected<int, int> e3(std::unexpect, 3), e4(std::unexpect, 4);

  swap(e1, e2);
  VERIFY( e1.value() == 2 );
  VERIFY( e2.value() == 1 );
  swap(e1, e3);
  VERIFY( ! e1.has_value() );
  VERIFY( e1.error() == 3 );
  VERIFY( e3.value() == 2 );
  swap(e1, e3);
  VERIFY( ! e3.has_value() );
  VERIFY( e1.value() == 2 );
  VERIFY( e3.error() == 3 );
  swap(e3, e4);
  VERIFY( ! e3.has_value() );
  VERIFY( ! e4.has_value() );
  VERIFY( e3.error() == 4 );
  VERIFY( e4.error() == 3 );

  std::expected<void, int> v1, v2;
  std::expected<void, int> v3(std::unexpect, 3), v4(std::unexpect, 4);

  swap(v1, v2);
  VERIFY( v1.has_value() );
  VERIFY( v2.has_value() );
  swap(v1, v3);
  VERIFY( ! v1.has_value() );
  VERIFY( v1.error() == 3 );
  VERIFY( v3.has_value() );
  swap(v1, v3);
  VERIFY( ! v3.has_value() );
  VERIFY( v1.has_value() );
  VERIFY( v3.error() == 3 );
  swap(v3, v4);
  VERIFY( ! v3.has_value() );
  VERIFY( ! v4.has_value() );
  VERIFY( v3.error() == 4 );
  VERIFY( v4.error() == 3 );

  return true;
}

int main()
{
  static_assert( test_swap() );
  test_swap();
}
