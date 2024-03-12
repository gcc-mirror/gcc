// { dg-do compile { target c++23 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

constexpr bool
test_creation_single()
{
  std::unique_ptr<int> p = std::make_unique<int>(1);
  VERIFY( *p == 1 );
  p = std::make_unique_for_overwrite<int>();
  *p = 2;
  VERIFY( *p == 2 );

  return true;
}
static_assert( test_creation_single() );

constexpr bool
test_creation_array()
{
  std::unique_ptr<int[]> a = std::make_unique<int[]>(2);
  VERIFY( a[0] == 0 );
  VERIFY( a[1] == 0 );
  a = std::make_unique_for_overwrite<int[]>(2);
  a[0] = 1;
  a[1] = 2;
  VERIFY( a[0] == 1 );
  VERIFY( a[1] == 2 );

  return true;
}
static_assert( test_creation_array() );
