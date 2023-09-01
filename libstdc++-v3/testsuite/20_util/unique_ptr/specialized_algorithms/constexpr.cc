// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

constexpr bool
test_swap_single()
{
  std::unique_ptr<int> p1;
  swap(p1, p1);
  VERIFY( !p1 );
  std::unique_ptr<int> p2;
  swap(p1, p2);
  VERIFY( !p1 && !p2 );
  std::unique_ptr<int> p3(new int(3));
  swap(p3, p3);
  VERIFY( *p3 == 3 );
  swap(p1, p3);
  VERIFY( *p1 == 3 );
  std::unique_ptr<int> p4(new int(4));
  swap(p4, p1);
  VERIFY( *p4 == 3 );
  VERIFY( *p1 == 4 );

  return true;
}
static_assert( test_swap_single() );

constexpr bool
test_swap_array()
{
  std::unique_ptr<int[]> a1;
  std::unique_ptr<int[]> a2;
  swap(a1, a2);
  VERIFY( !a1 && !a2 );
  std::unique_ptr<int[]> a3(new int[]{3});
  swap(a1, a3);
  VERIFY( a1[0] == 3 );
  std::unique_ptr<int[]> a4(new int[]{4, 5});
  swap(a1, a4);
  VERIFY( a1[1] == 5 );

  return true;
}
static_assert( test_swap_array() );
