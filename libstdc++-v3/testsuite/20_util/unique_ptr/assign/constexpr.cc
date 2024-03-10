// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

constexpr bool
test_move()
{
  std::unique_ptr<int> p1(new int(2));
  std::unique_ptr<int> p2;
  p2 = std::move(p1);
  VERIFY( *p2 == 2 );
  std::unique_ptr<int[]> a1(new int[]{0, 1, 2});
  std::unique_ptr<int[]> a2;
  a2 = std::move(a1);
  VERIFY( a2[2] == 2 );

  return true;
}
static_assert( test_move() );

constexpr bool
test_convert()
{
  std::unique_ptr<int> p1(new int(2));
  std::unique_ptr<const int> p2;
  p2 = std::move(p1);
  VERIFY( *p2 == 2 );
  std::unique_ptr<int[]> a1(new int[]{0, 1, 2});
  std::unique_ptr<const int[]> a2;
  a2 = std::move(a1);
  VERIFY( a2[2] == 2 );

  return true;
}
static_assert( test_convert() );

constexpr bool
test_null()
{
  std::unique_ptr<int> p(new int(2));
  p = nullptr;
  VERIFY( !p );
  p = nullptr;
  return true;
}
static_assert( test_null() );
