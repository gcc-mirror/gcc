// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <memory>

#ifndef __cpp_lib_constexpr_memory
# error "Feature test macro for constexpr unique_ptr is missing in <memory>"
#elif __cpp_lib_constexpr_memory < 202202L
# error "Feature test macro for constexpr unique_ptr has wrong value in <memory>"
#endif

#include <testsuite_hooks.h>

constexpr bool
test_default()
{
  std::unique_ptr<int> p;
  std::unique_ptr<int> np(nullptr);
  VERIFY( p == np );

  std::unique_ptr<int[]> a;
  std::unique_ptr<int[]> na(nullptr);
  VERIFY( a == na );

  return true;
}
static_assert( test_default() );

constexpr bool
test_ptr()
{
  std::unique_ptr<int> p(new int(2));
  VERIFY( *p == 2 );
  std::unique_ptr<int[]> a(new int[]{0, 1, 2});
  VERIFY( a[2] == 2 );

  return true;
}
static_assert( test_ptr() );

constexpr bool
test_del()
{
  const std::default_delete<int> pd;
  std::unique_ptr<int> p1(new int(1), pd);
  VERIFY( *p1 == 1 );
  std::unique_ptr<int> p2(new int(2), std::default_delete<int>{});
  VERIFY( *p2 == 2 );
  const std::default_delete<int[]> ad;
  std::unique_ptr<int[]> a1(new int[]{3, 4}, ad);
  VERIFY( a1[0] == 3 );
  std::unique_ptr<int[]> a2(new int[]{5, 6}, std::default_delete<int[]>{});
  VERIFY( a2[1] == 6 );

  return true;
}
static_assert( test_del() );

constexpr bool
test_move()
{
  std::unique_ptr<int> p1(new int(2));
  std::unique_ptr<int> p2 = std::move(p1);
  VERIFY( *p2 == 2 );
  std::unique_ptr<int[]> a1(new int[]{0, 1, 2});
  std::unique_ptr<int[]> a2 = std::move(a1);
  VERIFY( a2[2] == 2 );

  return true;
}
static_assert( test_move() );

constexpr bool
test_convert()
{
  std::unique_ptr<int> p1(new int(2));
  std::unique_ptr<const int> p2 = std::move(p1);
  VERIFY( *p2 == 2 );
  std::unique_ptr<int[]> a1(new int[]{0, 1, 2});
  std::unique_ptr<const int[]> a2 = std::move(a1);
  VERIFY( a2[2] == 2 );

  return true;
}
static_assert( test_convert() );
