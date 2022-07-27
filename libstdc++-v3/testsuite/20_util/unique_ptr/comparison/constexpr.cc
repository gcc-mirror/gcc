// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <memory>
#include <testsuite_hooks.h>

constexpr bool
test_eq()
{
  std::unique_ptr<int> p1, p2;
  VERIFY( p1 == p2 );
  p1.reset(new int(1));
  VERIFY( p1 == p1 );
  VERIFY( p1 != p2 );
  struct null_deleter { constexpr void operator()(const void*) const { } };
  std::unique_ptr<const int[], null_deleter> p3(p1.get());
  VERIFY( p3 == p3 );
  VERIFY( p1 == p3 );
  VERIFY( p3 != p2 );

  return true;
}
static_assert( test_eq() );

constexpr bool
test_rel()
{
  std::unique_ptr<int> p1, p2;
  VERIFY( !(p1 < p2) );
  VERIFY( !(p1 > p2) );
  VERIFY( p1 <= p2 );
  VERIFY( p1 >= p2 );
  p1.reset(new int(1));
  VERIFY( p1 <= p1 );
  VERIFY( p1 >= p1 );
  VERIFY( p1 > p2 );
  VERIFY( p2 < p1 );
  VERIFY( p2 <= p1 );
  VERIFY( p1 >= p2 );
  struct null_deleter { constexpr void operator()(const void*) const { } };
  std::unique_ptr<const int[], null_deleter> p3(p1.get());
  VERIFY( p3 <= p3 );
  VERIFY( p3 >= p3 );
  VERIFY( p1 <= p3 );
  VERIFY( p3 > p2 );
  VERIFY( p3 >= p2 );
  VERIFY( p2 < p3 );
  VERIFY( p2 <= p3 );

  return true;
}
static_assert( test_rel() );

constexpr bool
test_3way()
{
  std::unique_ptr<int> p1, p2;
  VERIFY( (p1 <=> p1) == 0 );
  VERIFY( (p1 <=> p2) == 0 );
  p1.reset(new int(1));
  VERIFY( (p1 <=> p1) == 0 );
  VERIFY( (p1 <=> p2) > 0 );
  VERIFY( (p2 <=> p1) < 0 );
  struct null_deleter { constexpr void operator()(const void*) const { } };
  std::unique_ptr<const int[], null_deleter> p3(p1.get());
  VERIFY( (p3 <=> p3) == 0 );
  VERIFY( (p1 <=> p3) == 0 );
  VERIFY( (p3 <=> p2) > 0 );
  VERIFY( (p2 <=> p3) < 0 );

  return true;
}
static_assert( test_3way() );
