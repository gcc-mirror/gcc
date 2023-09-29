// { dg-do compile { target c++20 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr bool
test_swap()
{
  std::optional<int> o0, o1(1);
  o0.swap(o1);
  VERIFY( *o0 == 1 );
  VERIFY( ! o1.has_value() );
  o0.swap(o1);
  VERIFY( ! o0.has_value() );
  VERIFY( *o1 == 1 );
  o0.swap(o0);
  VERIFY( ! o0.has_value() );
  o1.swap(o1);
  VERIFY( *o1 == 1 );
  std::optional<int> o2(2);
  swap(o1, o2);
  VERIFY( *o1 == 2 );
  VERIFY( *o2 == 1 );

  return true;
}

static_assert( test_swap() );
