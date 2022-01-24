// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr bool
test_reset()
{
  std::optional<int> oi(1);
  oi.reset();
  VERIFY( ! oi.has_value() );
  oi.reset();
  VERIFY( ! oi.has_value() );

  return true;
}

static_assert( test_reset() );
