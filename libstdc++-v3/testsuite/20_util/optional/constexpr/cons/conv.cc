// { dg-options "-std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <optional>
#include <testsuite_hooks.h>

constexpr bool
test_cons()
{
  std::optional<int> oi(1);
  std::optional<long> ol(oi);
  VERIFY( *ol == 1L );
  VERIFY( *oi == 1 );

  std::optional<unsigned> ou(std::move(oi));
  VERIFY( *ou == 1u );
  VERIFY( oi.has_value() && *oi == 1 );

  return true;
}

static_assert( test_cons() );
