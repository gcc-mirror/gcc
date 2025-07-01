// { dg-do compile { target c++11 } }

#include <testsuite_hooks.h>

struct X { explicit operator void*() const { return nullptr; } };

void
test_VERIFY(int i)
{
  // This should not be parsed as a function type bool(bool(i)):
  VERIFY( bool(i) );

  // This should not produce warnings about lambda in unevaluated context:
  VERIFY( []{ return 1; }() );

  // Only one expression allowed:
  VERIFY(1, 2); // { dg-error "in expansion of macro" }
  // { dg-error "compound expression in functional cast" "" { target *-*-* } 0 }

  // A scoped enum is not contextually convertible to bool:
  enum class E { E0 };
  VERIFY( E::E0 ); // { dg-error "could not convert" }

  // explicit conversion to void* is not contextually convertible to bool:
  X x;
  VERIFY( x ); // { dg-error "in expansion of macro" }
  // { dg-error "invalid cast .* to type 'bool'" "" { target *-*-* } 0 }
}
