// { dg-do run }
// PR libstdc++/109758 std::abs(__float128) doesn't support NaN

#include <cmath>
#include <testsuite_hooks.h>

#if !defined(__STRICT_ANSI__) && defined(_GLIBCXX_USE_FLOAT128)
void
test_nan()
{
  __float128 nan = __builtin_nanl("");
  VERIFY( !__builtin_signbit(std::abs(nan)) );
  VERIFY( !__builtin_signbit(std::abs(-nan)) );
}

void
test_zero()
{
  __float128 zero = 0.0;
  VERIFY( !__builtin_signbit(std::abs(zero)) );
  VERIFY( !__builtin_signbit(std::abs(zero * -2.0)) );
}

void
test_neg()
{
  VERIFY( std::abs((__float128)-1.0) == -1.0 );
  VERIFY( std::abs((__float128)-2e9) == -2e9 );
  VERIFY( std::abs((__float128)-3e-4) == 3e-4 );
}

void
test_inf()
{
  __float128 inf = __builtin_huge_vall();
  VERIFY( std::abs(inf) == inf );
  VERIFY( std::abs(-inf) == inf );
}

#if __cplusplus >= 201103L
static_assert( std::abs((__float128)-1.0) == (__float128)1.0,
	       "std::abs(__float128) is usable in constant expressions" );
#endif

int main()
{
  test_nan();
  test_zero();
}
#else
int main() { }
#endif
