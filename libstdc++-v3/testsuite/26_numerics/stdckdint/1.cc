// { dg-do run { target c++26 } }

#include <stdckdint.h>

#if __STDC_VERSION_STDCKDINT_H__ != 202311L
# error "__STDC_VERSION_STDCKDINT_H__ not defined correctly in <stdckdint.h>"
#endif

#include <limits.h>
#include <testsuite_hooks.h>

void
test_add()
{
  int result;
  bool overflow;

  overflow = ::ckd_add(&result, (unsigned)INT_MAX, 1LL);
  VERIFY( overflow );
  VERIFY( result == INT_MIN );

  overflow = ::ckd_add(&result, (long long)INT_MIN, -1);
  VERIFY( overflow );
  VERIFY( result == INT_MAX );

  overflow = ::ckd_add(&result, 99u, 100ll);
  VERIFY( ! overflow );
  VERIFY( result == 199 );
}

void
test_sub()
{
  int result;
  bool overflow;

  overflow = ::ckd_sub(&result, -1, -5);
  VERIFY( ! overflow );
  VERIFY( result == 4 );
}

void
test_mul()
{
  long long result;
  bool overflow;

  overflow = ::ckd_mul(&result, INT_MIN, -1);
  VERIFY( ! overflow );
  VERIFY( result == -(long long)INT_MIN );

  unsigned uresult;
  overflow = ::ckd_mul(&uresult, INT_MIN, -1);
  VERIFY( ! overflow );
  VERIFY( result == (unsigned)INT_MAX + 1u );
}

int main()
{
  test_add();
  test_sub();
  test_mul();
}
