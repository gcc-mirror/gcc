// { dg-do run { target c++26 } }

// N5014 29.5.4.5 Class Template philox_engine

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::philox_engine<std::uint_fast32_t,
		     32, 4, 10, 0xCD9E8D57,
		     0x9E3779B9, 0xD2511F53,
		     0xBB67AE85> x, y;

  VERIFY ( x == y);
  x.discard(100);
  y.discard(100);

  VERIFY (x == y);

  x.discard(2);
  VERIFY (x != y);
  y.discard(2);
  VERIFY (x == y);
}

int
main()
{
  test01();
}
