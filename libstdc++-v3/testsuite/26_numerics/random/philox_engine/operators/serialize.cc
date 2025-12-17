// { dg-do run { target c++26 } }

// N5014 29.5.4.5 Class Template philox_engine

#include <sstream>
#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::stringstream str;
  std::philox_engine<std::uint_fast32_t,
		     32, 4, 10, 0xCD9E8D57,
		     0x9E3779B9, 0xD2511F53,
		     0xBB67AE85> x, y;

  x();
  str << x;

  VERIFY ( !(x == y) );
  str >> y;
  VERIFY ( x == y );
  for (unsigned long i = 0; i < 100; ++i)
  {
    VERIFY (x() == y());
  }
  str.clear();
  str << y;
  x();
  x();
  x();
  str >> x;
  VERIFY ( x == y );
  for (unsigned long i = 0; i < 1000; ++i)
  {
    VERIFY (x() == y());
  }
}

int
main()
{
  test01();
}
