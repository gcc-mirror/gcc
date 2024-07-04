// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }
// { dg-require-namedlocale "en_US.ISO8859-1" }

// PR libstdc++/114244 Need to use round when parsing fractional seconds

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_pr114244()
{
  using namespace std::chrono;
  seconds s;
  milliseconds ms;
  microseconds us;

  std::istringstream is;

  is.clear();
  is.str("0.002");
  VERIFY( is >> parse("%S", ms) );
  VERIFY( ms == 2ms ); // not 1ms

  is.imbue(std::locale(ISO_8859(1,en_US)));
  is.clear();
  is.str("0.002");
  VERIFY( is >> parse("%S", us) );
  VERIFY( us == 2000us ); // not 1999us
}

int main()
{
  test_pr114244();
}
