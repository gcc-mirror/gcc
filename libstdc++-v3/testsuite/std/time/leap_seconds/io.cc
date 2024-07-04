// { dg-do run { target c++20 } }
// { dg-require-effective-target tzdb }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <sstream>
#include <format>
#include <testsuite_hooks.h>

void
test_output()
{
  using namespace std::chrono;

  std::ostringstream out;
  out << '\n';

  for (auto& l : get_tzdb().leap_seconds)
  if (l <= sys_days{2018y/March/17d})
    out << l.date() << ": " << l.value() << '\n';

  VERIFY( out.str() == R"(
1972-07-01 00:00:00: 1s
1973-01-01 00:00:00: 1s
1974-01-01 00:00:00: 1s
1975-01-01 00:00:00: 1s
1976-01-01 00:00:00: 1s
1977-01-01 00:00:00: 1s
1978-01-01 00:00:00: 1s
1979-01-01 00:00:00: 1s
1980-01-01 00:00:00: 1s
1981-07-01 00:00:00: 1s
1982-07-01 00:00:00: 1s
1983-07-01 00:00:00: 1s
1985-07-01 00:00:00: 1s
1988-01-01 00:00:00: 1s
1990-01-01 00:00:00: 1s
1991-01-01 00:00:00: 1s
1992-07-01 00:00:00: 1s
1993-07-01 00:00:00: 1s
1994-07-01 00:00:00: 1s
1996-01-01 00:00:00: 1s
1997-07-01 00:00:00: 1s
1999-01-01 00:00:00: 1s
2006-01-01 00:00:00: 1s
2009-01-01 00:00:00: 1s
2012-07-01 00:00:00: 1s
2015-07-01 00:00:00: 1s
2017-01-01 00:00:00: 1s
)" );
}

int main()
{
  test_output();
}
