// { dg-do run }
// LWG 4084. std::fixed ignores std::uppercase
// PR libstdc++/114862 std::uppercase not applying to nan's and inf's

#include <sstream>
#include <limits>
#include <iomanip>
#include <testsuite_hooks.h>

void
test_nan()
{
  std::ostringstream out;
  double nan = std::numeric_limits<double>::quiet_NaN();
  out << std::fixed;
  out << ' ' << nan << ' ' << -nan;
  out << std::uppercase;
  out << ' ' << nan << ' ' << -nan;
  out << std::showpoint;
  out << ' ' << nan << ' ' << -nan;
  out << std::showpos;
  out << ' ' << nan << ' ' << -nan;
  VERIFY( out.str() == " nan -nan NAN -NAN NAN -NAN +NAN -NAN" );
}

void
test_inf()
{
  std::ostringstream out;
  double inf = std::numeric_limits<double>::infinity();
  out << std::fixed;
  out << ' ' << inf << ' ' << -inf;
  out << std::uppercase;
  out << ' ' << inf << ' ' << -inf;
  out << std::showpoint;
  out << ' ' << inf << ' ' << -inf;
  out << std::showpos;
  out << ' ' << inf << ' ' << -inf;
  VERIFY( out.str() == " inf -inf INF -INF INF -INF +INF -INF" );
}

int main()
{
  test_nan();
  test_inf();
}
