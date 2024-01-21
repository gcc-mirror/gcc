// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <format>
#include <sstream>
#include <testsuite_hooks.h>

void
test01()
{
  using std::format;
  using namespace std::chrono;

  auto st = sys_days{2000y/January/1};
  auto gt = clock_cast<gps_clock>(st);

  auto s = format("{0:%F %T %Z} == {1:%F %T %Z}", st, gt);
  VERIFY( s == "2000-01-01 00:00:00 UTC == 2000-01-01 00:00:13 GPS" );

  std::ostringstream ss;
  ss << gt;
  VERIFY( ss.str() == "2000-01-01 00:00:13" );

  gps_time<duration<float>> gtf = gt;
  ss.str("");
  ss.clear();
  ss << (gps_time<duration<long double>>(gt) + 20ms);
  VERIFY( ss.str() == "2000-01-01 00:00:13.020" );
}

void
test_format()
{
  using std::format;
  using namespace std::chrono;

  auto st = sys_days{2000y/January/1};
  auto gt = clock_cast<gps_clock>(st);
  auto s = std::format("{}", gt);
  VERIFY( s == "2000-01-01 00:00:13" );

  // PR libstdc++/113500
  s = std::format("{}", gt + 150ms + 10.5s);
  VERIFY( s == "2000-01-01 00:00:23.650" );
}

int main()
{
  test01();
  test_format();
}
