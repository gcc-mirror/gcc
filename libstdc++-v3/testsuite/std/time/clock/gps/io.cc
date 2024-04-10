// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <format>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
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

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min + 3s;
  gps_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("2023-8-9 21:44:3 +1 BST#");
  VERIFY( is >> parse("%9F %T %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<gps_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
