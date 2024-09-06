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

  s = std::format("{:%Z %z %Ez %Oz}", gt);
  VERIFY( s == "GPS +0000 +00:00 +00:00" );

  s = std::format("{}", gps_seconds{});
  VERIFY( s == "1980-01-06 00:00:00" );
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 43min + 45s;
  gps_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("2023-8-9 21:44:3 +1 BST#");
  VERIFY( is >> parse("%9F %T %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<gps_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  // Test round trip
  std::stringstream ss;
  ss << clock_cast<gps_clock>(expected) << " GPS -1234";
  VERIFY( ss >> parse("%F %T %Z %z", tp, abbrev, offset) );
  VERIFY( ! ss.eof() );
  VERIFY( (tp + offset) == clock_cast<gps_clock>(expected) );
  VERIFY( abbrev == "GPS" );
  VERIFY( offset == -(12h + 34min) );

  // Test rounding
  ss.clear();
  ss.str("2224-09-06 23");
  gps_time<days> d;
  ss >> parse("%F %H", d); // Should be truncated to start of day, not rounded.
  ss.str("");
  ss << d;
  VERIFY( ss.str() == "2224-09-06 00:00:00" );
  ss.str("1969-12-31 23");
  ss >> parse("%F %H", d); // Should be truncated to start of day, not rounded.
  ss.str("");
  ss << d;
  VERIFY( ss.str() == "1969-12-31 00:00:00" );

  gps_time<duration<long, std::ratio<10>>> ds; // decaseconds
  ss.str("2224-09-06 15:07:06");
  ss >> parse("%F %T", ds); // Should be rounded to nearest decasecond.
  ss << ds;
  VERIFY( ss.str() == "2224-09-06 15:07:10" );

  ss.str("");
  ss << gps_seconds{};
  VERIFY( ss >> parse("%F %T", tp) );
  VERIFY( tp.time_since_epoch() == 0s );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
