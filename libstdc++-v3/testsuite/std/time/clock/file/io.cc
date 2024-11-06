// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using namespace std::chrono;

  file_time<file_clock::duration> t = file_clock::now();
  std::ostringstream ss1, ss2;
  ss1 << floor<seconds>(t);
  ss2 << floor<seconds>(clock_cast<system_clock>(t));
  VERIFY( ss1.str() == ss2.str() );
}

void
test_format()
{
  using namespace std::chrono;
  auto t = file_clock::now();

  auto s = std::format("{}", t);
  std::ostringstream ss;
  ss << t;
  VERIFY( s == ss.str() );

  // PR libstdc++/113500
  auto ft = clock_cast<file_clock>(sys_days(2024y/January/21)) + 0ms + 2.5s;
  s = std::format("{}", ft);
  VERIFY( s == "2024-01-21 00:00:02.500");

  const std::chrono::file_time<std::chrono::seconds> t0{};
  s = std::format("{:%Z %z %Ez %Oz}", t0);
  VERIFY( s == "UTC +0000 +00:00 +00:00" );

  s = std::format("{}", t0);
  // chrono::file_clock epoch is unspecified, so this is libstdc++-specific.
  VERIFY( s == "2174-01-01 00:00:00" );
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min;
  file_time<seconds> tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("002023-08-09 21:44 +01 BST!");
  VERIFY( is >> parse("%6F %R %z %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<file_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  // Test round trip
  std::stringstream ss;
  ss << clock_cast<file_clock>(expected) << " 0123456";
  VERIFY( ss >> parse("%F %T %z%Z", tp, abbrev, offset) );
  VERIFY( ss.eof() );
  VERIFY( (tp + offset) == clock_cast<file_clock>(expected) );
  VERIFY( abbrev == "456" );
  VERIFY( offset == (1h + 23min) );

  // Test rounding
  ss.clear();
  ss.str("2224-09-06 23");
  file_time<days> d;
  ss >> parse("%F %H", d); // Should be truncated to start of day, not rounded.
  ss.str("");
  ss << d;
  VERIFY( ss.str() == "2224-09-06 00:00:00" );
  ss.str("1969-12-31 23");
  ss >> parse("%F %H", d); // Should be truncated to start of day, not rounded.
  ss.str("");
  ss << d;
  VERIFY( ss.str() == "1969-12-31 00:00:00" );

  file_time<duration<long, std::ratio<10>>> ds; // decaseconds
  ss.str("2224-09-06 15:07:06");
  ss >> parse("%F %T", ds); // Should be rounded to nearest decasecond.
  ss << ds;
  VERIFY( ss.str() == "2224-09-06 15:07:10" );

  ss.str("");
  ss << file_time<seconds>{};
  VERIFY( ss >> parse("%F %T", tp) );
  VERIFY( tp.time_since_epoch() == 0s );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
