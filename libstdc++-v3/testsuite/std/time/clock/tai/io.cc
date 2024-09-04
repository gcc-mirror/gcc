// { dg-do run { target c++20 } }
// { dg-timeout-factor 2 }

#include <chrono>
#include <format>
#include <testsuite_hooks.h>

void
test_ostream()
{
  using std::format;
  using namespace std::chrono;

  auto st = sys_days{2000y/January/1};
  auto tt = clock_cast<tai_clock>(st);

  auto s = format("{0:%F %T %Z} == {1:%F %T %Z}", st, tt);
  VERIFY( s == "2000-01-01 00:00:00 UTC == 2000-01-01 00:00:32 TAI" );

  s = std::format("{:=>21}", tt);
  VERIFY( s == "==2000-01-01 00:00:32" );
}

void
test_format()
{
  using std::format;
  using namespace std::chrono;

  auto st = sys_days{2000y/January/1};
  auto tt = clock_cast<tai_clock>(st);
  auto s = std::format("{}", tt);
  VERIFY( s == "2000-01-01 00:00:32" );

  // PR libstdc++/113500
  s = std::format("{}", tt + 150ms);
  VERIFY( s == "2000-01-01 00:00:32.150" );

  s = std::format("{:%Z %z %Ez %Oz}", tt);
  VERIFY( s == "TAI +0000 +00:00 +00:00" );

  s = std::format("{}", tai_seconds{});
  VERIFY( s == "1958-01-01 00:00:00" );
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min + 3s;
  tai_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("8/9/23 214440 +1 BST#");
  VERIFY( is >> parse("%D %2H%2M%2S %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<tai_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );

  // Test round trip
  std::stringstream ss;
  ss << clock_cast<tai_clock>(expected) << " TAI 0123";
  VERIFY( ss >> parse("%F %T %Z %z", tp, abbrev, offset) );
  VERIFY( ! ss.eof() );
  VERIFY( (tp + offset) == clock_cast<tai_clock>(expected) );
  VERIFY( abbrev == "TAI" );
  VERIFY( offset == (1h + 23min) );

  ss.str("");
  ss << tai_seconds{};
  VERIFY( ss >> parse("%F %T", tp) );
  VERIFY( tp.time_since_epoch() == 0s );
}

int main()
{
  test_ostream();
  test_format();
  test_parse();
}
