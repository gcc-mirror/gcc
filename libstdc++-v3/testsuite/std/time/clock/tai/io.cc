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
}

void
test_parse()
{
  using namespace std::chrono;
  const sys_seconds expected = sys_days(2023y/August/9) + 20h + 44min + 3s;
  tai_seconds tp;

  minutes offset;
  std::string abbrev;
  std::istringstream is("8/9/23 214403 +1 BST#");
  VERIFY( is >> parse("%D %2H%2M%2S %Oz %Z", tp, abbrev, offset) );
  VERIFY( ! is.eof() );
  VERIFY( tp == clock_cast<tai_clock>(expected) );
  VERIFY( abbrev == "BST" );
  VERIFY( offset == 60min );
}

int main()
{
  test_ostream();
  test_parse();
}
