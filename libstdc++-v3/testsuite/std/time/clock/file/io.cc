// { dg-options "-std=gnu++20" }
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
}

int main()
{
  test_ostream();
  test_format();
}
