// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <format>
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
}

int main()
{
  test01();
}
