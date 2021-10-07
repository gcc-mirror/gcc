// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::chrono;

  auto epoch = sys_seconds{sys_days{1970y/January/1}};
  auto utc_epoch = clock_cast<utc_clock>(epoch);
  VERIFY( utc_epoch.time_since_epoch() == 0s );

  auto y2k = sys_seconds{sys_days{2000y/January/1}};
  auto utc_y2k = clock_cast<utc_clock>(y2k);
  VERIFY( utc_y2k.time_since_epoch() == 946'684'822s );
}

int main()
{
  test01();
}
