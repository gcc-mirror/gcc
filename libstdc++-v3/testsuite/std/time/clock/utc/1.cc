// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::chrono;

  // [time.clock.utc.overview]

  auto epoch = sys_seconds{sys_days{1970y/January/1}};
  auto utc_epoch = clock_cast<utc_clock>(epoch);
  VERIFY( utc_epoch.time_since_epoch() == 0s );

  auto y2k = sys_seconds{sys_days{2000y/January/1}};
  auto utc_y2k = clock_cast<utc_clock>(y2k);
  VERIFY( utc_y2k.time_since_epoch() == 946'684'822s );
}

void
test02()
{
  using namespace std::chrono;

  // [time.clock.utc.members]

  auto t = sys_days{July/1/2015} - 2ns;
  auto u = utc_clock::from_sys(t);
  VERIFY(u.time_since_epoch() - t.time_since_epoch() == 25s);
  t += 1ns;
  u = utc_clock::from_sys(t);
  VERIFY(u.time_since_epoch() - t.time_since_epoch() == 25s);
  t += 1ns;
  u = utc_clock::from_sys(t);
  VERIFY(u.time_since_epoch() - t.time_since_epoch() == 26s);
  t += 1ns;
  u = utc_clock::from_sys(t);
  VERIFY(u.time_since_epoch() - t.time_since_epoch() == 26s);
}

int main()
{
  test01();
  test02();
}
