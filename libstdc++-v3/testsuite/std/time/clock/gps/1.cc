// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::chrono;

  gps_seconds gps_epoch{0s};
  utc_seconds gps_as_utc{sys_days{1980y/January/Sunday[1]}.time_since_epoch() + 9s};

  VERIFY( clock_cast<utc_clock>(gps_epoch) == gps_as_utc );
  VERIFY( gps_epoch == clock_cast<gps_clock>(gps_as_utc) );

  tai_seconds tai_epoch{0s};
  VERIFY( clock_cast<tai_clock>(clock_cast<gps_clock>(tai_epoch)) == tai_epoch );
}

void
test02()
{
  using namespace std::chrono;

  sys_days d{2022y/November/12};
  VERIFY( clock_cast<system_clock>(clock_cast<gps_clock>(d)) == d );
  gps_seconds t(1234567s);
  VERIFY( clock_cast<gps_clock>(clock_cast<system_clock>(t)) == t );
  VERIFY( clock_cast<gps_clock>(clock_cast<utc_clock>(t)) == t );
}

int main()
{
  test01();
  test02();
}
