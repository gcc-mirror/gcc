// { dg-options "-std=gnu++20" }
// { dg-do run { target c++20 } }

#include <chrono>
#include <testsuite_hooks.h>

void
test01()
{
  using namespace std::chrono;

  tai_seconds tai_epoch{0s};
  utc_seconds tai_as_utc{sys_days{1958y/January/1}.time_since_epoch() - 10s};

  VERIFY( clock_cast<utc_clock>(tai_epoch) == tai_as_utc );
  VERIFY( tai_epoch == clock_cast<tai_clock>(tai_as_utc) );

  sys_days y2k{2000y/January/1};
  tai_seconds y2k_as_tai{clock_cast<tai_clock>(y2k)};
  utc_seconds y2k_as_utc = utc_clock::from_sys(y2k);
  VERIFY( clock_cast<utc_clock>(y2k_as_tai) == y2k_as_utc );
  VERIFY( y2k_as_tai == clock_cast<tai_clock>(y2k_as_utc) );
}

void
test02()
{
  using namespace std::chrono;

  sys_days d{2022y/November/12};
  VERIFY( clock_cast<system_clock>(clock_cast<tai_clock>(d)) == d );
  tai_seconds t(1234567s);
  VERIFY( clock_cast<tai_clock>(clock_cast<system_clock>(t)) == t );
  VERIFY( clock_cast<tai_clock>(clock_cast<utc_clock>(t)) == t );
}

int main()
{
  test01();
  test02();
}
