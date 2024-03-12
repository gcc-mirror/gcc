// { dg-do run { target c++20 } }
// { dg-additional-options "-DHAVE_TZDB" { target tzdb } }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono_literals;

void
test_before()
{
  // No leaps seconds defined before the epoch.
  auto s = std::chrono::utc_seconds(-1s);
  auto lsi = get_leap_second_info(s);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == 0s );

  auto ms = std::chrono::utc_time<std::chrono::milliseconds>(s - 5500ms);
  lsi = get_leap_second_info(ms);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == 0s );
}

void
test_after()
{
#ifdef HAVE_TZDB
  const auto& leaps = std::chrono::get_tzdb().leap_seconds;
  std::chrono::seconds sum(0);
  for (auto leap : leaps)
    sum += leap.value();

  // After the last defined leap second.
  auto last = leaps.back().date().time_since_epoch();
  auto ut = std::chrono::utc_time<std::chrono::milliseconds>(last + 72h + 10ms);
  auto lsi = get_leap_second_info(ut);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == sum );
#endif
}

void
test_between()
{
  std::chrono::sys_days st(1995y/9/4);
  auto ut = std::chrono::clock_cast<std::chrono::utc_clock>(st);
  auto lsi = get_leap_second_info(ut);
  VERIFY( lsi.is_leap_second == false );
  VERIFY( lsi.elapsed == 19s );
}

void
test_during()
{
#ifdef HAVE_TZDB
  // Verify that leap_second_info::is_leap_second is true for each leap second.
  const auto& leaps = std::chrono::get_tzdb().leap_seconds;
  for (const auto& leap : leaps)
  {
    // N.B. this assumes all leap seconds are positive:
    std::chrono::seconds elapsed(&leap - &leaps.front());
    std::chrono::utc_seconds ut(leap.date().time_since_epoch() + elapsed);
    auto lsi = get_leap_second_info(ut);
    VERIFY( lsi.is_leap_second == true );
    VERIFY( lsi.elapsed == elapsed + 1s );
    lsi = get_leap_second_info(ut + 999ms);
    VERIFY( lsi.is_leap_second == true );
    VERIFY( lsi.elapsed == elapsed + 1s );
  }
#endif
}

int main()
{
  test_before();
  test_after();
  test_between();
  test_during();
}
