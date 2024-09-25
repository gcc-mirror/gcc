// { dg-options "-ffreestanding" }
// { dg-do compile { target c++11 } }

#include <chrono>

using namespace std::chrono;

milliseconds
test_duration()
{
  seconds sec{1};
  sec = sec + -sec;
  return duration_cast<milliseconds>(sec + microseconds{100});
}

struct Clock
{
  using rep = long;
  using period = std::ratio<1,10>;
  using duration = std::chrono::duration<rep, period>;
  using time_point = std::chrono::time_point<Clock>;

  static const bool is_steady = true;

  static time_point now() noexcept
  {
    static time_point tick{duration{0}};
    return tick + tick.time_since_epoch();
  }
};

Clock::time_point
test_time_point()
{
  auto t = Clock::now() + milliseconds{1};
  return time_point_cast<Clock::duration>(t);
}

#if __cplusplus > 202002L
static_assert( is_clock_v<Clock> );

bool
test_calendar()
{
  auto t = test_time_point();
  t = clock_cast<Clock>(t);
  local_days d{floor<days>(t + 1h + 1min + 1s).time_since_epoch()};
  year_month_day ymd{d};
  weekday w{d};
  return w.ok();
}
#endif
