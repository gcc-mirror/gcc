// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

template<class Clock, class Dur>
void
do_test_leap_second_parsing()
{
  std::chrono::time_point<Clock, Dur> tp, tp2;

  std::string subs;
  if constexpr (std::ratio_less_v<typename Dur::period, std::ratio<1, 1>>)
    subs = ".05";

  std::istringstream ss("20081231-23:59:60" + subs + " ");
  ss >> std::chrono::parse("%Y%m%d-%T ", tp);

  if constexpr (std::is_same_v<Clock, std::chrono::local_t>)
    VERIFY( ss ); // We allow parsing "23:59:60" as local_time.
  else
  {
    if constexpr (std::is_same_v<Clock, std::chrono::utc_clock>)
    {
      // Entire input was consumed.
      VERIFY( ss );
      VERIFY( ss.eof() );
      // The parsed value is the leap second inserted on Jan 1 2017.
      VERIFY( std::chrono::get_leap_second_info(tp).is_leap_second );
    }
    else
      VERIFY( !ss ); // Other clocks do not allow "HH:MM:60"

    std::chrono::minutes offset(9999);
    ss.clear();
    ss.str("20081231-22:59:60" + subs + " -0100"); // Same time at -1h offset.
    ss >> std::chrono::parse("%Y%m%d-%T %z", tp2, offset);

    if constexpr (std::is_same_v<Clock, std::chrono::utc_clock>)
    {
      VERIFY( ss );
      VERIFY( tp2 == tp );
      VERIFY( offset == std::chrono::minutes(-60) );
    }
    else
      VERIFY( !ss );
  }
}

template<class Clock>
void
test_leap_second_parsing()
{
  do_test_leap_second_parsing<Clock, std::chrono::milliseconds>();
  do_test_leap_second_parsing<Clock, std::chrono::seconds>();
}

int main()
{
  test_leap_second_parsing<std::chrono::system_clock>();
  test_leap_second_parsing<std::chrono::utc_clock>();
  test_leap_second_parsing<std::chrono::tai_clock>();
  test_leap_second_parsing<std::chrono::gps_clock>();
  test_leap_second_parsing<std::chrono::file_clock>();
  test_leap_second_parsing<std::chrono::local_t>();
}
