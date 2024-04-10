// { dg-do run { target c++20 } }

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

template<class Clock>
void
test_leap_second_parsing()
{
  std::chrono::time_point<Clock, std::chrono::milliseconds> tp, tp2;

  std::istringstream ss("20161231-23:59:60.05");
  ss >> std::chrono::parse("%Y%m%d-%T", tp);

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

    ss.clear();
    ss.str("20161231-22:59:60.05 -0100"); // Same time at -1h offset.
    ss >> std::chrono::parse("%Y%m%d-%T %z", tp2);

    if constexpr (std::is_same_v<Clock, std::chrono::utc_clock>)
    {
      VERIFY( ss );
      VERIFY( tp2 == tp );
    }
    else
      VERIFY( !ss );
  }
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
