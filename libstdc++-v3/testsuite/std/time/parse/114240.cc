// { dg-do run { target c++20 } }

// PR libstdc++/114240 sys_days not being parsed with only a date in the stream

#include <chrono>
#include <sstream>
#include <testsuite_hooks.h>

template<class Clock>
void
test_parse_date_only()
{
  using namespace std::chrono;

  using CDays = time_point<Clock, days>;
  CDays td;
  std::istringstream is("2024-03-05");
  VERIFY( is >> parse("%Y-%m-%d ", td) );
  if constexpr (std::is_same_v<Clock, std::chrono::local_t>)
    VERIFY( td == static_cast<local_time<days>>(2024y/March/5) );
  else
  {
    auto tp = clock_cast<Clock>(sys_days(2024y/March/5));
    VERIFY( td == time_point_cast<days>(tp) );
  }
}

int main()
{
  test_parse_date_only<std::chrono::system_clock>();
  test_parse_date_only<std::chrono::utc_clock>();
  test_parse_date_only<std::chrono::tai_clock>();
  test_parse_date_only<std::chrono::gps_clock>();
  test_parse_date_only<std::chrono::file_clock>();
  test_parse_date_only<std::chrono::local_t>();
}
