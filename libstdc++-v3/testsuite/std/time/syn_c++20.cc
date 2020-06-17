// Copyright (C) 2020 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <chrono>

#ifndef __cpp_lib_chrono
# error "Feature test macro for chrono is missing in <chrono>"
// FIXME
// #elif __cpp_lib_chrono < 201907L
// # error "Feature test macro for chrono has wrong value in <chrono>"
#endif

namespace __gnu_test
{
  // Check for the new additions to <chrono> in C++20

  using std::chrono::is_clock;
  using std::chrono::is_clock_v;

  using std::chrono::days;
  using std::chrono::weeks;
  using std::chrono::years;
  using std::chrono::months;

  using std::chrono::sys_time;
  using std::chrono::sys_seconds;
  using std::chrono::sys_days;

  // FIXME
#if 0
  using std::chrono::utc_clock;
  using std::chrono::utc_time;
  using std::chrono::utc_seconds;

  using std::chrono::leap_second_info;
  using std::chrono::get_leap_second_info;

  using std::chrono::tai_clock;
  using std::chrono::tai_time;
  using std::chrono::tai_seconds;

  using std::chrono::gps_clock;
  using std::chrono::gps_time;
  using std::chrono::gps_seconds;
#endif

  using std::chrono::file_clock;
  using std::chrono::file_time;

  using std::chrono::local_t;
  using std::chrono::local_time;
  using std::chrono::local_seconds;
  using std::chrono::local_days;

  // FIXME
#if 0
  using std::chrono::clock_time_conversion;
  using std::chrono::clock_cast;

  using std::chrono::last_spec;

  using std::chrono::day;
  using std::chrono::month;
  using std::chrono::year;
  using std::chrono::weekday;
  using std::chrono::weekday_indexed;
  using std::chrono::weekday_last;
  using std::chrono::month_day;
  using std::chrono::month_day_last;
  using std::chrono::month_weekday;
  using std::chrono::month_weekday_last;
  using std::chrono::year_month;
  using std::chrono::year_month_day;
  using std::chrono::year_month_day_last;
  using std::chrono::year_month_weekday;
  using std::chrono::year_month_weekday_last;
  using std::chrono::year_month;
  using std::chrono::year_month_day;

  using std::chrono::hh_mm_ss;
  using std::chrono::is_am;
  using std::chrono::is_pm;
  using std::chrono::make12;
  using std::chrono::make24;

  using std::chrono::tzdb;
  using std::chrono::tzdb_list;
  using std::chrono::get_tzdb;
  using std::chrono::get_tzdb_list;
  using std::chrono::locate_zone;
  using std::chrono::current_zone;

  using std::chrono::reload_tzdb;
  using std::chrono::remote_version;

  using std::chrono::nonexistent_local_time;
  using std::chrono::ambiguous_local_time;

  using std::chrono::sys_info;
  using std::chrono::local_info;

  using std::chrono::choose;
  using std::chrono::time_zone;

  using std::chrono::zoned_traits;
  using std::chrono::zoned_time;
  using std::chrono::zoned_seconds;

  using std::chrono::leap_second;

  using std::chrono::time_zone_link;

  using std::chrono::local_time_format;

  using std::chrono::parse;

  using std::chrono::last;
  using std::chrono::Sunday;
  using std::chrono::Monday;
  using std::chrono::Tuesday;
  using std::chrono::Wednesday;
  using std::chrono::Thursday;
  using std::chrono::Friday;
  using std::chrono::Saturday;

  using std::chrono::January;
  using std::chrono::February;
  using std::chrono::March;
  using std::chrono::April;
  using std::chrono::May;
  using std::chrono::June;
  using std::chrono::July;
  using std::chrono::August;
  using std::chrono::September;
  using std::chrono::October;
  using std::chrono::November;
  using std::chrono::December;

  using std::chrono_literals::operator""d;
  using std::chrono_literals::operator""y;
#endif

  template<typename>
    constexpr bool is_duration = false;
  template<typename R, typename P>
    constexpr bool is_duration<std::chrono::duration<R, P>> = true;

  static_assert( is_duration<days> );
  static_assert( is_duration<weeks> );
  static_assert( is_duration<years> );
  static_assert( is_duration<months> );

  template<typename D, typename P>
    constexpr bool has_period = std::is_same_v<typename D::period, P>;

  using std::ratio;
  using std::ratio_multiply;
  using std::ratio_divide;
  using std::chrono::hours;
  static_assert( has_period<days, ratio_multiply<ratio<24>, hours::period>> );
  static_assert( has_period<weeks, ratio_multiply<ratio<7>, days::period>> );
  static_assert( has_period<years,
			    ratio_multiply<ratio<146097, 400>, days::period>> );
  static_assert( has_period<months, ratio_divide<years::period, ratio<12>>> );

  template<typename>
    constexpr bool is_time_point = false;
  template<typename C, typename D>
    constexpr bool is_time_point<std::chrono::time_point<C, D>> = true;

  static_assert( is_time_point<sys_time<std::chrono::milliseconds>> );
  static_assert( is_time_point<sys_seconds> );
  static_assert( is_time_point<sys_days> );

  static_assert( std::is_class_v<local_t> );
  static_assert( is_time_point<local_time<std::chrono::milliseconds>> );
  static_assert( is_time_point<local_seconds> );
  static_assert( is_time_point<local_days> );

  static_assert( std::is_class_v<file_clock> );
  static_assert( is_time_point<file_time<std::chrono::milliseconds>> );
}
