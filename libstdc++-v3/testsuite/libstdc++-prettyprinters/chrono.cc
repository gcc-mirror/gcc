// { dg-options "-g -O0 -std=gnu++2a" }
// { dg-do run { target c++2a } }
// { dg-additional-options "-DTEST_ZONED_TIME" { target tzdb } }

// Copyright The GNU Toolchain Authors.
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

#include <chrono>
#include <iostream>

int
main()
{
  using namespace std::chrono;

  seconds just_a_sec(1);
  // { dg-final { note-test just_a_sec {std::chrono::duration = { 1s }} } }
  microseconds just_a_moment(5001);
  // { dg-final { note-test just_a_moment {std::chrono::duration = { 5001us }} } }
  duration<float, std::ratio<22, 7>> pie(2.72f);
  // { dg-final { note-test pie {std::chrono::duration = { 2.72[22/7]s }} } }

  sys_seconds half_past_epoch(1800s);
  // { dg-final { note-test half_past_epoch {std::chrono::sys_time = { 1800s [1970-01-01 00:30:00] }} } }
  utc_time utc(467664h);
  // { dg-final { note-test utc {std::chrono::utc_time = { 467664h }} } }

#if _GLIBCXX_USE_CXX11_ABI && defined TEST_ZONED_TIME
  zoned_time<milliseconds> zt("Europe/London", half_past_epoch);
  // { dg-final { note-test zt {std::chrono::zoned_time = { "Europe/London" 1800000ms [1970-01-01 00:30:00] }} { target cxx11_abi } } }
#endif

  [[maybe_unused]] day ninth(9);
  // { dg-final { note-test ninth {9} } }
  [[maybe_unused]] month may = May;
  // { dg-final { note-test may {May} } }
  auto twentytwentythree = 2023y;
  // { dg-final { note-test twentytwentythree {2023y} } }
  [[maybe_unused]] weekday tues = Tuesday;
  // { dg-final { note-test tues {Tuesday} } }
  [[maybe_unused]] weekday_indexed second_tues = Tuesday[2];
  // { dg-final { note-test second_tues {Tuesday[2]} } }
  [[maybe_unused]] weekday_last last_tues = Tuesday[last];
  // { dg-final { note-test last_tues {Tuesday[last]} } }
  [[maybe_unused]] month_day midsummer = June/21;
  // { dg-final { note-test midsummer {June/21} } }
  [[maybe_unused]] month_day_last end_jan = January/last;
  // { dg-final { note-test end_jan {January/last} } }
  [[maybe_unused]] month_weekday handsel = January/Monday[1];
  // { dg-final { note-test handsel {January/Monday[1]} } }
  [[maybe_unused]] month_weekday_last reek = July/Sunday[last];
  // { dg-final { note-test reek {July/Sunday[last]} } }
  [[maybe_unused]] year_month feb_2023 = 2023y/February;
  // { dg-final { note-test feb_2023 {2023y/February} } }
  [[maybe_unused]] year_month_day barbican = September/17/1997y;
  // { dg-final { note-test barbican {1997y/September/17} } }
  [[maybe_unused]] year_month_day_last party_like = 1999y/December/last;
  // { dg-final { note-test party_like {1999y/December/last} } }
  [[maybe_unused]] year_month_weekday easter = 2023y/April/Sunday[2];
  // { dg-final { note-test easter {2023y/April/Sunday[2]} } }
  [[maybe_unused]] year_month_weekday_last donnerstag = 2017y/July/Thursday[last];
  // { dg-final { note-test donnerstag {2017y/July/Thursday[last]} } }

  hh_mm_ss<seconds> hms(4h + 3min + 2s);
  // { dg-final { note-test hms {04:03:02} } }

  hh_mm_ss<nanoseconds> hms_nano(-14h - 13min - 12s - 11ns);
  // { dg-final { note-test hms_nano {-14:13:12.000000011} } }

  std::cout << "\n";
  return 0;			// Mark SPOT
}

// { dg-final { gdb-test SPOT } }
