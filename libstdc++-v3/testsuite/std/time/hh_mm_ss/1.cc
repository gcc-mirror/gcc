// { dg-do compile { target c++20 } }

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// Class template hh_mm_ss [time.hh_mm_ss]

#include <chrono>

constexpr void
constexpr_hh_mm_ss()
{
  using namespace std::chrono;
  using std::ratio;

  static_assert(hh_mm_ss<hours>::fractional_width == 0);
  static_assert(hh_mm_ss<minutes>::fractional_width == 0);
  static_assert(hh_mm_ss<seconds>::fractional_width == 0);
  static_assert(hh_mm_ss<milliseconds>::fractional_width == 3);
  static_assert(hh_mm_ss<microseconds>::fractional_width == 6);
  static_assert(hh_mm_ss<nanoseconds>::fractional_width == 9);
  static_assert(hh_mm_ss<duration<int, ratio<1, 2>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 3>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 4>>>::fractional_width == 2);
  static_assert(hh_mm_ss<duration<int, ratio<2, 4>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 5>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<1, 6>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 7>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 8>>>::fractional_width == 3);
  static_assert(hh_mm_ss<duration<int, ratio<1, 9>>>::fractional_width == 6);
  static_assert(hh_mm_ss<duration<int, ratio<1, 10>>>::fractional_width == 1);
  static_assert(hh_mm_ss<duration<int, ratio<756, 625>>>::fractional_width == 4);
  static_assert(hh_mm_ss<duration<int, ratio<1, (1ll << 62)>>>::fractional_width
		== 18);

  constexpr auto hms = hh_mm_ss{-(8h + 9min + 10s + 11ms + 12us + 13ns)};
  static_assert(__is_same(decltype(hms)::precision, nanoseconds));
  static_assert(hms.hours() == 8h);
  static_assert(hms.minutes() == 9min);
  static_assert(hms.seconds() == 10s);
  static_assert(hms.subseconds() == 11ms + 12us + 13ns);
  static_assert(hms.is_negative());
  static_assert(hh_mm_ss{hms.to_duration()}.to_duration() == hms.to_duration());

  static_assert(seconds{hh_mm_ss{100min}} == 100min);

  // treat_as_floating_point_v
  using fseconds = duration<double, ratio<1>>;
  constexpr hh_mm_ss<fseconds> fsec{0x123.0004p5s};
  static_assert(std::is_same_v<hh_mm_ss<fseconds>::precision, fseconds>);
  static_assert(fsec.hours() == 2h);
  static_assert(fsec.minutes() == 35min);
  static_assert(fsec.seconds() == 12s);
  static_assert(fsec.subseconds() == 0x.0004p5s);
  static_assert(!fsec.is_negative());
  static_assert(fsec.to_duration() == 0x123.0004p5s);

  using fminutes = duration<double, ratio<60>>;
  constexpr hh_mm_ss<fminutes> fmin{-0x1.23p4min};
  static_assert(std::is_same_v<hh_mm_ss<fminutes>::precision, fseconds>);
  static_assert(fmin.hours() == 0h);
  static_assert(fmin.minutes() == 18min);
  static_assert(fmin.seconds() == 11s);
  static_assert(fmin.subseconds() == 0.25s);
  static_assert(fmin.is_negative());
  static_assert(fmin.to_duration() == -0x1.23p4min);
}

constexpr void
default_construction()
{
  using namespace std::chrono;

  constexpr hh_mm_ss<seconds> s1;
  static_assert(s1.to_duration() == s1.to_duration().zero());
  constexpr hh_mm_ss<duration<char>> s2;
  static_assert(s2.to_duration() == s2.to_duration().zero());
  constexpr hh_mm_ss<duration<int, std::centi>> s3;
  static_assert(s3.to_duration() == s3.to_duration().zero());
  constexpr hh_mm_ss<duration<long long, std::femto>> s4;
  static_assert(s4.to_duration() == s4.to_duration().zero());
  constexpr hh_mm_ss<duration<double>> s5;
  static_assert(s5.to_duration() == s5.to_duration().zero());
}

constexpr void
size()
{
  using namespace std::chrono;

  struct S0 { long long h; char m; char s; bool neg; struct { } empty; };
  static_assert(sizeof(hh_mm_ss<seconds>) == sizeof(S0));
  struct S1 { long long h; char m; char s; bool neg; char ss; };
  static_assert(sizeof(hh_mm_ss<duration<int, std::centi>>) == sizeof(S1));
  struct S2 { long long h; char m, s; bool neg; int ss; };
  static_assert(sizeof(hh_mm_ss<duration<int, std::milli>>) == sizeof(S2));
  struct S3 { long long h; char m, s; bool neg; long long ss; };
  static_assert(sizeof(hh_mm_ss<duration<int, std::pico>>) == sizeof(S3));
  static_assert(sizeof(hh_mm_ss<duration<long long, std::pico>>) == sizeof(S3));
  struct S4 { long long h; char m, s; bool neg; double ss; };
  static_assert(sizeof(hh_mm_ss<duration<double, std::micro>>) == sizeof(S4));
}

constexpr void
unsigned_rep()
{
  using namespace std::chrono;

  constexpr duration<unsigned, std::milli> ms(3690001);

  constexpr hh_mm_ss hms(ms); // PR libstdc++/108265
  static_assert( ! hms.is_negative() );
  static_assert( hms.to_duration() == milliseconds(ms.count()) );
  static_assert( hms.hours() == 1h );
  static_assert( hms.minutes() == 1min );
  static_assert( hms.seconds() == 30s );
  static_assert( hms.subseconds() == 1ms );
}
