// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// Class template day [time.cal.month_day_last]

#include <chrono>

constexpr void
constexpr_month_day_last()
{
  using namespace std::chrono;
  using mdl = month_day_last;

  constexpr auto mdl0 = February / last;
  static_assert(mdl0.month() == February);

  constexpr auto mdl1 = month_day_last{month{3}};
  static_assert(mdl1.month() == month{3});

  static_assert( mdl{month{3}}.ok());
  static_assert(!mdl{month{0}}.ok());
  static_assert(!mdl{month{13}}.ok());

  static_assert( (mdl{month{1}} == mdl{month{1}}));
  static_assert(!(mdl{month{2}} != mdl{month{2}}));
  static_assert(!(mdl{month{3}} <  mdl{month{3}}));
  static_assert(!(mdl{month{4}} >  mdl{month{4}}));
  static_assert( (mdl{month{5}} <= mdl{month{5}}));
  static_assert( (mdl{month{6}} >= mdl{month{6}}));
  static_assert( (mdl{month{10}} == mdl{month{10}}));
  static_assert( (mdl{month{9}} != mdl{month{10}}));
  static_assert( (mdl{month{8}} < mdl{month{10}}));
  static_assert( (mdl{month{11}} > mdl{month{10}}));
  static_assert( (mdl{month{10}} <= mdl{month{10}}));
  static_assert( (mdl{month{10}} >= mdl{month{10}}));

  static_assert( (mdl{month{1}} <=> mdl{month{1}})
		== std::strong_ordering::equal);
  static_assert( (mdl{month{11}} <=> mdl{month{10}})
		== std::strong_ordering::greater);
  static_assert( (mdl{month{8}} <=> mdl{month{10}})
		== std::strong_ordering::less);

  static_assert(August/last == mdl{month{8}});
  static_assert(8/last == mdl{month{8}});
  static_assert(last/August == mdl{month{8}});
  static_assert(last/8 == mdl{month{8}});
}
