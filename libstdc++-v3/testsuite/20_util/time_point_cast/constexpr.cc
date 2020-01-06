// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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
#include <testsuite_common_types.h>

int main()
{
  using namespace std::chrono;
  typedef time_point<system_clock, hours> 	to_type;
  typedef time_point<system_clock, minutes> 	from_type;

  // constexpr
  constexpr minutes m(6000);
  constexpr hours h(19);
  constexpr to_type tpm(h); // time_point object with minutes
  constexpr from_type tph(m); // time_point object with hours

  constexpr auto res(time_point_cast<hours>(tpm));

  return 0;
}
