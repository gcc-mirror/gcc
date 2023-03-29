// { dg-do run { target c++11 } }
// { dg-require-cstdint "" }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

void test01()
{
  using namespace std::chrono;

  typedef duration<std::int64_t, std::ratio<36 * 24 * 36525>> Years;
  Years galactic_empire_age( 12067 );

  VERIFY( duration_cast<seconds>( galactic_empire_age ).count()
	  == duration_cast<minutes>( galactic_empire_age ).count() * 60 );
  VERIFY( duration_cast<minutes>( galactic_empire_age ).count()
	  == duration_cast<seconds>( galactic_empire_age ).count() / 60 );
}

int main()
{
  test01();
  return 0;
}
