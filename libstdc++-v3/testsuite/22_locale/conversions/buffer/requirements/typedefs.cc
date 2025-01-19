// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2025 Free Software Foundation, Inc.
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

// 22.3.3.2.3  Buffer conversions

#include <locale>
#include <type_traits>

void test01()
{
  // Check for required typedefs
  struct cvt_type : std::codecvt<wchar_t, char, mbstate_t> { };
  typedef std::char_traits<wchar_t> traits_type;
  typedef std::wbuffer_convert<cvt_type, wchar_t, traits_type> test_type; // { dg-warning "deprecated" "" { target c++17 } }
  typedef test_type::state_type state_type;

  static_assert( std::is_same<cvt_type::state_type, state_type>::value,
		 "state type" );
}
