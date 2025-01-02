// { dg-do compile }
// 2001-02-11 gdr
// Origin: Craig Rodrigues <rodrigc@mediaone.net>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// 21.1.2: char_traits typedefs

#include <string>

int main()
{
  // Check for required typedefs.
  typedef std::char_traits<wchar_t> test_type;
  typedef test_type::char_type char_type;
  typedef test_type::int_type int_type;
  typedef test_type::off_type off_type;
  typedef test_type::pos_type pos_type;
  typedef test_type::state_type state_type;

  // 21.1.3: char_traits<wchar_t>::int_type == wint_t
  test_type::int_type* p = 0;
  wint_t* q __attribute__((unused)) = p;                   

  return 0;
}
