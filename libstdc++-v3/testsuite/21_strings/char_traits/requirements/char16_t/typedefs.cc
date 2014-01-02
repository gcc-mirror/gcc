// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }

// Copyright (C) 2008-2014 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <string>
#include <cstdint>

int main()
{
  // Check for required typedefs.
  typedef std::char_traits<char16_t> test_type;
  typedef test_type::char_type char_type;
  typedef test_type::int_type int_type;
  typedef test_type::off_type off_type;
  typedef test_type::pos_type pos_type;
  typedef test_type::state_type state_type;

  // char_traits<char16_t>::int_type == uint_least16_t
  test_type::int_type* p = 0;
  std::uint_least16_t* q __attribute__((unused)) = p;                   

  return 0;
}
