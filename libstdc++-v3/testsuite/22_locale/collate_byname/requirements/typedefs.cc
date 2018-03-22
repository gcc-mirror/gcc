// { dg-do compile }
// 2001-08-15  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2018 Free Software Foundation, Inc.
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

// 22.2.4 The collate category

#include <locale>

void test01()
{
  // Check for required typedefs
  typedef std::collate_byname<char> test_type;
  typedef test_type::char_type char_type;
  typedef test_type::string_type string_type;
}
