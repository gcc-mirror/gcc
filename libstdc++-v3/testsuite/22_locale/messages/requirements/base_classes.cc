// { dg-do compile }
// 2001-07-17  Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

// 22.2.7 The message retrieval category

#include <locale>

void test01()
{
  // Check for required base class.
  typedef std::messages<char> test_type;
  typedef std::locale::facet base_type1;
  typedef std::messages_base base_type2;
  const test_type& obj = std::use_facet<test_type>(std::locale()); 
  const base_type1* base1 __attribute__((unused)) = &obj;
  const base_type2* base2 __attribute__((unused)) = &obj;
}
