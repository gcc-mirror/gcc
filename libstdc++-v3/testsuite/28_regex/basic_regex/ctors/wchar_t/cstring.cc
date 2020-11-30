// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }

// 2007-03-12  Stephen M. Webb  <stephen.webb@bregmasoft.com>
//
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

// [28.8.2] class template basic_regex constructor

#include <regex>

// Tests C-style null-terminated-string constructor of the basic_regex class.  
void test01()
{
  typedef std::basic_regex<wchar_t> test_type;

  const wchar_t* cs = L"aab";
  test_type re(cs);
}

int
main()
{ 
  test01();
  return 0;
}
