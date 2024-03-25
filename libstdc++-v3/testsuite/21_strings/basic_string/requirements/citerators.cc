// { dg-do run { target c++11 } }
// { dg-require-string-conversions "" }

// Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

#include <string>
#include <debug/string>

#include <testsuite_containers.h>

int main()
{
  __gnu_test::citerator<std::string> test1;
  __gnu_test::citerator<__gnu_debug::string> dtest1;
  __gnu_test::citerator<std::wstring> test2;
  __gnu_test::citerator<__gnu_debug::wstring> dtest2;
  return 0;
}
