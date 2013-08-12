// { dg-options "-x c -std=gnu++0x -lsupc++ -fvtable-verify=none" }

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

#include <cstdarg>
#include <cstddef>
#include <cstdlib>

#include <exception>
#include <limits>
#include <new>
#include <typeinfo>

#include <initializer_list>
#include <type_traits>

int main()
{
  std::exception e;

  const char* str __attribute__((unused)) = typeid(e).name();
  
  typedef std::numeric_limits<long> limit_type;
  limit_type limit_l __attribute__((unused));
  int r __attribute__((unused)) = limit_type::radix;

  const char* cp = new char;
  delete cp;

  bool b __attribute__((unused)) = std::is_integral<int>::value;

  std::initializer_list<int> ilisti __attribute__((unused));

  return 0;
}
