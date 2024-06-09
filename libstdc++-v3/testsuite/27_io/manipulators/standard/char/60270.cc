// { dg-do run { target c++14 } }

// Copyright (C) 2014-2024 Free Software Foundation, Inc.
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

// 27.7.6 - Quoted manipulators		[quoted.manip]

// libstdc++/60270

#include <string>
#include <sstream>
#include <iomanip>
#include <testsuite_hooks.h>

int main()
{
  std::istringstream in;
  std::string s = "xxx";
  in >> s;
  VERIFY( !s.empty() );
  in >> std::quoted(s);
  VERIFY( !s.empty() );
}
