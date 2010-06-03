// 1999-06-04 bkoz

// Copyright (C) 1999, 2000, 2001, 2002, 2003, 2009, 2010
// Free Software Foundation, Inc.
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

// 21.3.1 basic_string constructors.

#include <string>
#include <vector>
#include <testsuite_hooks.h>

// libstdc++/8347
void test05()
{
  bool test __attribute__((unused)) = true;

  std::vector<char> empty;
  std::string empty2(empty.begin(), empty.end());

  // libstdc++/8716 (same underlying situation, same fix)
  char const * s = 0;
  std::string zero_length_built_with_NULL(s,0);
}

int main()
{ 
  test05();
  return 0;
}
