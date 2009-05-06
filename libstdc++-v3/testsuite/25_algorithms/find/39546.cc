// Copyright (C) 2009 Free Software Foundation, Inc.
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

// 25.3.1 algorithms, find()

#include <vector>
#include <string>
#include <algorithm>
#include <testsuite_hooks.h>

// libstdc++/39546
void
test01()
{
  bool test __attribute__((unused)) = true;

  std::vector<std::string> dict;
  dict.push_back("one");
  dict.push_back("two");
  dict.push_back("three");

  VERIFY( std::find(dict.begin(), dict.end(), "two") == dict.begin() + 1 );
}

int
main()
{
  test01();
}
