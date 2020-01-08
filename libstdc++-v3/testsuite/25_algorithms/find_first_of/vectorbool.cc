// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

// 25.1.4 [lib.alg.find.first.of]

#include <algorithm>
#include <vector>
#include <testsuite_hooks.h>

void
test1()
{
  std::vector<bool> v1;
  v1.push_back(false);
  v1.push_back(false);
  v1.push_back(true);
  v1.push_back(false);
  v1.push_back(true);
  v1.push_back(true);
  v1.push_back(false);
  v1.push_back(true);
  v1.push_back(true);

  std::vector<bool> v2;
  v2.push_back(true);
  v2.push_back(false);

  VERIFY( std::find_first_of(v1.begin(), v1.end(), v2.begin(), v2.end())
	  == v1.begin() );
}

int 
main()
{
  test1();
}
