// Copyright (C) 2018-2020 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do run { target c++17 } }

#include <filesystem>
#include <iterator>
#include <testsuite_hooks.h>
#include <testsuite_fs.h>

void
test01()
{
  for (std::filesystem::path p : __gnu_test::test_paths)
  {
    if (p.empty())
      VERIFY(std::distance(p.begin(), p.end()) == 0);
    else
      VERIFY(std::distance(p.begin(), p.end()) != 0);

    for (const std::filesystem::path& cmpt : p)
    {
      if (cmpt.empty())
	VERIFY(std::distance(cmpt.begin(), cmpt.end()) == 0);
      else
	VERIFY(std::distance(cmpt.begin(), cmpt.end()) == 1);
    }
  }
}

int
main()
{
  test01();
}
