// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do run { target { lp64 || llp64 } } }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target run_expensive_tests }

#include <functional>
#include <string>

void
test01()
{
  const std::size_t big = std::size_t(1) << 31;
  std::string s;
  try {
    s.resize(big, 'a');
  } catch (const std::bad_alloc&) {
    return; // try to avoid a FAIL if memory allocation fails
  }
  // PR libstdc++/89629
  (void) std::hash<std::string>{}(s);
}

int
main()
{
  test01();
}
