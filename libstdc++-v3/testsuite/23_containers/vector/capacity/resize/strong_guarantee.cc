// Copyright (C) 2018-2021 Free Software Foundation, Inc.
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

#include <vector>
#include <testsuite_hooks.h>

struct X
{
  X() : data(1)
  {
    if (fail)
      throw 1;
  }

  static bool fail;

  std::vector<int> data;
};

bool X::fail = false;

void
test01()
{
  std::vector<X> v(2);
  X* const addr = &v[0];
  bool caught = false;
  try {
    X::fail = true;
    v.resize(v.capacity() + 1); // force reallocation
  } catch (int) {
    caught = true;
  }
  VERIFY( caught );
  VERIFY( v.size() == 2 );
  VERIFY( &v[0] == addr );
  // PR libstdc++/83982
  VERIFY( ! v[0].data.empty() );
  VERIFY( ! v[1].data.empty() );
}

int
main()
{
  test01();
}
