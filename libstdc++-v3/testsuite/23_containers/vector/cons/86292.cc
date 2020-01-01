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

// { dg-do run }

#include <vector>
#include <testsuite_hooks.h>
#include <testsuite_iterators.h>

struct X
{
  X() { ++count; }
  X(const X&) { if (++copies >= max_copies) throw 1; ++count; }
  ~X() { --count; }

  static int count;
  static int copies;
  static int max_copies;
};

int X::count = 0;
int X::copies = 0;
int X::max_copies = 0;

void
test01()
{
  X x[3];
  const int count = X::count;
  X::max_copies = 2;
  __gnu_test::test_container<const X, __gnu_test::input_iterator_wrapper>
    x_input(x, x+3);
  bool caught = false;
  try
  {
    std::vector<X> v(x_input.begin(), x_input.end());
  }
  catch(int)
  {
    caught = true;
  }
  VERIFY( caught );
  VERIFY( X::count == count );
}

int
main()
{
  test01();
}
