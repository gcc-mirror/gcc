// Copyright (C) 2018-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <numeric>
#include <iterator>
#include <testsuite_hooks.h>

struct Int
{
  Int(int v) : val(v) { }

  ~Int() = default;

  Int(const Int& x) : val(x.val), copies(x.copies), moved_from(x.moved_from)
  { ++copies; }

  Int(Int&& x) : val(x.val), copies(x.copies), moved_from(x.moved_from)
  { x.moved_from = true; }

  Int& operator=(const Int& x)
  {
    val = x.val;
    copies = x.copies + 1;
    moved_from = x.moved_from;
    return *this;
  }

  Int& operator=(Int&& x)
  {
    val = x.val;
    copies = x.copies;
    moved_from = x.moved_from;
    x.moved_from = true;
    return *this;
  }

  int val = 0;
  int copies = 0;
  bool moved_from = false;
};

Int operator-(Int x, Int y) { x.val -= y.val; return x; }

struct Subtract
{
  Int operator()(Int x, Int y) const { x.val -= y.val; return x; }
};

void
test01()
{
  Int i[] = { 0, 1, 2, 3, 4 };
  std::adjacent_difference(std::begin(i), std::end(i), std::begin(i));
  for (const auto& r : i)
  {
    VERIFY( r.copies == 2 );
    VERIFY( !r.moved_from );
  }
}

void
test02()
{
  Int i[] = { 0, 1, 2, 3, 4 };
  std::adjacent_difference(std::begin(i), std::end(i), std::begin(i),
			   Subtract{});
  for (const auto& r : i)
  {
    VERIFY( r.copies == 2 );
    VERIFY( !r.moved_from );
  }
}

void
test03()
{
  Int i[] = { 0, 1, 2, 3, 4 };
  std::adjacent_difference(std::make_move_iterator(std::begin(i)),
			   std::make_move_iterator(std::end(i)),
			   std::begin(i));
  for (const auto& r : i)
  {
    VERIFY( r.copies == 1 );
    VERIFY( !r.moved_from );
  }
}

void
test04()
{
  Int i[] = { 0, 1, 2, 3, 4 };
  std::adjacent_difference(std::make_move_iterator(std::begin(i)),
			   std::make_move_iterator(std::end(i)),
			   std::begin(i), Subtract{});
  for (const auto& r : i)
  {
    VERIFY( r.copies == 1 );
    VERIFY( !r.moved_from );
  }
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
