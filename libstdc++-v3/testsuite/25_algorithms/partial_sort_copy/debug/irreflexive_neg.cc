// Copyright (C) 2015-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.
//
// { dg-do run { xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <algorithm>

bool
bad_lower(int lhs, int rhs)
{
  if (lhs == 0)
    return true;

  return lhs < rhs;
}

void test01()
{
  int ins[] = { 0, 1, 2, 3 };
  int outs[] = { 9, 9 };
  std::partial_sort_copy(ins, ins + 4, outs, outs + 2, bad_lower);
}

int main()
{
  test01();
  return 0;
}
