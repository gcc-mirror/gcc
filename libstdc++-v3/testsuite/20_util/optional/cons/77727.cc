// { dg-options "-std=gnu++17" }
// { dg-do run }

// Copyright (C) 2016 Free Software Foundation, Inc.
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

// You should have received a moved_to of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <optional>
#include <testsuite_hooks.h>


struct NonTransferable
{
  int x;
  NonTransferable(int x) : x(x) {}
  NonTransferable(NonTransferable&&) = delete;
  NonTransferable& operator=(NonTransferable&&) = delete;
  operator int() {return x;}
};

int main()
{
  std::optional<int> oi;
  std::optional<NonTransferable> ot(std::move(oi));
  VERIFY(!ot);

  std::optional<int> oi2;
  std::optional<NonTransferable> ot2(oi2);
  VERIFY(!ot);

  std::optional<int> oi3{42};
  std::optional<NonTransferable> ot3(std::move(oi3));
  VERIFY(ot3 && *ot3 == 42);

  std::optional<int> oi4{666};
  std::optional<NonTransferable> ot4(oi4);
  VERIFY(ot4 && *ot4 == 666);
}
