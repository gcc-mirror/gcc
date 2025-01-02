// { dg-options "-Wno-unused-result" }
// { dg-do compile { target c++20 } }

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

#include <array>

void
test01()
{
  int two_dee[3][4];
  std::to_array(two_dee); // { dg-error "here" }
}

void
test02()
{
  struct X
  {
    int two_dee[3][4];
  };
  std::to_array(X{}.two_dee); // { dg-error "here" }
}

void
test03()
{
  struct MoveOnly
  {
    MoveOnly() = default;
    MoveOnly(MoveOnly&&) = default;
  };

  MoveOnly mo[2];
  std::to_array(mo); // { dg-error "here" }

  const MoveOnly cmo[3];
  std::to_array(std::move(cmo)); // { dg-error "here" }
}

// { dg-prune-output "static assertion failed" }
