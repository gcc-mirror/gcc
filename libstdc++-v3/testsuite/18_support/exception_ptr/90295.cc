// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// { dg-options "-O1 -g0" }
// { dg-final { scan-assembler-not "St15__exception_ptr13exception_ptr" } }

#include <exception>

void
test01()
{
  // PR libstdc++/90295
  // Operations on null exception_ptr objects should be optimised away.

  std::exception_ptr p1;
  if (!(p1 == nullptr))
    std::rethrow_exception(p1);

  std::exception_ptr p2 = p1;
  if (!(p2 == p1))
    std::rethrow_exception(p2);

  p1 = p2;
  if (p1 != p2)
    std::rethrow_exception(p1);

  swap(p1, p2);
  if (nullptr != p1)
    std::rethrow_exception(p1);

  p1 = std::exception_ptr(nullptr);
  if (!(p1 == p2))
    std::rethrow_exception(p1);
}
