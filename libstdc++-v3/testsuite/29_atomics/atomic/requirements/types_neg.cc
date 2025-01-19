// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <atomic>

std::atomic<const int> a; // { dg-error "here" }

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) = default;
  MoveOnly& operator=(MoveOnly&&) = default;
};
std::atomic<MoveOnly> b; // { dg-error "here" }

struct NoMove
{
  NoMove() = default;
  NoMove(const NoMove&) = default;
  NoMove& operator=(const NoMove&) = default;
  NoMove(NoMove&&) = delete;
  NoMove& operator=(NoMove&&) = delete;
};
std::atomic<NoMove> c; // { dg-error "here" }

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
