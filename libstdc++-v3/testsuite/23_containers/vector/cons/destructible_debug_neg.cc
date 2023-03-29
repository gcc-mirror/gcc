// Copyright (C) 2017-2023 Free Software Foundation, Inc.
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

// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++11 } }
// { dg-skip-if "" { *-*-* } { "-D_GLIBCXX_PARALLEL" } }

#include <vector>

// PR libstdc++/80553

struct DeletedDtor {
  ~DeletedDtor() = delete;
};

class PrivateDtor {
  ~PrivateDtor() { }
};

void
test01()
{
  std::vector<DeletedDtor> v;
}

void
test02()
{
  std::vector<PrivateDtor> v;
}

// { dg-error "value type is destructible" "" { target *-*-* } 0 }

// In Debug Mode the "required from here" errors come from <debug/vector>
// { dg-error "required from here" "" { target *-*-* } 172 }

// Needed because of PR c++/92193
// { dg-prune-output "deleted function" }
// { dg-prune-output "private within this context" }
