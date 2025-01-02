// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <memory>

// This has a trivial destructor, but should not be destructible!
struct DeletedDtor {
  ~DeletedDtor() = delete;
};

void
test01()
{
  alignas(DeletedDtor) unsigned char buf[sizeof(DeletedDtor)];
  auto p = ::new (buf) DeletedDtor();
  std::destroy(p, p + 1);	// { dg-error "here" }
  std::destroy_n(p, 1);		// { dg-error "here" }
}

class PrivateDtor {
  ~PrivateDtor() { }
};

void
test02()
{
  alignas(PrivateDtor) unsigned char buf[sizeof(PrivateDtor)];
  auto p = ::new (buf) PrivateDtor();
  std::destroy(p, p + 1);	// { dg-error "here" }
  std::destroy_n(p, 1);		// { dg-error "here" }
}

// { dg-error "value type is destructible" "" { target *-*-* } 0 }
