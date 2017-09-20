// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <new>

void f1(int) noexcept;
int f2(const char*, ...);

void
test01()
{
  std::launder( &f1 ); // { dg-error "deleted function" }
  std::launder( &f2 ); // { dg-error "deleted function" }
  void* p = nullptr;
  std::launder( p );  // { dg-error "deleted function" }
  const void* cp = nullptr;
  std::launder( cp );  // { dg-error "deleted function" }
  volatile void* vp = nullptr;
  std::launder( vp );  // { dg-error "deleted function" }
  const volatile void* cvp = nullptr;
  std::launder( cvp );  // { dg-error "deleted function" }
}
