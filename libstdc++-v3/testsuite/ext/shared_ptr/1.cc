// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>
#include <testsuite_hooks.h>

struct A : std::__enable_shared_from_this<A> { };

void
test01()
{
  std::__shared_ptr<A> sp(new A);
  auto sp2 = sp->shared_from_this();
  VERIFY( (bool)sp2 );
  static_assert( std::is_same<decltype(sp), decltype(sp2)>::value, "" );
}

int
main()
{
  test01();
}
