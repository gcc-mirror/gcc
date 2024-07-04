// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

// { dg-do run { target { *-*-gnu* } } }
// { dg-require-effective-target c++11 }
// { dg-require-gthreads "" }

// N.B. this test intentionally does *not* use -pthread

#include <thread>
#include <testsuite_hooks.h>

void
test01()
{
  VERIFY( std::this_thread::get_id() != std::thread::id() );
}

int
main()
{
  test01();
}
