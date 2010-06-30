// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009, 2010 Free Software Foundation, Inc.
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

#include <thread>

void test01()
{
  // copy
  typedef std::thread test_type;
  test_type t1;
  test_type t2(t1);		// { dg-error "deleted" "" { xfail *-*-* } }
}

// This is failing for the wrong reason; it should fail because we're
// trying to call the deleted copy constructor, but instead it fails
// because we try to call the thread(_Callable&&,_Args&&...) constructor
// and fail because thread isn't callable.  But that's OK for now.
// { dg-error "" "" { target *-*-* } 30 }

// { dg-prune-output "include" }
