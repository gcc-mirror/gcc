// { dg-do compile }
// { dg-options "-std=gnu++17" }
// { dg-require-gthreads "" }

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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


#include <shared_mutex>

void test01()
{
  // assign
  typedef std::shared_mutex mutex_type;
  mutex_type m1;
  mutex_type m2;
  m1 = m2;			// { dg-error "deleted" }
}

// { dg-prune-output "include" }
