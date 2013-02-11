// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" } 
// { dg-require-gthreads "" }

// Copyright (C) 2008-2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <mutex>

void test01()
{
  typedef std::mutex mutext_t;
  typedef std::recursive_mutex rmutext_t;

  typedef std::defer_lock_t dl_t;
  typedef std::try_to_lock_t ttl_t;
  typedef std::adopt_lock_t al_t;

  using std::defer_lock;
  using std::try_to_lock;
  using std::adopt_lock;

  typedef std::lock_guard<mutext_t> lock_t;
  typedef std::unique_lock<rmutext_t> ulock_t;

  typedef std::once_flag once_t;
}
