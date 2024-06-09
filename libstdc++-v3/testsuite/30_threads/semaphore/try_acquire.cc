// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++20 } }
// { dg-additional-options "-pthread" { target pthread } }
// { dg-require-gthreads "" }
// { dg-add-options libatomic }

#include <semaphore>
#include <limits>
#include <cstddef>
#include <testsuite_hooks.h>

void test01()
{
  std::counting_semaphore<10> s(3);

  s.acquire();
  VERIFY( s.try_acquire() );
  VERIFY( s.try_acquire() );
  VERIFY( !s.try_acquire() );
  s.release();
  VERIFY( s.try_acquire() );
}

void test02()
{
  std::binary_semaphore s(1);

  s.acquire();
  VERIFY( !s.try_acquire() );
  s.release();
  VERIFY( s.try_acquire() );
}


int main()
{
  test01();
  test02();
}
