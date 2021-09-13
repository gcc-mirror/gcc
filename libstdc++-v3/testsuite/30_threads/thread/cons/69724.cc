// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

// { dg-do run }
// { dg-options "-pthread"  }
// { dg-require-effective-target c++11 }
// { dg-require-effective-target pthread }
// { dg-require-gthreads "" }

#include <thread>
#include <testsuite_rvalref.h>

struct F : __gnu_test::copycounter
{
  F() = default;

  F(const F&) = default;

  // Move constructor copies base class, to use counter:
  F(F&& f) : copycounter(f) { f.valid = false; }

  void run() { VERIFY(this->valid); }
};

void
test01()
{
  std::thread{&F::run, F{}}.join();
  VERIFY( F::copycount == 1 );
}

void
test02()
{
  F::copycount = 0;
  const F f;
  std::thread{&F::run, f}.join();
  VERIFY( F::copycount == 1 );
}

void
test03()
{
  F::copycount = 0;
  F f;
  std::thread{&F::run, std::ref(f)}.join();
  VERIFY( F::copycount == 0 );
}

int
main()
{
  test01();
  test02();
  test03();
}
