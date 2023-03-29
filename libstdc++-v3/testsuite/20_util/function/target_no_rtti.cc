// { dg-options "-fno-rtti" }
// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2020-2023 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

using std::function;

long f() { return 1; }
struct F { long operator()() {  return 2; } };

void test01()
{
  std::function<int()> fun = f;
  long (**tgt1)() = fun.target<long(*)()>();
  VERIFY( *tgt1 == f );
  VERIFY( (*tgt1)() == 1L );
  VERIFY( fun.target<long(*)()>() == tgt1 );
  VERIFY( fun.target<long(* const)()>() == tgt1 );
  VERIFY( fun.target<F>() == nullptr );
  VERIFY( fun.target<void>() == nullptr );
  VERIFY( fun.target<int()>() == nullptr );
  VERIFY( fun.target<long()>() == nullptr );

  const F ff;
  fun = ff;
  F* tgt2 = fun.target<F>();
  VERIFY( tgt2 != nullptr );
  VERIFY( (*tgt2)() == 2L );
  VERIFY( fun.target<const F>() == tgt2 );
  VERIFY( fun.target<int(*)()>() == nullptr );
  VERIFY( fun.target<void()>() == nullptr );
  VERIFY( fun.target<void(*)()>() == nullptr );
}

int main()
{
  test01();
}
