// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

// { dg-add-options libatomic }
// { dg-do run { target c++20 } }

#include <stop_token>
#include <testsuite_hooks.h>

void
test01()
{
  std::stop_source src1, src2;
  const std::stop_source orig1(src1);
  VERIFY( src1 != src2 );
  src1 = src2;
  VERIFY( src1 == src2 );
  VERIFY( src1 != orig1 );
}

void
test02()
{
  std::stop_source src1, src2;
  const std::stop_source orig1(src1), orig2(src2), src0(std::nostopstate);
  src1 = std::move(src2);
  VERIFY( src1 == orig2 );
  VERIFY( src2 == src0 );
  VERIFY( src1 != orig1 );
  VERIFY( src0 != orig1 );
}

int main()
{
  test01();
  test02();
}
